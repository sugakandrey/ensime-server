// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core.debug

import com.sun.jdi.VMDisconnectedException
import org.scaladebugger.api.debuggers.{ AttachingDebugger, Debugger, LaunchingDebugger }
import org.scaladebugger.api.profiles.scala210.Scala210DebugProfile
import org.scaladebugger.api.virtualmachines.{ DummyScalaVirtualMachine, ScalaVirtualMachine }
import org.slf4j.LoggerFactory

import scala.util.{ Failure, Try }

/**
 * Represents a manager of virtual machines connected to Ensime.
 *
 * @param globalStartFunc Invoked whenever a virtual machine is started
 * @param globalStopFunc Invoked whenever a virtual machine is stopped or disconnects
 *                 unexpectedly
 */
class VirtualMachineManager(
    private val globalStartFunc: (ScalaVirtualMachine) => Unit = _ => {},
    private val globalStopFunc: (ScalaVirtualMachine) => Unit = _ => {}
) {
  private val log = LoggerFactory.getLogger(this.getClass)

  /** Create a standard dummy vm using the Scala 2.10 profile */
  private lazy val dummyScalaVirtualMachine: ScalaVirtualMachine = {
    val d = DummyScalaVirtualMachine.newInstance()
    d.use(Scala210DebugProfile.Name)
    d
  }

  @volatile private var debugger: Option[Debugger] = None
  @volatile private var vm: Option[ScalaVirtualMachine] = None
  @volatile private var mode: Option[VmMode] = None

  /**
   * Represents the mode actively being used by the internal debugger.
   *
   * @return Some mode instance if active, otherwise None
   */
  def activeMode: Option[VmMode] = mode

  /**
   * Starts a new debugger using the specified VM mode.
   *
   * @param mode The mode (launching/attaching) to use when determining which
   *             kind of debugger to start
   * @param options Optional JVM options to provide to the JVM started when
   *                using a launching debugger
   * @param startFunc Optional function to be invoked once the debugger has
   *                  connected with the virtual machine
   */
  def start(
    mode: VmMode,
    options: Seq[String] = Nil,
    startFunc: (ScalaVirtualMachine) => Unit = _ => {}
  ): Unit = synchronized {
    import scala.concurrent.duration._

    // Start the specific debugger based on mode and retrieve a
    // ScalaVirtualMachine instance from launching or attaching
    val (d, s) = mode match {
      case VmStart(commandLine) =>
        val d = LaunchingDebugger(
          className = commandLine,
          jvmOptions = options,
          suspend = true
        ).withPending(dummyScalaVirtualMachine)

        val s = d.start(
          timeout = 10.seconds,
          defaultProfile = Scala210DebugProfile.Name,
          startProcessingEvents = false
        )

        (d, s)

      case VmAttach(hostname, port) =>
        val d = AttachingDebugger(
          port = port.toInt,
          hostname = hostname
        ).withPending(dummyScalaVirtualMachine)

        val s = d.start(
          timeout = 10.seconds,
          defaultProfile = Scala210DebugProfile.Name,
          startProcessingEvents = false
        )

        (d, s)
    }

    // Bind our event handlers like class prepare and thread start/stop
    bindEvents(s)

    // Mark our debugger and acquired virtual machine as ready
    update(d, s, mode)

    // Invoke our start function, ignoring any error that may arise
    withVM(globalStartFunc)
    withVM(startFunc)

    // Begin processing events
    s.startProcessingEvents()

    (d, s)
  }

  /**
   * Sets the active debugger, virtual machine, and mode.
   *
   * @param debugger The new debugger managing virtual machines
   * @param scalaVirtualMachine The remote, connected virtual machine
   * @param vmMode The mode associated with the debugger
   */
  private def update(
    debugger: Debugger,
    scalaVirtualMachine: ScalaVirtualMachine,
    vmMode: VmMode
  ) = synchronized {
    this.debugger = Some(debugger)
    vm = Some(scalaVirtualMachine)
    mode = Some(vmMode)
  }

  /**
   * Stops and shuts down the currently-active debugger and its associated
   * virtual machines.
   *
   * @param stopFunc Optional function to call before the virtual machine
   *                 shuts down
   */
  def stop(stopFunc: (ScalaVirtualMachine) => Unit = _ => {}): Unit = synchronized {
    // Invoke our stop function, ignoring any error that may arise
    if (hasActiveVM) {
      withVM(globalStopFunc)
      withVM(stopFunc)
    }

    clear()
  }

  /** Clears the active mode, vm, and debugger. */
  private def clear(): Unit = synchronized {
    // Clear our associated mode
    mode = None

    // Dispose of the virtual machine and discard the reference
    Try(vm.foreach(_.underlyingVirtualMachine.dispose()))
    vm = None

    // Shutdown the associated debugger
    Try(debugger.foreach(_.stop()))
    debugger = None
  }

  /**
   * Returns whether or not this manager has an active JVM that is being
   * debugged.
   *
   * @return True if a remote virtual machine is connected, otherwise false
   */
  def hasActiveVM: Boolean = vm.nonEmpty

  /**
   * Retrieves the active JVM if available and runs the specified action on
   * top of it.
   *
   * @param action The action to evaluate on top of the JVM
   * @tparam T The expected return value from the action
   * @return Some containing the result if successful, otherwise None
   */
  def withVM[T](action: (ScalaVirtualMachine => T)): Try[T] = synchronized {
    if (!hasActiveVM) {
      val error = new IllegalStateException("No VM active for debugging!")
      log.error("Unable to execute action with VM", error)
      Failure(error)
    } else {
      log.trace("Applying action to remote vm")
      val result = Try(action(vm.get))

      result.failed.foreach {
        case e: VMDisconnectedException =>
          log.error("Attempted interaction with disconnected VM:", e)
          clear()
        case e: Throwable =>
          log.error("Exception thrown whilst handling vm action", e)
      }

      result
    }
  }

  /**
   * Retrieves the dummy JVM and runs the specified action on top of it.
   *
   * @param action The action to evaluate on top of the dummy JVM
   * @tparam T The expected return value from the action
   * @return Some containing the result if successful, otherwise None
   */
  def withDummyVM[T](action: (ScalaVirtualMachine => T)): Try[T] = synchronized {
    log.trace("Applying action to dummy vm")
    val result = Try(action(dummyScalaVirtualMachine))

    result.failed.foreach(e =>
      log.error("Exception thrown whilst handling dummy vm action", e))

    result
  }

  /**
   * Attaches common event handlers to the virtual machine.
   *
   * @param s The Scala virtual machine with which to attach event handlers
   */
  private def bindEvents(s: ScalaVirtualMachine): Unit = {
    import org.scaladebugger.api.dsl.Implicits._

    // If our VM disconnects/dies, stop the debugger
    // NOTE: This is not wise if we are using a Listening debugger which can
    //       have more than one JVM connected at once
    s.onUnsafeVMDisconnect().foreach(_ => stop())
    s.onUnsafeVMDeath().foreach(_ => stop())
  }
}
