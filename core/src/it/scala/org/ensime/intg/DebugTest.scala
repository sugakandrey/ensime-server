// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.intg

import akka.event.slf4j.SLF4JLogging
import java.io.File

import akka.testkit._
import java.io.InputStream
import java.util.Scanner
import org.ensime.api._
import org.ensime.core._
import org.ensime.fixture._
import org.ensime.util._
import org.ensime.util.file._
import org.scalatest.Matchers
import scala.concurrent.{ Await, Future, Promise }
import scala.util.{ Properties, Try }
import scala.concurrent.duration._

// must be refreshing as the tests don't clean up after themselves properly
class DebugTest extends EnsimeSpec
    with IsolatedEnsimeConfigFixture
    with IsolatedTestKitFixture
    with IsolatedProjectFixture
    with DebugTestUtils {

  val original = EnsimeConfigFixture.DebugTestProject.copy(
    javaLibs = Nil // no need to index the JRE
  )

  "Debug - stepping" should "be able to step over/in/out" taggedAs Debugger in withEnsimeConfig { implicit config =>
    withTestKit { implicit testkit =>
      withProject { (project, asyncHelper) =>
        implicit val p = (project, asyncHelper)

        // Start on line 8, which is first line in main method
        withDebugSession(
          "stepping.BasicStepping",
          "stepping/BasicStepping.scala",
          8
        ) { (threadId, breakpointsFile) =>
            import testkit._

            // Should be able to step over a method call
            project ! DebugNextReq(threadId)
            expectMsg(remaining, "Failed to step over line!", TrueResponse)

            // Should be able to step into a method call
            project ! DebugStepReq(threadId)
            expectMsg(remaining, "Failed to step into method call!", TrueResponse)

            // Should be able to step out of a method call
            project ! DebugStepOutReq(threadId)
            expectMsg(remaining, "Failed to step out of method call!", TrueResponse)
          }
      }
    }
  }

  "Breakpoints" should "trigger/continue" taggedAs Debugger in withEnsimeConfig { implicit config =>
    withTestKit { implicit testkit =>
      withProject { (project, asyncHelper) =>
        implicit val p = (project, asyncHelper)
        withDebugSession(
          "breakpoints.Breakpoints",
          "breakpoints/Breakpoints.scala",
          32
        ) { (threadId, breakpointsFile) =>
            import testkit._

            // NOTE: Can encounter scala/Predef.scala if picking stack trace
            //       at arbitrary point
            project ! DebugBacktraceReq(threadId, 0, 3)
            expectMsgType[DebugBacktrace] should matchPattern {
              case DebugBacktrace(List(
                DebugStackFrame(0, Nil, 0, "breakpoints.Breakpoints", "mainTest",
                  LineSourcePosition(`breakpointsFile`, 32), _),
                DebugStackFrame(1, List(
                  DebugStackLocal(0, "args", "Array(length = 0)[<EMPTY>]", "java.lang.String[]")
                  ), 1, "breakpoints.Breakpoints$", "main",
                  LineSourcePosition(`breakpointsFile`, 42), _),
                DebugStackFrame(2, Nil, 1, "breakpoints.Breakpoints", "main",
                  LineSourcePosition(`breakpointsFile`, _), _)
                ), `threadId`, "main") =>
            }

            project ! DebugSetBreakReq(breakpointsFile, 11)
            expectMsg(TrueResponse)

            project ! DebugSetBreakReq(breakpointsFile, 13)
            expectMsg(TrueResponse)

            project ! DebugContinueReq(threadId)
            expectMsg(TrueResponse)

            asyncHelper.expectMsg(DebugBreakEvent(threadId, "main", breakpointsFile, 11))

            project ! DebugContinueReq(threadId)
            expectMsg(TrueResponse)

            asyncHelper.expectMsg(DebugBreakEvent(threadId, "main", breakpointsFile, 13))

            project ! DebugClearBreakReq(breakpointsFile, 11)
            expectMsg(TrueResponse)

            project ! DebugContinueReq(threadId)
            expectMsg(TrueResponse)

            asyncHelper.expectMsg(DebugBreakEvent(threadId, "main", breakpointsFile, 13))

            project ! DebugSetBreakReq(breakpointsFile, 11)
            expectMsg(TrueResponse)

            project ! DebugClearBreakReq(breakpointsFile, 13)
            expectMsg(TrueResponse)

            project ! DebugContinueReq(threadId)
            expectMsg(TrueResponse)

            asyncHelper.expectMsg(DebugBreakEvent(threadId, "main", breakpointsFile, 11))

            project ! DebugContinueReq(threadId)
            expectMsg(TrueResponse)

            asyncHelper.expectMsg(DebugBreakEvent(threadId, "main", breakpointsFile, 11))

            project ! DebugContinueReq(threadId)
            expectMsg(TrueResponse)
          }
      }
    }
  }

  it should "list/clear" taggedAs Debugger in withEnsimeConfig { implicit config =>
    withTestKit { implicit testkit =>
      withProject { (project, asyncHelper) =>
        implicit val p = (project, asyncHelper)
        withDebugSession(
          "breakpoints.Breakpoints",
          "breakpoints/Breakpoints.scala",
          32
        ) {
            case (threadId, breakpointsFile) =>
              import testkit._

              project ! DebugListBreakpointsReq
              expectMsgType[BreakpointList] should matchPattern {
                case BreakpointList(Nil, Nil) =>
              }

              // break in main
              project ! DebugSetBreakReq(breakpointsFile, 11)
              expectMsg(TrueResponse)
              project ! DebugSetBreakReq(breakpointsFile, 13)
              expectMsg(TrueResponse)

              // breakpoints should now be active
              project ! DebugListBreakpointsReq
              inside(expectMsgType[BreakpointList]) {
                case BreakpointList(activeBreakpoints, pendingBreakpoints) =>
                  activeBreakpoints should contain theSameElementsAs Set(
                    Breakpoint(breakpointsFile, 11), Breakpoint(breakpointsFile, 13)
                  )
                  pendingBreakpoints shouldBe empty
              }

              // check clear works again
              project ! DebugClearAllBreaksReq
              expectMsg(TrueResponse)
              project ! DebugListBreakpointsReq
              expectMsgType[BreakpointList] should matchPattern {
                case BreakpointList(Nil, Nil) =>
              }
          }
      }
    }
  }

  "Debug variables" should "inspect variables" taggedAs Debugger in withEnsimeConfig { implicit config =>
    withTestKit { implicit testkit =>
      withProject { (project, asyncHelper) =>
        implicit val p = (project, asyncHelper)
        withDebugSession(
          "variables.ReadVariables",
          "variables/ReadVariables.scala",
          21
        ) { (threadId, variablesFile) =>
            // boolean local
            getVariableValue(threadId, "a") should matchPattern {
              case DebugPrimitiveValue("true", "boolean") =>
            }

            // char local
            getVariableValue(threadId, "b") should matchPattern {
              case DebugPrimitiveValue("'c'", "char") =>
            }

            // short local
            getVariableValue(threadId, "c") should matchPattern {
              case DebugPrimitiveValue("3", "short") =>
            }

            // int local
            getVariableValue(threadId, "d") should matchPattern {
              case DebugPrimitiveValue("4", "int") =>
            }

            // long local
            getVariableValue(threadId, "e") should matchPattern {
              case DebugPrimitiveValue("5", "long") =>
            }

            // float local
            getVariableValue(threadId, "f") should matchPattern {
              case DebugPrimitiveValue("1.0", "float") =>
            }

            // double local
            getVariableValue(threadId, "g") should matchPattern {
              case DebugPrimitiveValue("2.0", "double") =>
            }

            // String local
            inside(getVariableValue(threadId, "h")) {
              case DebugStringInstance("\"test\"", debugFields, "java.lang.String", _) =>
                exactly(1, debugFields) should matchPattern {
                  case DebugClassField(_, "value", "char[]", "Array(length = 4)['t','e','s',...]") =>
                }
            }

            // primitive array local
            getVariableValue(threadId, "i") should matchPattern {
              case DebugArrayInstance(3, "int[]", "int", _) =>
            }

            // type local
            inside(getVariableValue(threadId, "j")) {
              case DebugObjectInstance(summary, debugFields, "scala.collection.immutable.$colon$colon", _) =>
                summary should startWith("Instance of scala.collection.immutable.$colon$colon")
                exactly(1, debugFields) should matchPattern {
                  case DebugClassField(_, head, "java.lang.Object", summary) if (
                    (head == "head" || head == "hd") &&
                    summary.startsWith("Instance of java.lang.Integer")
                  ) =>
                }
            }

            // object array local
            getVariableValue(threadId, "k") should matchPattern {
              case DebugArrayInstance(3, "java.lang.Object[]", "java.lang.Object", _) =>
            }
          }
      }
    }
  }

  they should "be able to convert variables to string representations" taggedAs Debugger in withEnsimeConfig { implicit config =>
    withTestKit { implicit testkit =>
      withProject { (project, asyncHelper) =>
        implicit val p = (project, asyncHelper)
        withDebugSession(
          "variables.ReadVariables",
          "variables/ReadVariables.scala",
          21
        ) { (threadId, variablesFile) =>
            // boolean local
            getVariableAsString(threadId, "a").text should be("true")

            // char local
            getVariableAsString(threadId, "b").text should be("'c'")

            // short local
            getVariableAsString(threadId, "c").text should be("3")

            // int local
            getVariableAsString(threadId, "d").text should be("4")

            // long local
            getVariableAsString(threadId, "e").text should be("5")

            // float local
            getVariableAsString(threadId, "f").text should be("1.0")

            // double local
            getVariableAsString(threadId, "g").text should be("2.0")

            // String local
            getVariableAsString(threadId, "h").text should be("\"test\"")

            // primitive array local
            getVariableAsString(threadId, "i").text should be("Array(length = 3)[1,2,3]")

            // type local
            getVariableAsString(threadId, "j").text should
              startWith("Instance of scala.collection.immutable.$colon$colon")

            // object array local
            val objArrayText = getVariableAsString(threadId, "k").text
            objArrayText should startWith("Array(length = 3)")
            objArrayText should include("Instance of variables.One")
            objArrayText should include("Instance of java.lang.Boolean")
            objArrayText should include("Instance of java.lang.Integer")
          }
      }
    }
  }

  they should "be retrievable for the BugFromGitter scenario" taggedAs Debugger in withEnsimeConfig { implicit config =>
    withTestKit { implicit testkit =>
      withProject { (project, asyncHelper) =>
        implicit val p = (project, asyncHelper)
        withDebugSession(
          "variables.BugFromGitter",
          "variables/BugFromGitter.scala",
          31
        ) { (threadId, variablesFile) =>
            getVariableAsString(threadId, "actualTimes").text should be("10")

            getVariableAsString(threadId, "name").text should be("\"Rory\"")

            getVariableAsString(threadId, "times").text should be("5")

            getVariableAsString(threadId, "arrayTest").text should be("Array(length = 4)[1,2,3,...]")

            getVariableAsString(threadId, "listTest").text should
              startWith("Instance of scala.collection.immutable.$colon$colon")

            getVariableAsString(threadId, "person").text should
              startWith("Instance of variables.Person")
          }
      }
    }
  }

  they should "set variable values" taggedAs Debugger in withEnsimeConfig { implicit config =>
    withTestKit { implicit testkit =>
      withProject { (project, asyncHelper) =>
        implicit val p = (project, asyncHelper)
        withDebugSession(
          "variables.WriteVariables",
          "variables/WriteVariables.scala",
          21
        ) { (threadId, variablesFile) =>
            import testkit._

            /* boolean local */ {
              val n = "a"

              project ! DebugLocateNameReq(threadId, n)
              val location = testkit.expectMsgType[DebugStackSlot]

              project ! DebugSetValueReq(location, "false")
              expectMsg(remaining, s"Unable to set boolean for '$n'!", TrueResponse)

              getVariableValue(threadId, n) should matchPattern {
                case DebugPrimitiveValue("false", "boolean") =>
              }
            }

            /* char local */ {
              val n = "b"

              project ! DebugLocateNameReq(threadId, n)
              val location = testkit.expectMsgType[DebugStackSlot]

              project ! DebugSetValueReq(location, "a")
              expectMsg(remaining, s"Unable to set char for '$n'!", TrueResponse)

              getVariableValue(threadId, n) should matchPattern {
                case DebugPrimitiveValue("'a'", "char") =>
              }
            }

            /* short local */ {
              val n = "c"

              project ! DebugLocateNameReq(threadId, n)
              val location = testkit.expectMsgType[DebugStackSlot]

              project ! DebugSetValueReq(location, "99")
              expectMsg(remaining, s"Unable to set short for '$n'!", TrueResponse)

              getVariableValue(threadId, n) should matchPattern {
                case DebugPrimitiveValue("99", "short") =>
              }
            }

            /* int local */ {
              val n = "d"

              project ! DebugLocateNameReq(threadId, n)
              val location = testkit.expectMsgType[DebugStackSlot]

              project ! DebugSetValueReq(location, "99")
              expectMsg(remaining, s"Unable to set int for '$n'!", TrueResponse)

              getVariableValue(threadId, n) should matchPattern {
                case DebugPrimitiveValue("99", "int") =>
              }
            }

            /* long local */ {
              val n = "e"

              project ! DebugLocateNameReq(threadId, n)
              val location = testkit.expectMsgType[DebugStackSlot]

              project ! DebugSetValueReq(location, "99")
              expectMsg(remaining, s"Unable to set long for '$n'!", TrueResponse)

              getVariableValue(threadId, n) should matchPattern {
                case DebugPrimitiveValue("99", "long") =>
              }
            }

            /* float local */ {
              val n = "f"

              project ! DebugLocateNameReq(threadId, n)
              val location = testkit.expectMsgType[DebugStackSlot]

              project ! DebugSetValueReq(location, "99.5")
              expectMsg(remaining, s"Unable to set float for '$n'!", TrueResponse)

              getVariableValue(threadId, n) should matchPattern {
                case DebugPrimitiveValue("99.5", "float") =>
              }
            }

            /* double local */ {
              val n = "g"

              project ! DebugLocateNameReq(threadId, n)
              val location = testkit.expectMsgType[DebugStackSlot]

              project ! DebugSetValueReq(location, "99.5")
              expectMsg(remaining, s"Unable to set double for '$n'!", TrueResponse)

              getVariableValue(threadId, n) should matchPattern {
                case DebugPrimitiveValue("99.5", "double") =>
              }
            }

            /* string local */ {
              val n = "h"

              project ! DebugLocateNameReq(threadId, n)
              val location = testkit.expectMsgType[DebugStackSlot]

              project ! DebugSetValueReq(location, "\"fish\"")
              expectMsg(remaining, s"Unable to set string for '$n'!", TrueResponse)

              getVariableValue(threadId, n) should matchPattern {
                case DebugStringInstance("\"fish\"", _, "java.lang.String", _) =>
              }
            }
          }
      }
    }
  }

  "Debug backtrace" should "generate backtrace" taggedAs Debugger in withEnsimeConfig { implicit config =>
    withTestKit { implicit testkit =>
      withProject { (project, asyncHelper) =>
        implicit val p = (project, asyncHelper)
        withDebugSession(
          "debug.Backtrace",
          "debug/Backtrace.scala",
          13
        ) { (threadId, f) =>
            import testkit._
            project ! DebugBacktraceReq(threadId, 0, 20)
            val backTrace = expectMsgType[DebugBacktrace]
            // just some sanity assertions
            assert(backTrace.frames.forall(_.className.startsWith("debug.Backtrace")))
            assert(backTrace.frames.forall(_.pcLocation.file.toString.endsWith("Backtrace.scala")))
          }
      }
    }
  }
}

trait DebugTestUtils {
  this: ProjectFixture with Matchers with EnsimeConfigFixture =>

  /**
   * Launches a new JVM using the given class name as the entrypoint. Pauses
   * the JVM at the specified source path and line.
   *
   * @param className containing the main method
   * @param fileName to place the breakpoint
   * @param breakLine where to start the session in the fileName
   * @param func The test code to execute (gives the debug thread id of the main thread and file handle)
   */
  def withDebugSession(
    className: String,
    fileName: String,
    breakLine: Int
  )(
    func: (DebugThreadId, File) => Any
  )(
    implicit
    config: EnsimeConfig,
    testkit: TestKitFix,
    // don't take an implicit TestActorRef or it steals the implicit sender
    p: (TestActorRef[Project], TestProbe)
  ): Any = {
    import testkit._
    val resolvedFile = scalaMain(config) / fileName
    val project = p._1
    val asyncHelper = p._2

    project ! DebugSetBreakReq(resolvedFile, breakLine)
    expectMsg(TrueResponse)

    val vm = VMStarter(config, className)

    try {
      Await.result(vm._4, (5 seconds).dilated)

      project ! DebugAttachReq(vm._2, vm._3.toString)

      expectMsg(DebugVmSuccess())

      asyncHelper.expectMsg(DebugVMStartEvent)

      val gotOnStartup = asyncHelper.expectMsgType[EnsimeServerMessage]
      // weird! we sometimes see a duplicate break event instantly, not really expected
      val additionalOnStartup = Try(asyncHelper.expectMsgType[EnsimeServerMessage](1 second)).toOption.toSeq
      // but it doesn't always come through

      val allEvents = gotOnStartup +: additionalOnStartup
      val threadId = allEvents.flatMap {
        case DebugBreakEvent(foundThreadId, "main", `resolvedFile`, `breakLine`) =>
          List(foundThreadId)
        case _ =>
          Nil
      }.headOption.getOrElse(fail("Cannot work out main threadId"))

      project ! DebugClearBreakReq(resolvedFile, breakLine)
      expectMsg(TrueResponse)

      func(threadId, resolvedFile)
    } finally {
      // Attempt graceful shutdown (disposes of JVM, clearing all requests
      // to let it finish naturally)
      project ! DebugStopReq

      // If shutdown fails, attempt to forcefully kill the process
      Try(expectMsgPF() { case TrueResponse => })
        .failed.foreach(_ => vm._1.destroy())
    }
  }

  def getVariableValue(
    threadId: DebugThreadId,
    variableName: String
  )(implicit
    testkit: TestKitFix,
    p: (TestActorRef[Project], TestProbe)): DebugValue = {
    import testkit._
    val project = p._1
    project ! DebugLocateNameReq(threadId, variableName)
    val vLoc = expectMsgType[DebugLocation]

    project ! DebugValueReq(vLoc)
    expectMsgType[DebugValue]
  }

  def getVariableAsString(
    threadId: DebugThreadId,
    variableName: String
  )(implicit
    testkit: TestKitFix,
    p: (TestActorRef[Project], TestProbe)): StringResponse = {
    import testkit._
    val project = p._1
    project ! DebugLocateNameReq(threadId, variableName)
    val vLoc = expectMsgType[DebugLocation]

    project ! DebugToStringReq(threadId, vLoc)
    expectMsgType[StringResponse]
  }

  def checkTopStackFrame(threadId: DebugThreadId, className: String, method: String, line: Int)(implicit testkit: TestKitFix, p: (TestActorRef[Project], TestProbe)): Unit = {
    import testkit._
    val project = p._1

    project ! DebugBacktraceReq(threadId, 0, 1)
    expectMsgType[DebugBacktrace] should matchPattern {
      case DebugBacktrace(List(DebugStackFrame(0, _, 1, `className`, `method`,
        LineSourcePosition(_, `line`), _)),
        `threadId`, "main") =>
    }
  }
}

// only for test because only the build tool really knows how to launch the JVM
object VMStarter extends SLF4JLogging {

  def logLines(src: InputStream): Future[Unit] = {
    val promise = Promise[Unit]()
    new Thread(new Runnable() {
      override def run(): Unit = {
        val sc = new Scanner(src)
        while (sc.hasNextLine) {
          if (!promise.isCompleted) promise.trySuccess(())
          log.info("DEBUGGING_PROCESS:" + sc.nextLine())
        }
      }
    }).start()
    promise.future
  }

  def java: String =
    if (Properties.isWin) Properties.javaHome + """\bin\javaw.exe"""
    else Properties.javaHome + "/bin/java"

  def apply(config: EnsimeConfig, clazz: String): (Process, String, Int, Future[Unit]) = {
    import collection.JavaConverters._

    // would be nice to have ephemeral debug ports
    val port = 5000 + scala.util.Random.nextInt(1000)

    val classpath = (config.compileClasspath ++ config.targetClasspath).mkString(File.pathSeparator)
    val args = Seq(
      java,
      "-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=" + port,
      "-Xms32m", "-Xmx64m",
      "-classpath", classpath,
      clazz
    )

    val process = new ProcessBuilder(
      args.asJava
    ).redirectErrorStream(true).start()

    val logging = logLines(process.getInputStream)

    (process, "127.0.0.1", port, logging)
  }

}
