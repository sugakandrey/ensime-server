// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.util

import java.util.{ Timer, TimerTask }
import java.util.concurrent.TimeUnit

import scala.concurrent.duration._
import scala.collection.JavaConverters._

import com.typesafe.config.ConfigFactory
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest._
import org.scalatest.concurrent.Eventually
import org.scalatest.time._
import org.slf4j.LoggerFactory
import org.slf4j.bridge.SLF4JBridgeHandler

/**
 * Indicates a test that requires launching a JVM under debug mode.
 *
 * These are typically very unstable on Windows.
 */
object Debugger extends Tag("Debugger")

/**
 * Boilerplate remover and preferred testing style in ENSIME.
 */
trait EnsimeSpec extends FlatSpec
    with Matchers
    with Inside
    with Retries
    with Eventually
    with TryValues
    with Inspectors
    with TypeCheckedTripleEquals
    with BeforeAndAfterAll { self =>

  SLF4JBridgeHandler.removeHandlersForRootLogger()
  SLF4JBridgeHandler.install()
  val log = LoggerFactory.getLogger(this.getClass)

  private val timer = new Timer(true)
  timer.schedule(
    new TimerTask {
      override def run(): Unit = {
        println(s"${self.getClass} is still running!")
        Thread.getAllStackTraces.asScala.foreach {
          case (thread, stacks) =>
            println(thread.getName)
            stacks.foreach { stack =>
              println(s"\tat$stack")
            }
        }
      }
    },
    300000L // 5 mins
  )

  private val akkaTimeout: Duration = ConfigFactory.load().getDuration("akka.test.default-timeout", TimeUnit.MILLISECONDS).milliseconds
  override val spanScaleFactor: Double = ConfigFactory.load().getDouble("akka.test.timefactor")
  implicit override val patienceConfig: PatienceConfig = PatienceConfig(
    timeout = scaled(akkaTimeout),
    interval = scaled(Span(5, Millis))
  )

  // taggedAs(org.scalatest.tagobject.Retryable)
  // will be retried (don't abuse it)
  override def withFixture(test: NoArgTest) = {
    if (isRetryable(test)) withRetry { super.withFixture(test) }
    else super.withFixture(test)
  }

  override def afterAll(): Unit = {
    timer.cancel()
    super.afterAll()
  }

}
