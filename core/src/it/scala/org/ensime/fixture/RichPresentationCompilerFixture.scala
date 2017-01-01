// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.fixture

import java.io.File

import scala.collection.immutable.Queue
import scala.tools.nsc.Settings
import scala.tools.nsc.interactive.Global

import akka.actor.ActorSystem
import akka.testkit.TestProbe
import org.ensime.api._
import org.ensime.core._
import org.ensime.indexer._
import org.ensime.util.{ PresentationReporter, ReportHandler }
import org.ensime.vfs._
import org.slf4j.LoggerFactory

trait RichPresentationCompilerFixture {
  def withRichPresentationCompiler(
    testCode: (TestKitFix, EnsimeConfig, RichPresentationCompiler) => Any
  ): Any
}

final class TestReporter(
  val handler: TestReportHandler = new TestReportHandler
) extends PresentationReporter(handler)
final class TestReportHandler extends ReportHandler {
  var messages = Queue.empty[String]
  override def messageUser(str: String): Unit = {
    messages = messages enqueue str
  }
  var clears = 0
  override def clearAllScalaNotes(): Unit = {
    clears += 1
  }
  @volatile var notes = Queue.empty[Note]
  override def reportScalaNotes(list: List[Note]): Unit = {
    notes = notes enqueue list
  }
}

object RichPresentationCompilerFixture {
  private[fixture] def create(
    config: EnsimeConfig,
    search: SearchService
  )(
    implicit
    system: ActorSystem,
    vfs: EnsimeVFS
  ): RichPresentationCompiler = {
    val scalaLib = config.allJars.find(_.getName.contains("scala-library")).get

    val presCompLog = LoggerFactory.getLogger(classOf[Global])
    val settings = new Settings(presCompLog.error)
    settings.YpresentationDebug.value = presCompLog.isTraceEnabled
    settings.YpresentationVerbose.value = presCompLog.isDebugEnabled
    settings.verbose.value = presCompLog.isDebugEnabled
    //settings.usejavacp.value = true
    settings.bootclasspath.append(scalaLib.getAbsolutePath)
    settings.classpath.value = config.compileClasspath.mkString(File.pathSeparator)

    val reporter = new TestReporter
    val indexer = TestProbe()
    val parent = TestProbe()

    new RichPresentationCompiler(
      config, settings, reporter, parent.ref, indexer.ref, search
    )
  }
}

trait IsolatedRichPresentationCompilerFixture
    extends RichPresentationCompilerFixture
    with IsolatedEnsimeVFSFixture
    with IsolatedTestKitFixture
    with IsolatedSearchServiceFixture {

  override def withRichPresentationCompiler(
    testCode: (TestKitFix, EnsimeConfig, RichPresentationCompiler) => Any
  ): Any = {
    withVFS { implicit vfs =>
      withTestKit { testkit =>
        import testkit._
        withSearchService { (config, search) =>
          import org.ensime.fixture.RichPresentationCompilerFixture._
          val pc = create(config, search)
          try {
            testCode(testkit, config, pc)
          } finally {
            pc.askShutdown()
          }
        }
      }
    }
  }

}

trait SharedRichPresentationCompilerFixture
    extends RichPresentationCompilerFixture
    with SharedTestKitFixture
    with SharedSearchServiceFixture {

  private[fixture] var pc: RichPresentationCompiler = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    import org.ensime.fixture.RichPresentationCompilerFixture._
    implicit val system = _testkit.system
    pc = create(_config, _search)
  }

  override def withRichPresentationCompiler(
    testCode: (TestKitFix, EnsimeConfig, RichPresentationCompiler) => Any
  ): Any = testCode(_testkit, _config, pc)
}
