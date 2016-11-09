// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.intg

import java.nio.file.Files

import org.apache.commons.io.FileUtils
import org.ensime.api._
import org.ensime.fixture._
import org.ensime.util.EnsimeSpec
import org.ensime.util.file._

/**
 * Tries to simulate SBT clean/compile to stress test timing issues.
 *
 * (which also tests the file watchers).
 */
class CompileTimingTest extends EnsimeSpec
    with IsolatedEnsimeConfigFixture
    with IsolatedTestKitFixture
    with IsolatedProjectFixture {

  val original = EnsimeConfigFixture.TimingTestProject

  "ensime-server" should "handle multiple sbt clean / compile" in {
    withEnsimeConfig { implicit config =>
      withTestKit { implicit testkit =>
        withProject { (project, asyncHelper) =>
          import testkit._

          val sourceRoot = scalaMain(config)

          val example = sourceRoot / "p1/Example.scala"

          val target = (mainTarget / "..").canon
          val targetBak = (target / "../scala-classes.bak").canon

          val exampleDiskInfo = SourceFileInfo(example, None, None)
          val exampleMemory = SourceFileInfo(example, None, Some(example))

          FileUtils.copyDirectory(target, targetBak)

          project ! TypecheckFileReq(exampleDiskInfo)
          expectMsg(VoidResponse)
          asyncHelper.expectMsg(FullTypeCheckCompleteEvent)

          // GUI usually responds to each typecheck by requesting symbols
          project ! SymbolDesignationsReq(Right(exampleDiskInfo), 0, 70, SourceSymbol.allSymbols)
          expectMsgType[SymbolDesignations]

          // typecheck an in-memory version of the file
          project ! TypecheckFileReq(exampleMemory)
          expectMsg(VoidResponse)

          asyncHelper.expectMsg(FullTypeCheckCompleteEvent)
          project ! SymbolDesignationsReq(Right(exampleMemory), 0, 70, SourceSymbol.allSymbols)
          expectMsgType[SymbolDesignations]

          // simulate sbt clean https://github.com/sbt/sbt/issues/106
          target.tree.reverse.filter(_.isFile).foreach(_.delete())

          asyncHelper.receiveN(2) should contain theSameElementsAs (Seq(
            FullTypeCheckCompleteEvent,
            CompilerRestartedEvent
          ))

          project ! SymbolDesignationsReq(Right(exampleDiskInfo), 0, 70, SourceSymbol.allSymbols)
          expectMsgType[SymbolDesignations]

          // simulate sbt compile
          targetBak.tree.toList.foreach { file =>
            // something better (using file walker) should go into Path utils...
            val from = file.toPath
            val relative = targetBak.toPath.relativize(from)
            val to = target.toPath.resolve(relative)
            if (Files.isRegularFile(from)) {
              // WORKAROUND: https://bugs.openjdk.java.net/browse/JDK-8029608
              // this throws AccessDeniedException if we deleted the folders
              Files.copy(from, to)
            }
          }

          asyncHelper.receiveN(2) should contain theSameElementsAs (Seq(
            FullTypeCheckCompleteEvent,
            CompilerRestartedEvent
          ))

          project ! SymbolDesignationsReq(Right(exampleDiskInfo), 0, 70, SourceSymbol.allSymbols)
          expectMsgType[SymbolDesignations]
        }
      }
    }
  }
}
