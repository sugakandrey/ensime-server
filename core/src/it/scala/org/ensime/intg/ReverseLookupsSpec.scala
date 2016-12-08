// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.intg

import org.ensime.api._
import org.ensime.core.RefactoringHandlerTestUtils
import org.ensime.fixture._
import org.ensime.util.EnsimeSpec
import org.ensime.util.ensimefile.Implicits.DefaultCharset
import org.ensime.util.file._
import org.scalatest.tags.Slow

@Slow
class ReverseLookupsSpec extends EnsimeSpec
    with IsolatedEnsimeConfigFixture
    with IsolatedTestKitFixture
    with IsolatedProjectFixture
    with IsolatedAnalyzerFixture
    with RefactoringHandlerTestUtils {

  override def original: EnsimeConfig = EnsimeConfigFixture.SimpleTestProject

  "FindUsages" should "find usages using reverse lookups info" in withEnsimeConfig { implicit config =>
    withTestKit { implicit testKit =>
      withProject { (project, asyncHelper) =>
        import testKit._
        val sourceRoot = scalaMain(config)
        val fooFile = sourceRoot / "org/example/Foo.scala"

        // uses of `testMethod`
        project ! UsesOfSymbolAtPointReq(Left(fooFile), 119)
        val uses = expectMsgType[ERangePositions]
        uses.positions.map(usage => (s"${File(usage.file).getName}", usage.offset, usage.start, usage.end)) should contain theSameElementsAs List(
          ("Foo.scala", 114, 110, 172),
          ("Foo.scala", 273, 269, 283),
          ("package.scala", 94, 80, 104)
        )
      }
    }
  }

  "Refactor Rename" should "make use of reverse lookups information" in withEnsimeConfig { implicit config =>
    withTestKit { implicit testKit =>
      withProject { (project, asyncHelper) =>
        import testKit._
        val sourceRoot = scalaMain(config)
        val fooFile = sourceRoot / "org/example/Foo.scala"
        val packageFile = sourceRoot / "org/example/package.scala"

        project ! RefactorReq(1234, RenameRefactorDesc("notATestMethod", fooFile, 119, 119), interactive = false)
        expectMsgPF() {
          case response @ RefactorDiffEffect(1234, RefactorType.Rename, diff) =>
            val relevantExpectedPartFoo =
              s"""|@@ -9,3 +9,3 @@
                    |   class Foo extends Bar {
                    |-    def testMethod(i: Int, s: String) = {
                    |+    def notATestMethod(i: Int, s: String) = {
                    |       i + s.length
                    |@@ -16,3 +16,3 @@
                    |   println("Hello, " + foo.x)
                    |-  println(foo.testMethod(7, "seven"))
                    |+  println(foo.notATestMethod(7, "seven"))
                    | 
                    |""".stripMargin

            val relevantExpectedPartPackage =
              s"""|@@ -6,3 +6,3 @@
                    | 
                    |-  new Foo.Foo().testMethod(1, "")
                    |+  new Foo.Foo().notATestMethod(1, "")
                    | }
                    |""".stripMargin

            val diffContents = diff.canon.readString()
            val expectedContentsFoo = expectedDiffContent(fooFile.getPath, relevantExpectedPartFoo)
            val expectedContentsPackage = expectedDiffContent(packageFile.getPath, relevantExpectedPartPackage)
            val expectedContents = s"$expectedContentsFoo\n$expectedContentsPackage"
            if (diffContents == expectedContents) true
            else fail(s"Different diff content than expected. \n Actual content: '$diffContents' \n ExpectedRelevantContent: '$expectedContents'")
        }
      }
    }
  }
}
