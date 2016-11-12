// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import java.io.File
import java.nio.charset.Charset
import java.text.SimpleDateFormat
import java.util.Date
import org.ensime.api._
import org.ensime.fixture._
import org.ensime.util.EnsimeSpec
import org.scalatest.Assertions

class RefactoringHandlerSpec extends EnsimeSpec
    with IsolatedAnalyzerFixture
    with RichPresentationCompilerTestUtils
    with RefactoringHandlerTestUtils {

  val encoding = "UTF-16"
  def original = EnsimeConfigFixture.EmptyTestProject.copy(
    compilerArgs = List("-encoding", encoding)
  )

  // transitionary methods
  def ContentsSourceFileInfo(file: File, contents: String) =
    SourceFileInfo(RawFile(file.toPath), Some(contents))
  def ContentsInSourceFileInfo(file: File, contentsIn: File) =
    SourceFileInfo(RawFile(file.toPath), contentsIn = Some(contentsIn))

  it should "add imports on the first line" in withAnalyzer { (dir, analyzerRef) =>
    import org.ensime.util.file._

    val file = srcFile(dir, "tmp-contents", contents(
      "import java.lang.Integer.toBinaryString",
      "import java.lang.String.valueOf",
      "",
      "trait Temp {",
      "  valueOf(5)",
      "  vo(\"5\")",
      "  toBinaryString(27)",
      "}"
    ), write = true, encoding = encoding)

    val analyzer = analyzerRef.underlyingActor

    val procId = 1
    val result = analyzer.handleRefactorRequest(
      new RefactorReq(
        procId, AddImportRefactorDesc("java.lang.Integer.{valueOf => vo}", new File(file.path)), false
      )
    )
    val diffContent = extractDiffFromResponse(result, analyzer.charset)

    val relevantExpectedPart = s"""|@@ -2,2 +2,3 @@
                                   | import java.lang.String.valueOf
                                   |+import java.lang.Integer.{valueOf => vo}
                                   | \n""".stripMargin

    val expectedContents = expectedDiffContent(file.path, relevantExpectedPart)

    diffContent should ===(expectedContents)
  }

  it should "add imports even if none exist" in {
    withAnalyzer { (dir, analyzerRef) =>

      val file = srcFile(dir, "tmp-contents", contents(
        "package org.ensime.testing",
        "",
        "trait Temp {",
        "  valueOf(5)",
        "  vo(\"5\")",
        "  toBinaryString(27)",
        "}"
      ), write = true, encoding = encoding)

      val analyzer = analyzerRef.underlyingActor

      val procId = 1
      val result = analyzer.handleRefactorRequest(
        new RefactorReq(
          procId, AddImportRefactorDesc("java.lang.Integer", new File(file.path)), false
        )
      )

      val diffContent = extractDiffFromResponse(result, analyzer.charset)

      val relevantExpectedPart = contents(
        "@@ -2,2 +2,4 @@",
        " ",
        "+import java.lang.Integer",
        "+",
        " trait Temp {",
        ""
      )

      val expectedContents = expectedDiffContent(file.path, relevantExpectedPart)

      diffContent should ===(expectedContents)
    }
  }

  it should "rename a function id with params' opening/closing parenthesis on different lines" in withAnalyzer { (dir, analyzerRef) =>

    val file = srcFile(dir, "tmp-contents", contents(
      "package org.ensime.testing",
      "trait Foo {",
      "def doIt(",
      ") = \"\"",
      "}",
      ""
    ), write = true, encoding = encoding)

    val analyzer = analyzerRef.underlyingActor

    val procId = 1
    val result = analyzer.handleRefactorRequest(
      new RefactorReq(
        procId, RenameRefactorDesc("doItNow", new File(file.path), 43, 47), false
      )
    )

    val diffContent = extractDiffFromResponse(result, analyzer.charset)

    val relevantExpectedPart = s"""|@@ -2,3 +2,3 @@
                                   | trait Foo {
                                   |-def doIt(
                                   |+def doItNow(
                                   | ) = ""
                                   |""".stripMargin
    val expectedContents = expectedDiffContent(file.path, relevantExpectedPart)

    diffContent should ===(expectedContents)
  }

  it should "organize imports when 3 imports exist" in {
    withAnalyzer { (dir, analyzerRef) =>
      //when 3 imports exist
      // "produce a diff file in the unified output format"

      val file = srcFile(dir, "tmp-contents", contents(
        "import java.lang.Integer.{valueOf => vo}",
        "import java.lang.Integer.toBinaryString",
        "import java.lang.String.valueOf",
        " ",
        "trait Temp {",
        "  valueOf(5)",
        "  vo(\"5\")",
        "  toBinaryString(27)",
        "}"
      ), write = true, encoding = encoding)

      val analyzer = analyzerRef.underlyingActor

      val procId = 1
      val result = analyzer.handleRefactorRequest(
        new RefactorReq(
          procId, OrganiseImportsRefactorDesc(new File(file.path)), false
        )
      )

      val diffContent = extractDiffFromResponse(result, analyzer.charset)

      val relevantExpectedPart = s"""|@@ -1,3 +1,2 @@
                                     |-import java.lang.Integer.{valueOf => vo}
                                     |-import java.lang.Integer.toBinaryString
                                     |+import java.lang.Integer.{valueOf => vo, toBinaryString}
                                     | import java.lang.String.valueOf
                                     |""".stripMargin
      val expectedContents = expectedDiffContent(file.path, relevantExpectedPart)

      diffContent should ===(expectedContents)
    }
  }

  it should "organize and group imports" in {
    withAnalyzer { (dir, analyzerRef) =>

      val file = srcFile(dir, "tmp-contents", contents(
        "import scala._",
        "import java.lang.Integer",
        "import scala.Int",
        "import java._",
        " ",
        "trait Temp {",
        "  def i(): Int",
        "  def j(): Integer",
        "}",
        ""
      ), write = true, encoding = encoding)

      val analyzer = analyzerRef.underlyingActor

      val procId = 1
      val result = analyzer.handleRefactorRequest(
        new RefactorReq(
          procId, OrganiseImportsRefactorDesc(new File(file.path)), false
        )
      )

      val diffContent = extractDiffFromResponse(result, analyzer.charset)

      val relevantExpectedPart = s"""|@@ -1,5 +1,5 @@
                                     |-import scala._
                                     |-import java.lang.Integer
                                     |-import scala.Int
                                     | import java._
                                     |+import java.lang.Integer
                                     |+
                                     |+import scala._
                                     |  \n""".stripMargin
      val expectedContents = expectedDiffContent(file.path, relevantExpectedPart)

      diffContent should ===(expectedContents)
    }
  }

  it should "support RenameSourceFileChange change" in {
    withAnalyzer { (dir, analyzerRef) =>
      val file = srcFile(dir, "Foo.scala", contents(
        "object Foo {}"
      ), write = true, encoding = encoding)
      val analyzer = analyzerRef.underlyingActor

      val procId = 1

      val result = analyzer.handleRefactorRequest(
        new RefactorReq(
          procId, RenameRefactorDesc("Qux", new File(file.path), 8, 10), false
        )
      )

      val diffContent = extractDiffFromResponse(result, analyzer.charset)
      val renamed = new File(file.path.replace("Foo.scala", "Qux.scala"))

      val expectedChangesFoo = s"""|@@ -1,1 +0,0 @@
                                   |-object Foo {}
                                   |""".stripMargin
      val expectedChangesQux = s"""|@@ -0,0 +1,1 @@
                                   |+object Qux {}
                                   |""".stripMargin

      val changes = Seq(
        (file.path, DeleteFile, expectedChangesFoo),
        (renamed.getPath, CreateFile, expectedChangesQux)
      )
      val expectedDiff = expectedDiffContent(changes)

      diffContent should ===(expectedDiff)
    }
  }
}

trait RefactoringHandlerTestUtils extends Assertions {

  import org.ensime.util.file._
  sealed trait FileChangeType
  case object CreateFile extends FileChangeType
  case object DeleteFile extends FileChangeType
  case object ChangeContents extends FileChangeType

  private val epoch: String = "1970-01-01 12:00:00 +0000"

  def expectedDiffContent(files: Seq[(String, FileChangeType, String)]): String = {
    val sdf = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss Z")
    files.map {
      case (filepath, fileChangeType, changes) =>
        val originalTime = fileChangeType match {
          case CreateFile => epoch
          case _ => sdf.format(new Date(new File(filepath).lastModified()))
        }
        val modifiedTime = fileChangeType match {
          case DeleteFile => epoch
          case _ => originalTime
        }
        val filepathPart = s"""|--- ${filepath}	${originalTime}
                               |+++ ${filepath}	${modifiedTime}
                               |""".stripMargin
        filepathPart + changes
    }.mkString("\n")
  }

  def expectedDiffContent(filepath: String, expectedContent: String): String =
    expectedDiffContent(Seq((filepath, ChangeContents, expectedContent)))

  def extractDiffFromResponse(response: RpcResponse, charset: Charset) = response match {
    case RefactorDiffEffect(_, _, f) =>
      val diffFile = f.canon
      val diffContent = diffFile.readString()(charset)

      diffFile.delete()
      diffContent

    case default =>
      fail("Not expected type of RpcResponse.")
  }
}
