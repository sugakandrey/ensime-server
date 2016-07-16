// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.model

import org.ensime.api._
import org.ensime.fixture._
import org.ensime.indexer.database.DatabaseService.FqnSymbol
import org.ensime.util.EnsimeSpec
import org.ensime.util.file._
import org.ensime.vfs._

class SourcePositionSpec extends EnsimeSpec
    with SharedEnsimeConfigFixture
    with SharedEnsimeVFSFixture {

  val original = EnsimeConfigFixture.SimpleTestProject.copy(
    javaLibs = Nil
  )

  "SourcePosition" should "resolve FqnSymbols for local files with no line number" in {
    withEnsimeConfig { implicit config =>
      lookup(knownFile) should matchPattern {
        case Some(LineSourcePosition(RawFile(_), 0)) =>
      }
    }
  }

  it should "resolve FqnSymbols for local with a line number" in {
    withEnsimeConfig { implicit config =>
      lookup(knownFile, Some(100)) should matchPattern {
        case Some(LineSourcePosition(RawFile(_), 100)) =>
      }
    }
  }

  it should "resolve FqnSymbols for archive entries with no line number" in {
    withEnsimeConfig { implicit config =>
      lookup(knownJarEntry) should matchPattern {
        case Some(LineSourcePosition(ArchiveFile(_, _), 0)) =>
      }
    }
  }

  it should "resolve FqnSymbols for archive entries with a line number" in {
    withEnsimeConfig { implicit config =>
      lookup(knownJarEntry, Some(100)) should matchPattern {
        case Some(LineSourcePosition(ArchiveFile(_, _), 100)) =>
      }
    }
  }

  def knownFile(implicit config: EnsimeConfig): String = {
    val f = scalaMain / "org/example/Foo.scala"
    "file://" + f
  }

  def knownJarEntry(implicit config: EnsimeConfig): String = {
    val scalatest = config.subprojects.head.referenceSourceJars.find(
      _.getName.contains("scalatest_")
    ).get.getAbsoluteFile
    "jar:" + scalatest + "!/org/scalatest/FunSpec.scala"
  }

  def lookup(uri: String, line: Option[Int] = None)(implicit config: EnsimeConfig) = {
    withVFS { implicit vfs: EnsimeVFS =>
      val sym = FqnSymbol(None, "", "", "", None, Some(uri), line, Some(0))
      LineSourcePositionHelper.fromFqnSymbol(sym)
    }
  }
}
