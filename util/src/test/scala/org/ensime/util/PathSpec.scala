// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.util

import java.nio.file.{ Files, Path }
import org.scalatest._
import org.scalatest.Matchers._

class PathSpec extends FlatSpec {
  import ensimefile.Implicits.DefaultCharset
  import path._

  val unix = Array[Byte](0x61, 0x0a, 0x62, 0x0a, 0x63)
  val windows = Array[Byte](0x61, 0x0d, 0x0a, 0x62, 0x0d, 0x0a, 0x63)
  val abc = List("a", "b", "c")

  // all the later tests assume that these work, so do them first
  "path._" should "provide scoped temp directories" in {
    var scoped: Path = null
    withTempDirPath { dir =>
      assert(dir.isDirectory())
      dir.canon shouldBe dir
      scoped = dir
    }
    assert(!scoped.isDirectory())
  }

  it should "provide scoped temp files" in {
    var scoped: Path = null
    withTempFilePath { file =>
      assert(file.isFile())
      scoped = file
    }
    assert(!scoped.exists())
  }

  it should "canonically resolve symbolic links" in withTempDirPath { dir =>
    val orig = dir / "orig" // NOTE: doesn't exist (we want this to work regardless)
    val link = dir / "link"

    Files.createSymbolicLink(link, orig)

    link.canon shouldBe orig
  }

  it should "canonically resolve files in symbolically linked directories" in withTempDirPath { dir =>
    val origDir = dir / "orig"
    val origFile = (origDir / "file")
    val linkDir = dir / "link"

    origDir.mkdirs()
    origFile.write(windows)

    Files.createSymbolicLink(linkDir, origDir)

    (linkDir / "file").canon shouldBe origFile
  }

  it should "create Files with parents if necessary" in withTempDirPath { dir =>
    val sub = (dir / "baz/bar/foo.wiz")
    sub.mkdirs()
    assert(sub.exists())
  }

  it should "copy recursively" in withTempDirPath { dir =>
    val from = dir / "from"
    (from / "foo/bar").mkdirs()
    (from / "foo/bar/baz.txt").write(unix)

    val to = dir / "to"
    to.mkdirs()

    from.copyDirTo(to)

    assert((to / "foo/bar/baz.txt").isFile)
  }

  it should "delete recursively" in withTempDirPath { dir =>
    val from = dir / "from"
    (from / "foo/bar").mkdirs()
    (from / "foo/bar/baz.txt").write(unix)

    from.deleteDirRecursively()

    assert(!from.exists())
  }

  it should "read and write byte arrays" in withTempFilePath { file =>
    file.write(windows)
    file.readBytes() shouldEqual windows
  }

  it should "read lines from a File with UNIX line endings" in withTempFilePath { file =>
    file.write(unix)
    file.readLines shouldBe abc
  }

  it should "read lines from a File with Windows line endings" in withTempFilePath { file =>
    file.write(windows)
    file.readLines shouldBe abc
  }

  it should "read a File as a String preserving CR" in withTempFilePath { file =>
    file.write(windows)

    // carriage returns are preserved
    file.readString() shouldBe "a\r\nb\r\nc"
  }

}
