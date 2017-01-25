// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import java.io.File
import java.nio.file._

import scala.util.Properties.jdkHome

import org.ensime.util.file._
import org.ensime.util.path._
import org.ensime.util.ensimefile._
import org.ensime.util.EnsimeSpec
import org.ensime.api._

// this test is mostly showing what Canon can do, we're testing
// shapeless more than our specific Poly1.
class CanonSpec extends EnsimeSpec {

  val file = new File(".")
  val canon = file.canon
  assert(file != canon)

  "Canon" should "canon File" in {
    Canonised(file) shouldBe canon
  }

  it should "canon List of Files" in {
    Canonised(List(file)) shouldBe List(canon)
  }

  class MyFile(name: String) extends File(name)

  it should "canon subtypes of File" in {
    val mine = new MyFile(".")
    val myCanon = mine.canon
    assert(mine != myCanon)
    Canonised(mine) shouldBe myCanon
  }

  it should "canon an RpcRequest" in {
    val request = TypeAtPointReq(Left(file), OffsetRange(100)): RpcRequest
    val expected = TypeAtPointReq(Left(canon), OffsetRange(100))
    Canonised(request) shouldBe expected
  }

  it should "canon an EnsimeServerMessage" in {
    val response = Breakpoint(file, 13): RpcResponse
    val expected = Breakpoint(canon, 13)
    Canonised(response) shouldBe expected
  }

  it should "canon a RawFile" in withTempDir { dir =>
    val ef = List(RawFile(file.toPath))
    val expected = List(RawFile(canon.toPath))

    Canon.config = EnsimeConfig(
      dir, dir, dir,
      "config", "version",
      Nil, Nil, Nil, Nil, Nil
    )

    Canonised(ef) shouldBe expected
  }

  it should "canon an ArchiveFile" in withTempDir { dir =>
    val src = Paths.get(jdkHome) / "src.zip"

    val entry = EnsimeFile(s"$src!/java/lang/String.java")
    val extracted = RawFile(dir.toPath / "dep-src/source-jars/java/lang/String.java")

    val ef = List(entry)
    val expected = List(extracted.canon)

    Canon.config = EnsimeConfig(
      dir, dir, dir,
      "config", "version",
      Nil, Nil, Nil, Nil, Nil
    )

    Canonised(ef) shouldBe expected
  }

}
