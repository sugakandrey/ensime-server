// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.util.ensimefile

import java.io.File
import java.net._
import java.nio.file._

import scala.util.Properties.jdkHome

import org.ensime.api._
import org.ensime.util.ensimefile.Implicits.DefaultCharset
import org.ensime.util.path._
import org.scalatest._

class EnsimeFileSpec extends FlatSpec with Matchers {

  lazy val src: Path = Paths.get(jdkHome) / "src.zip"

  "EnsimeFile" should "construct instances from file paths" in {
    val filepath = "/foo/bar/baz.scala"
    EnsimeFile(filepath) shouldBe RawFile(Paths.get(filepath))
  }

  it should "construct instances from files" in {
    val file = new File("/foo/bar/baz.scala")
    EnsimeFile(file) shouldBe RawFile(file.toPath)
  }

  it should "construct instances from entries in jars" in {
    val jar = "/foo/bar/baz.jar"
    val entry = "/faz/gaz.scala"
    val full = s"$jar!$entry"
    EnsimeFile(full) shouldBe ArchiveFile(Paths.get(jar), entry)
  }

  it should "provide access to RichEnsimeFile" in {
    EnsimeFile(new File("/foo/bar/baz.scala")).isScala // compiles, it works
  }

  it should "create instances from URLs" in {
    val srcUrl = src.toUri.toURL

    EnsimeFile(srcUrl) shouldBe RawFile(src)
    EnsimeFile(new URL(s"jar:$srcUrl!/")) shouldBe ArchiveFile(src, "/")
  }

  it should "create instances from Windows URLs" in {
    val file = """C:\Program Files\Java\jdk1.7.0\src.zip"""
    val url = s"jar:file:/$file"

    EnsimeFile(url) shouldBe RawFile(Paths.get(file))
  }

  "RichRawFile" should "support isJava / isScala" in {
    RawFile(Paths.get("/foo/bar/baz.scala")).isScala shouldBe true
    RawFile(Paths.get("/foo/bar/baz.java")).isJava shouldBe true
  }

  it should "check for file existence with exists()" in {
    EnsimeFile("../LICENSE").exists() shouldBe true
    EnsimeFile("there-is-no-spoon").exists() shouldBe false
  }

  it should "load file contents with readStringDirect()" in {
    EnsimeFile("../LICENSE").readStringDirect().contains("GPL") shouldBe true
  }

  "RichArchiveFile" should "support isJava / isScala" in {
    val jar = Paths.get("/foo/bar/baz.jar")
    ArchiveFile(jar, "/foo.scala").isScala shouldBe true
    ArchiveFile(jar, "/foo.java").isJava shouldBe true
  }

  it should "check for jar and entry existence with exists()" in {
    EnsimeFile("there-is-no.jar!/thing").exists() shouldBe false
    EnsimeFile(s"$src!/java/lang/String.java").exists() shouldBe true
    EnsimeFile(s"$src!/java/lang/Ensime.scala").exists() shouldBe false
  }

  it should "load entry contents with readStringDirect()" in {
    EnsimeFile(s"$src!/java/lang/String.java").readStringDirect().contains("-6849794470754667710L") shouldBe true
  }

}
