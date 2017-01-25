// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.config

import org.ensime.util.file._
import org.ensime.util.{ EnsimeSpec, EscapingStringInterpolation }

import org.ensime.api._

import scala.util.Properties

class EnsimeConfigSpec extends EnsimeSpec {

  import EscapingStringInterpolation._

  def test(dir: File, contents: String, testFn: (EnsimeConfig) => Unit): Unit = {
    testFn(EnsimeConfigProtocol.parse(contents))
  }

  "EnsimeConfig" should "parse a simple config" in withTempDir { dir =>
    val abc = dir / "abc"
    val cache = dir / ".ensime_cache"
    val javaHome = File(Properties.javaHome)

    abc.mkdirs()
    cache.mkdirs()

    test(dir, s"""
(:name "project"
 :scala-version "2.10.4"
 :java-home "$javaHome"
 :root-dir "$dir"
 :cache-dir "$cache"
 :reference-source-roots ()
 :subprojects ((:name "module1"
                :scala-version "2.10.4"
                :depends-on-modules ()
                :targets ("$abc")
                :test-targets ("$abc")
                :source-roots ()
                :reference-source-roots ()
                :compiler-args ()
                :runtime-deps ()
                :test-deps ()))
 :projects ((:id (:project "module1" :config "compile")
             :depends ()
             :sources ()
             :targets ("$abc")
             :scalac-options ()
             :javac-options ()
             :library-jars ()
             :library-sources ()
             :library-docs ())))""", { implicit config =>
      config.name shouldBe "project"
      config.scalaVersion shouldBe "2.10.4"
      val module1 = config.modules("module1")
      module1.name shouldBe "module1"
      module1.dependencies shouldBe empty
      config.projects.size shouldBe 1
    })
  }

  it should "parse a minimal config for a binary only project" in withTempDir { dir =>
    val abc = dir / "abc"
    val cache = dir / ".ensime_cache"
    val javaHome = File(Properties.javaHome)

    abc.mkdirs()
    cache.mkdirs()

    test(dir, s"""
(:name "project"
 :scala-version "2.10.4"
 :java-home "$javaHome"
 :root-dir "$dir"
 :cache-dir "$cache"
 :subprojects ((:name "module1"
                :scala-version "2.10.4"
                :targets ("$abc"))))""", { implicit config =>

      config.name shouldBe "project"
      config.scalaVersion shouldBe "2.10.4"
      val module1 = config.modules("module1")
      module1.name shouldBe "module1"
      module1.dependencies shouldBe empty
      module1.targets should have size 1
    })
  }
}
