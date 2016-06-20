package org.ensime.core

import org.ensime.api.EnsimeConfig
import org.ensime.fixture.{ EnsimeConfigFixture, IsolatedRichPresentationCompilerFixture }
import org.ensime.indexer._
import org.ensime.util.EnsimeSpec
import org.ensime.vfs._

class ScalapSymbolToFqnSpec extends EnsimeSpec
    with IsolatedRichPresentationCompilerFixture
    with RichPresentationCompilerTestUtils
    with ReallyRichPresentationCompilerFixture {

  override def original: EnsimeConfig = EnsimeConfigFixture.ShapelessTestProject

  "ScalapSymbolToFqn" should "find and resolve class names defined in Predef" in withPresCompiler { (_, cc) =>
    val vfs = cc.vfs

    val predef = vfs.vres("scala/Predef.class")
    val definedClassNames = new ClassfileDepickler(predef).getClasses
    definedClassNames.length should ===(22)
    definedClassNames.foreach { scalaClass =>
      val sym = cc.askSymbolByFqn(scalaClass.javaName)
      sym shouldBe defined
      sym.get should not be a [cc.NoSymbol]
    }
  }

    it should "find and resolve field names in Predef" in withPresCompiler { (_, cc) =>
      val vfs = cc.vfs
      val search = cc.search

      val predef = vfs.vres("scala/Predef.class")
      val fieldNames = new ClassfileDepickler(predef).getClasses.flatMap(_.fields)
      println(fieldNames.length)
      fieldNames.foreach { field =>
        val sym = cc.askSymbolByFqn(field.javaName)
        sym shouldBe defined
        sym.get shouldBe a[cc.TermSymbol]
      }
    }

    it should "index shapeless jar" in withPresCompiler { (config, cc) =>
      val vfs = cc.vfs
      val shapelessFile = config.allJars.find(_.getName.contains("shapeless"))
      val jar = vfs.vjar(shapelessFile.get)
      val classFiles = jar.findFiles(ClassfileSelector) match {
        case null => Nil
        case files => files.toList
      }
      println(classFiles.length)
      val classes = classFiles.flatMap(new ClassfileDepickler(_).getClasses)
      classes.foreach { scalaClass =>
        println(scalaClass.javaName)
        val sym = cc.askSymbolByFqn(scalaClass.javaName)
        sym shouldBe defined
        sym.get should not be a[cc.NoSymbol]
        scalaClass.fields.foreach { field =>
          cc.askSymbolByFqn(field.javaName).get shouldBe a[cc.TermSymbol]
        }
      }
    }
}
