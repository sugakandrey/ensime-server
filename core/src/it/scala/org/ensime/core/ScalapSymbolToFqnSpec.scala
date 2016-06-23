package org.ensime.core

import org.ensime.api.{ DeclaredAs, EnsimeConfig }
import org.ensime.fixture.{ EnsimeConfigFixture, IsolatedRichPresentationCompilerFixture }
import org.ensime.indexer._
import org.ensime.util.EnsimeSpec
import org.ensime.vfs._

class ScalapSymbolToFqnSpec extends EnsimeSpec
    with IsolatedRichPresentationCompilerFixture
    with RichPresentationCompilerTestUtils
    with ReallyRichPresentationCompilerFixture {

  override def original: EnsimeConfig = EnsimeConfigFixture.ShapelessTestProject

  private def verify(javaName: FullyQualifiedName, scalaName: String, isTerm: Boolean, cc: RichPresentationCompiler): Unit = {
    val byJavaName = cc.askSymbolByFqn(javaName).get
    val byScalaName = cc.askSymbolByScalaName(scalaName, Some(isTerm)).get
    byJavaName should not be a[cc.NoSymbol]
    javaName match {
      case FieldName(_, _) => byJavaName shouldBe a[cc.TermSymbol]
      case MethodName(_, _, _) => byJavaName shouldBe a[cc.MethodSymbol]
      case _ =>
    }
    byScalaName.fullName should ===(byJavaName.fullName)
  }

  "ScalapSymbolToFqn" should "find and resolve class names defined in Predef" in withPresCompiler { (_, cc) =>
    val vfs = cc.vfs

    val predef = vfs.vres("scala/Predef.class")
    val definedClassNames = new ClassfileDepickler(predef).getClasses
    definedClassNames.length should ===(22)
    definedClassNames.foreach { scalaClass =>
      val isObject = scalaClass.declaredAs == DeclaredAs.Object
      verify(scalaClass.javaName, scalaClass.scalaName, isObject, cc)
    }
  }

  it should "find and resolve field names in Predef" in withPresCompiler { (_, cc) =>
    val vfs = cc.vfs
    val search = cc.search

    val predef = vfs.vres("scala/Predef.class")
    val fieldNames = new ClassfileDepickler(predef).getClasses.flatMap(_.fields)
    fieldNames.foreach { field =>
      verify(field.javaName, field.scalaName, isTerm = true, cc)
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
    val classes = classFiles.flatMap(new ClassfileDepickler(_).getClasses)
    classes.foreach { scalaClass =>
      val isObject = scalaClass.declaredAs == DeclaredAs.Object
      verify(scalaClass.javaName, scalaClass.scalaName, isObject, cc)
      scalaClass.fields.foreach { field =>
        verify(field.javaName, field.scalaName, isTerm = true, cc)
      }
    }
  }
}
