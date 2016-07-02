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

  override def original: EnsimeConfig = EnsimeConfigFixture.FqnsTestProject

  private def verify(javaName: FullyQualifiedName, scalaName: String, declaredAs: DeclaredAs, cc: RichPresentationCompiler): Unit = {
    val byJavaName = cc.askSymbolByFqn(javaName).get
    val byScalaName = cc.askSymbolByScalaName(scalaName, Some(declaredAs)).get
    byJavaName should not be a[cc.NoSymbol]
    javaName match {
      case FieldName(_, _) => byJavaName shouldBe a[cc.TermSymbol]
      case _ =>
    }
    byScalaName.fullName should ===(byJavaName.fullName)
  }

  "ScalapSymbolToFqn" should "find and resolve class names defined in Predef" in withPresCompiler { (_, cc) =>
    val vfs = cc.vfs

    val predef = vfs.vres("scala/Predef.class")
    val definedClassNames = new ClassfileDepickler(predef).getClasses
    definedClassNames.length should ===(16)
    definedClassNames.foreach { scalaClass =>
      verify(scalaClass.javaName, scalaClass.scalaName, scalaClass.declaredAs, cc)
    }
  }

  it should "find and resolve field names in Predef" in withPresCompiler { (_, cc) =>
    val vfs = cc.vfs

    val predef = vfs.vres("scala/Predef.class")
    val fieldNames = new ClassfileDepickler(predef).getClasses.flatMap(_.fields)
    fieldNames.foreach { field =>
      verify(field.javaName, field.scalaName, DeclaredAs.Field, cc)
    }
  }

  it should "index all class file in typelevel libraries" in withPresCompiler { (config, cc) =>
    val vfs = cc.vfs
    val jars = config.allJars
    jars.foreach { file =>
      val jar = vfs.vjar(file)
      val classes = (jar.findFiles(ClassfileSelector) match {
        case null => Nil
        case files => files.toList
      }).flatMap(new ClassfileDepickler(_).getClasses)
      classes.foreach { scalaClass =>
        verify(scalaClass.javaName, scalaClass.scalaName, scalaClass.declaredAs, cc)
        scalaClass.fields.foreach { field =>
          verify(field.javaName, field.scalaName, DeclaredAs.Field, cc)
        }
        scalaClass.methods.foreach { method =>
          val methodSym = cc.askSymbolByScalaName(method.scalaName, Some(DeclaredAs.Method)).get
          methodSym shouldBe a[cc.TermSymbol]
        }
      }
    }
  }
}
