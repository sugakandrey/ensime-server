// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import org.ensime.api.{DeclaredAs, EnsimeConfig}
import org.ensime.fixture.{EnsimeConfigFixture, IsolatedRichPresentationCompilerFixture}
import org.ensime.indexer.{ClassfileDepickler, FieldName, FullyQualifiedName}
import org.ensime.util.EnsimeSpec
import org.ensime.vfs.ClassfileSelector
import org.ensime.vfs._

class TypelevelLibrariesSymbolToFqnSpec extends EnsimeSpec
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

  it should "index all class file in typelevel libraries" in withPresCompiler { (config, cc) =>
    val vfs = cc.vfs
    val jars = config.allJars.filter(!_.getName.contains("macro-compat"))
    jars.foreach { file =>
      val jar = vfs.vjar(file)
      val classes = (jar.findFiles(ClassfileSelector) match {
        case null => Nil
        case files => files.toList
      }).flatMap(new ClassfileDepickler(_).getClasses.values)
      classes.foreach { scalaClass =>
        verify(scalaClass.javaName, scalaClass.scalaName, scalaClass.declaredAs, cc)
        scalaClass.fields.valuesIterator.foreach { field =>
          verify(field.javaName, field.scalaName, DeclaredAs.Field, cc)
        }
        scalaClass.methods.valuesIterator.foreach { methods =>
          methods.foreach { method =>
            val methodSym = cc.askSymbolByScalaName(method.scalaName, Some(DeclaredAs.Method)).get
            methodSym shouldBe a[cc.TermSymbol]
          }
        }
      }
    }
  }

}
