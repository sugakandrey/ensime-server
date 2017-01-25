// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import org.ensime.api.{ DeclaredAs, EnsimeConfig }
import org.ensime.fixture.{ EnsimeConfigFixture, SharedRichPresentationCompilerFixture }
import org.ensime.indexer._
import org.ensime.util.EnsimeSpec
import org.ensime.vfs._

class ScalapSymbolToFqnSpec extends EnsimeSpec
    with SharedRichPresentationCompilerFixture
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
    definedClassNames.foreach {
      case (_, scalaClass) =>
        verify(scalaClass.javaName, scalaClass.scalaName, scalaClass.declaredAs, cc)
    }
  }

  it should "find and resolve field names in Predef" in withPresCompiler { (_, cc) =>
    val vfs = cc.vfs

    val predef = vfs.vres("scala/Predef.class")
    val fieldNames = new ClassfileDepickler(predef).getClasses.values.flatMap(_.fields.valuesIterator)
    fieldNames.foreach { field =>
      verify(field.javaName, field.scalaName, DeclaredAs.Field, cc)
    }
  }
}
