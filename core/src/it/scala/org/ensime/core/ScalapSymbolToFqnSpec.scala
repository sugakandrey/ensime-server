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
  import ReallyRichPresentationCompilerFixture._

  override def original: EnsimeConfig = EnsimeConfigFixture.ShapelessTestProject

  "ScalapSymbolToFqn" should "find and resolve class names defined in Predef" in withPresCompiler { (_, cc) =>
    val vfs = cc.vfs

    val predef = vfs.vres("scala/Predef.class")
    val definedClassNames = new ClassfileDepickler(predef).getClasses
    definedClassNames.length should ===(22)
    /* definedClassNames.foreach { className =>
      val sym = cc.askSymbolByFqn(className)
      sym shouldBe defined
      sym.get should not be an[cc.NoSymbol]
    }
        This is currently broken, see
        https://github.com/ensime/ensime-server/issues/1507 */
  }

  it should "find and resolve field names in Predef" in withPresCompiler { (_, cc) =>
    val vfs = cc.vfs
    val search = cc.search

    val predef = vfs.vres("scala/Predef.class")
    val fieldNames = new ClassfileDepickler(predef).getFields
    fieldNames.foreach { field =>
      val sym = cc.askSymbolByFqn(field)
      sym shouldBe defined
      sym.get should not be an[cc.NoSymbol]
    }
  }
}
