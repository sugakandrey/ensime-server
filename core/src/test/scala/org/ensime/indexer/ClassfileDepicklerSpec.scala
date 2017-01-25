// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.indexer

import org.ensime.fixture.SharedEnsimeVFSFixture
import org.ensime.util.EnsimeSpec
import org.ensime.vfs._

import scala.util.Try

class ClassfileDepicklerSpec extends EnsimeSpec with SharedEnsimeVFSFixture {

  "ClassfileDepickler" should "not depickle J2SE classes" in withVFS { vfs =>
    new ClassfileDepickler(vfs.vres("java/lang/String.class")).scalasig should ===(None)
  }

  it should "support typical Scala classes" in withVFS { vfs =>
    new ClassfileDepickler(vfs.vres("scala/collection/immutable/List.class")).scalasig shouldBe defined
  }

  it should "not expect anything in companions" in withVFS { vfs =>
    new ClassfileDepickler(vfs.vres("scala/collection/immutable/List$.class")).scalasig should ===(None)
  }

  it should "not expect anything in closures" in withVFS { vfs =>
    // scala 2.10/2.11 specific, there will be no "scala/io/Source$$anonfun$1.class" generated under 2.12
    val anonFun = Try { vfs.vres("scala/io/Source$$anonfun$1.class") }
    anonFun.foreach(fo => new ClassfileDepickler(fo).scalasig should ===(None))
  }

  it should "find type aliases" in withVFS { vfs =>
    new ClassfileDepickler(vfs.vres("scala/Predef.class")).getClasses("scala.Predef$").typeAliases.get(s"scala.Predef$$String") should ===(Some(
      RawType(
        ClassName(PackageName(List("scala")), "Predef$"),
        ClassName(PackageName(List("scala")), s"Predef$$String"),
        "scala.Predef.String",
        Public,
        " = java.lang.String"
      )
    ))
  }
}
