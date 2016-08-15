// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.indexer

import akka.event.slf4j.SLF4JLogging
import org.ensime.fixture.IsolatedEnsimeVFSFixture
import org.ensime.util.EnsimeSpec
import org.ensime.vfs._

class ClassfileIndexerSpec extends EnsimeSpec with IsolatedEnsimeVFSFixture {

  val indexer = new ClassfileIndexer with SLF4JLogging {}
  import indexer._

  "ClassfileIndexer" should "support Java 6 class files" in withVFS { implicit vfs =>
    val clazz = indexClassfile(vfs.vres("jdk6/Test.class"))
    clazz.name shouldBe ClassName(PackageName(Nil), "Test")
    clazz.generics shouldBe None
    clazz.superClass shouldBe Some(ClassName(PackageName(List("java", "lang")), "Object"))
    clazz.interfaces shouldBe Nil
    clazz.access shouldBe Default
    clazz.deprecated shouldBe false
    clazz.fields shouldBe List()
    clazz.methods shouldBe List(
      RawMethod(
        name = MethodName(
          ClassName(PackageName(Nil), "Test"),
          "main",
          Descriptor(List(ArrayDescriptor(ClassName(PackageName(List("java", "lang")), "String"))), ClassName(PackageName(Nil), "void"))
        ),
        access = Public,
        generics = None,
        line = Some(4),
        Set(
          ClassName(PackageName(Nil), "void"),
          FieldName(ClassName(PackageName(List("java", "lang")), "System"), "out"),
          ClassName(PackageName(List("java", "io")), "PrintStream"),
          ClassName(PackageName(List("java", "lang")), "String"),
          MethodName(
            ClassName(PackageName(List("java", "io")), "PrintStream"),
            "print",
            Descriptor(List(ClassName(PackageName(List("java", "lang")), "String")), ClassName(PackageName(Nil), "void"))
          )
        )
      )
    )
    clazz.source shouldBe RawSource(Some("Test.java"), Some(1))
    val refs = clazz.internalRefs ++ clazz.methods.flatMap(_.internalRefs) ++ clazz.fields.flatMap(_.internalRefs)

    refs shouldBe Set(
      ClassName(PackageName(Nil), "void"),
      ClassName(PackageName(List("java", "lang")), "Object"),
      FieldName(ClassName(PackageName(List("java", "lang")), "System"), "out"),
      ClassName(PackageName(List("java", "lang")), "Object"),
      ClassName(PackageName(List("java", "io")), "PrintStream"),
      MethodName(
        ClassName(PackageName(List("java", "io")), "PrintStream"),
        "print",
        Descriptor(List(ClassName(PackageName(List("java", "lang")), "String")), ClassName(PackageName(Nil), "void"))
      ),
      ClassName(PackageName(List("java", "lang")), "String")
    )
  }

  it should "support Java 8 class files" in withVFS { implicit vfs =>
    indexClassfile(vfs.vres("jdk8/Test.class"))
    indexClassfile(vfs.vres("jdk8/MyAnnotation.class"))
    indexClassfile(vfs.vres("jdk8/Test$InnerClassWithCtorParam.class"))
  }

  it should "support typical J2SE classes" in withVFS { implicit vfs =>
    val clazz = indexClassfile(vfs.vres("java/lang/String.class"))
    clazz.access shouldBe Public
    clazz.name shouldBe ClassName(PackageName(List("java", "lang")), "String")
  }

  it should "support typical Scala classes" in withVFS { implicit vfs =>
    val clazz = indexClassfile(vfs.vres("scala/collection/immutable/List.class"))
    clazz.name shouldBe ClassName(PackageName(List("scala", "collection", "immutable")), "List")
  }

  it should "support typical Scala nested classes " in withVFS { implicit vfs =>
    val clazz = indexClassfile(vfs.vres("scala/collection/immutable/List$.class"))
    clazz.name shouldBe ClassName(PackageName(List("scala", "collection", "immutable")), "List$")
  }

  it should "support method overloading" in withVFS { implicit vfs =>
    val clazz = indexClassfile(vfs.vres("java/nio/channels/FileChannel.class"))
    val methods = clazz.methods.filter { ref => ref.name.fqnString.startsWith("java.nio.channels.FileChannel.write") }

    methods.map(_.name.fqnString) should contain theSameElementsAs List(
      "java.nio.channels.FileChannel.write(Ljava/nio/ByteBuffer;)I",
      "java.nio.channels.FileChannel.write([Ljava/nio/ByteBuffer;II)J",
      "java.nio.channels.FileChannel.write([Ljava/nio/ByteBuffer;)J",
      "java.nio.channels.FileChannel.write(Ljava/nio/ByteBuffer;J)I"
    )
  }

}
