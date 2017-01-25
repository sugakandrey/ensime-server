// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import org.ensime.fixture._
import org.ensime.indexer._
import org.ensime.util.EnsimeSpec

/*
 * WARNING: these FQNs have been hand-crafted, we should really check
 * them against bytecode versions of the signatures to make sure we're
 * asserting on the right thing.
 */
class SymbolToFqnSpec extends EnsimeSpec
    with IsolatedRichPresentationCompilerFixture
    with RichPresentationCompilerTestUtils
    with ReallyRichPresentationCompilerFixture {
  import ReallyRichPresentationCompilerFixture._

  val original = EnsimeConfigFixture.DocsTestProject

  "SymbolToFqn" should "calculate the FQNs from Symbols.Symbol" in withPresCompiler { (config, cc) =>
    runForPositionInCompiledSource(
      config, cc,
      "package com.example",
      "import com.google.common.io.Files",
      "import com.google.common.base.Charsets",
      "import java.nio.channels.FileChannel._",
      "import java.i@java_pkg@o.File",
      "class Thing {",
      "  def main(): Unit = {",
      "    val o = Some(1)",
      "    val nums = o.m@0@ap(_ + 2)",
      "    val b:Boo@0.5@lean = false",
      "    nums.isDe@1@fined",
      "    val nums2 = o.flat@2@Map {i:Int => Some(i + 1)}",
      "    val x = Some(Some(1)).fla@3@tten",
      "    val y = Some(1).fo@4@ld(0) { ea => ea + 2 }",
      "    val z = Some(1).mkS@5@tring(\".\", \".\", \".\")",
      "    val zz = Some(1).mkS@6@tring",
      "    val zzz = Some(1).mkS@7@tring(\".\")",
      "    val q = Some(1).getOr@8@Else(2)",
      "    val r = Some(1).gro@9@uped(2)",
      "    val xx = List.emp@10@ty",
      "    val f = new Fi@10.5@le(\".\")",
      "    Files.mo@11@ve(f, f)",
      "    Files.asByte@12@Source(f)",
      "    Files.m@13@ap(f, MapMode.PRIVATE)",
      "    Files.m@14@ap(f, MapMode.PRIVATE, 5)",
      "    val a = Array@14.5@[Byte]()",
      "    Files.wri@15@te(a, f)",
      "    val aa = So@16@me(4)",
      "    val sss = \"abcd@17@efg\"",
      "    val ii = 123@18@456",
      "    val c = classOf[File].isInst@21@ance(fox)",
      "    scala.Predef.DummyIm@22@plicit",
      "    val hash = new java.util.Hash@23@Map[Int, Int]()",
      "    val entries = hash.entry@24@Set()",
      "    val ou = Some(1) +@29@+ List(1, 2)",
      "    val iPlus = 1 @32@+ 1",
      "  }",
      "  val fo@20@x = new File(\".\")",
      "  def ar@a1@r1(a: Array[Byte]): Array[Byte] = null",
      "  def ar@a2@r2(a: Array[Array[Byte]]): Array[Array[Byte]] = null",
      "  def ar@a3@r3(a: Array[Object]): Array[Object] = null",
      "  def ar@a4@r4(a: Array[Any]): Array[Any] = null",
      "  def ar@a5@r5(a: Array[Array[Any]]): Array[Array[Any]] = null",
      "}",
      "object `pac@po1@kage` { val f@package_field@oo: String = \"\" } ",
      "object Outer { class In@inner_class@ner { class Nes@nested_class@ted } }"
    ) { (p, label, cc) =>
        cc.askSymbolFqn(p).getOrElse { fail(label) } shouldBe {
          label match {
            case "0" => method(Seq("scala"), "Option", "map", "(Lscala/Function1;)Lscala/Option;")
            case "0.5" => ClassName.PrimitiveBoolean
            case "1" => method(Seq("scala"), "Option", "isDefined", "()Z")
            case "2" => method(Seq("scala"), "Option", "flatMap", "(Lscala/Function1;)Lscala/Option;")
            case "3" => method(Seq("scala"), "Option", "flatten", "(Lscala/Predef$$less$colon$less;)Lscala/Option;")
            case "4" => method(Seq("scala"), "Option", "fold", "(Lscala/Function0;Lscala/Function1;)Ljava/lang/Object;")

            case "5" => method(Seq("scala", "collection"), "TraversableOnce", "mkString", "(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;")
            case "6" => method(Seq("scala", "collection"), "TraversableOnce", "mkString", "()Ljava/lang/String;")
            case "7" => method(Seq("scala", "collection"), "TraversableOnce", "mkString", "(Ljava/lang/String;)Ljava/lang/String;")

            case "8" => method(Seq("scala"), "Option", "getOrElse", "(Lscala/Function0;)Ljava/lang/Object;")
            case "9" => method(Seq("scala", "collection"), "IterableLike", "grouped", "(I)Lscala/collection/Iterator;")
            case "10" => method(Seq("scala", "collection", "immutable"), "List$", "empty", "()Lscala/collection/immutable/List;")
            case "10.5" => clazz(Seq("java", "io"), "File")

            case "11" => method(Seq("com", "google", "common", "io"), "Files$", "move", "(Ljava/io/File;Ljava/io/File;)V")
            case "12" => method(Seq("com", "google", "common", "io"), "Files$", "asByteSource", "(Ljava/io/File;)Lcom/google/common/io/ByteSource;")
            case "13" => method(Seq("com", "google", "common", "io"), "Files$", "map", "(Ljava/io/File;Ljava/nio/channels/FileChannel$MapMode;)Ljava/nio/MappedByteBuffer;")
            case "14" => method(Seq("com", "google", "common", "io"), "Files$", "map", "(Ljava/io/File;Ljava/nio/channels/FileChannel$MapMode;J)Ljava/nio/MappedByteBuffer;")
            case "14.5" => clazz(Seq("scala"), "Array$")
            case "15" => method(Seq("com", "google", "common", "io"), "Files$", "write", "([BLjava/io/File;)V")
            case "16" => clazz(Seq("scala"), "Some$")

            case "17" => clazz(Seq("java", "lang"), "String")
            case "18" => ClassName.PrimitiveInt

            case "20" =>
              // not a typo, there really is a space at the end of val fields
              field(Seq("com", "example"), "Thing", "fox ")

            case "21" => method(Seq("java", "lang"), "Class", "isInstance", "(Ljava/lang/Object;)Z")
            case "22" => clazz(Seq("scala"), "Predef$DummyImplicit$")
            case "23" => clazz(Seq("java", "util"), "HashMap")

            case "24" => method(Seq("java", "util"), "HashMap", "entrySet", "()Ljava/util/Set;")

            case "29" =>
              method(Seq("scala", "collection"), "TraversableLike", "$plus$plus",
                "(Lscala/collection/GenTraversableOnce;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;")

            case "32" => method(Seq("scala"), "Int", "$plus", "(I)I")

            case "a1" => method(Seq("com", "example"), "Thing", "arr1", "([B)[B")
            case "a2" => method(Seq("com", "example"), "Thing", "arr2", "([[B)[[B")
            case "a3" => method(Seq("com", "example"), "Thing", "arr3", "([Ljava/lang/Object;)[Ljava/lang/Object;")
            case "a4" => method(Seq("com", "example"), "Thing", "arr4", "([Ljava/lang/Object;)[Ljava/lang/Object;")
            case "a5" => method(Seq("com", "example"), "Thing", "arr5", "([[Ljava/lang/Object;)[[Ljava/lang/Object;")

            case "po1" => clazz(Seq("com", "example"), "package$")

            case "java_pkg" => PackageName(List("java", "io"))

            case "package_field" =>
              // space not a typo
              field(Seq("com", "example"), "package$", "foo ")

            case "inner_class" => clazz(Seq("com", "example"), "Outer$Inner")
            case "nested_class" => clazz(Seq("com", "example"), "Outer$Inner$Nested")
          }
        }
      }
  }

  it should "calculate the FQNs from the typeSymbol of Types.Type" in withPresCompiler { (config, cc) =>
    runForPositionInCompiledSource(
      config, cc,
      "package com.example",
      "import com.google.common.io.Files",
      "import com.google.common.base.Charsets",
      "import java.nio.channels.FileChannel._",
      "import java.io.File",
      "class Thing {",
      "  def main(): Unit = {",
      "    val hash = new java.util.Hash@23@Map[Int, Int]()",
      "    val e@24@ntries = hash.entrySet()",
      "    val e@25@ntry = entries.iterator().next()",
      "  }",
      "}",
      "object `pac@po1@kage` { val f@package_field@oo: String = \"\"} ",
      "object Outer { class In@inner_class@ner { class Nes@nested_class@ted } }"
    ) { (p, label, cc) =>
        cc.askTypeFqn(p).getOrElse { fail(label) } shouldBe {
          label match {
            case "23" => clazz(Seq("java", "util"), "HashMap")
            case "24" => clazz(Seq("java", "util"), "Set")
            case "25" => clazz(Seq("java", "util"), s"Map$$Entry")

            case "po1" => clazz(Seq("com", "example"), "package$")

            case "package_field" => clazz(Seq("java", "lang"), "String")

            case "inner_class" => clazz(Seq("com", "example"), "Outer$Inner")
            case "nested_class" => clazz(Seq("com", "example"), "Outer$Inner$Nested")

          }
        }
      }
  }

  it should "calculate the FQN for imports" in withPresCompiler { (config, cc) =>
    import ReallyRichPresentationCompilerFixture._
    runForPositionInCompiledSource(config, cc,
      "package com.example",
      "import @1.0@java@1.1@.@1.2@io@1.3@.@1.4@File@1.5@.@1.6@separator@1.7@",
      "import java.io.{ @2.0@Bits => one@2.1@, @2.2@File => two@2.3@ }",
      "import java.io._@3.0@") { (p, label, cc) =>
        withClue(label) {
          cc.askTypeFqn(p).map(_.fqnString) shouldBe {
            label match {
              case "1.0" => Some("java")
              case "1.1" => Some("java")
              case "1.2" => Some("java.io")
              case "1.3" => Some("java.io")
              case "1.4" => Some("java.io.File$")
              case "1.5" => Some("java.io.File$")
              case "1.6" => Some("java.lang.String")
              case "1.7" => Some("java.lang.String")
              case "2.0" => Some("java.io.Bits$")
              case "2.1" => Some("java.io.Bits$")
              case "2.2" => Some("java.io.File$")
              case "2.3" => Some("java.io.File$")
              case "3.0" => None
            }
          }
        }
      }
  }

}
