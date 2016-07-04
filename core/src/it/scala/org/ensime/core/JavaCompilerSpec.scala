// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import java.io.File

import org.ensime.api, api.{ BasicTypeInfo => _, _ }
import org.ensime.fixture._
import org.ensime.indexer.SearchServiceTestUtils._
import org.ensime.model.BasicTypeInfo
import org.ensime.util.EnsimeSpec
import org.ensime.indexer._

class JavaCompilerSpec extends EnsimeSpec
    with IsolatedJavaCompilerFixture {

  val original = EnsimeConfigFixture.SimpleTestProject

  "JavaCompiler" should "generate compilation notes" in {
    withJavaCompiler { (_, config, cc, store, search) =>
      runForPositionInCompiledSource(config, cc,
        "import java.io.File;",
        "class Test1 {",
        "  ksjdfkdjsf @1@",
        "}") { (sf, p, label, cc) =>
        }
      store.notes should not be empty
    }
  }

  it should "find type at point" in {
    withJavaCompiler { (_, config, cc, store, search) =>
      runForPositionInCompiledSource(config, cc,
        "import java.io.File;",
        "class Tes@0@t1 {",
        "  private void main() {",
        "    int fo@1@o = 1;",
        "    System.out.pri@2@ntln(fo@3@o);",
        "  }",
        "}") { (sf, offset, label, cc) =>
          val info = cc.askTypeAtPoint(sf, offset).get
          label match {
            case "0" => info.name shouldBe "Test1"
            case "1" => info.name shouldBe "int"
            case "2" => info shouldBe ArrowTypeInfo(
              "void (int)", "void (int)",
              BasicTypeInfo("void", DeclaredAs.Class, "void"),
              ParamSectionInfo(
                ("arg0" -> BasicTypeInfo(
                  "int",
                  DeclaredAs.Class,
                  "int"
                )) :: Nil,
                isImplicit = false
              ) :: Nil, Nil
            )
            case "3" => info.name shouldBe "int"
          }
        }
    }
  }

  it should "link symbols to their source positions" in {
    withJavaCompiler { (_, config, cc, store, _) =>
      val test1 = SourceFileInfo(new File(config.rootDir, "testing/simple/src/main/java/org/example/Test1.java"))
      val test2 = SourceFileInfo(new File(config.rootDir, "testing/simple/src/main/java/org/example/Test2.java"))

      cc.askLinkPos(ClassName(PackageName(List("org", "example")), "Test2"), test2) should matchPattern { case Some(OffsetSourcePosition(f, 22)) => }
      cc.askLinkPos(ClassName(PackageName(List("org", "example")), "Foo"), test2) should matchPattern { case None => }
      cc.askLinkPos(ClassName(PackageName(List("org", "example")), "Test2.Bar"), test2) should matchPattern { case Some(OffsetSourcePosition(f, 260)) => }
      //    cc.askLinkPos(JavaFqn("org.example", "Test2", Some("compute()")), test2) should matchPattern { case Some(OffsetSourcePosition(f, 58)) => }

    }
  }

  it should "find symbol at point" in withJavaCompiler { (_, config, cc, store, search) =>
    refresh()(search)

    runForPositionInCompiledSource(config, cc,
      "package org.example;",
      "import java.io.File;",
      "class Test1 {",
      "  private class Foo { public Foo() {} }",
      "  public static final int CONST = 2;",
      "  private void main(String[] args) {",
      "    int foo = 1;",
      "    System.out.println(ar@1@gs);",
      "    System.out.pr@3@intln(new Fo@2@o());",
      "    System.out.println(new Fi@4@le(\".\"));",
      "    System.out.println(Tes@5@t2.com@6@pute());",
      "    System.out.println(comp@7@ute(2, 3));",
      "    System.out.println(CO@8@NST);",
      "    System.out.println(@11@fo@0@o@12@);",
      "    int k = 2;",
      "    System.out.println( @13@k@14@ );",
      "  }",
      "  private static int compute(int a, int b) {",
      "    return a + b;",
      "  }",
      "  private static String hello(D@9@ay day) {",
      "    if (day == Day.MO@10@N) return \"monday\";",
      "    return \"tues\";",
      "  }",
      "  public enum Day { MON, TUES }",
      "}") { (sf, offset, label, cc) =>
        val info = cc.askSymbolAtPoint(sf, offset).get
        label match {
          case "0" | "11" | "12" =>
            info.name shouldBe "foo"
            info.localName shouldBe "foo"
            info.`type`.name shouldBe "int"
            info.`type` shouldBe a[api.BasicTypeInfo]
            info.declPos should matchPattern { case Some(OffsetSourcePosition(f, 174)) if f.getName == "Test1.java" => }
          case "1" =>
            info.name shouldBe "args"
            info.localName shouldBe "args"
            info.`type`.name shouldBe "java.lang.String[]"
            info.`type` shouldBe a[api.BasicTypeInfo]
            info.declPos should matchPattern { case Some(OffsetSourcePosition(f, 153)) if f.getName == "Test1.java" => }
          case "2" =>
            info.name shouldBe "org.example.Test1.Foo"
            info.localName shouldBe "Foo"
            info.`type`.name shouldBe "org.example.Test1.Foo"
            info.`type` shouldBe a[api.BasicTypeInfo]
            info.declPos should matchPattern { case Some(OffsetSourcePosition(f, 58)) if f.getName == "Test1.java" => }
          case "3" =>
            info.name shouldBe "java.io.PrintStream.println(java.lang.Object)"
            info.localName shouldBe "println"
            info.`type` shouldBe ArrowTypeInfo(
              "void println(Object arg0)", "void println(java.lang.Object arg0)",
              BasicTypeInfo("void", DeclaredAs.Class, "void"),
              ParamSectionInfo(
                ("arg0" -> BasicTypeInfo(
                  "java.lang.Object",
                  DeclaredAs.Class,
                  "java.lang.Object"
                )) :: Nil,
                isImplicit = false
              ) :: Nil, Nil
            )
          case "4" =>
            info.name shouldBe "java.io.File"
            info.localName shouldBe "File"
            info.`type`.name shouldBe "java.io.File"
            info.`type` shouldBe a[api.BasicTypeInfo]
          case "5" =>
            info.name shouldBe "org.example.Test2"
            info.localName shouldBe "Test2"
            info.`type`.name shouldBe "org.example.Test2"
            info.`type` shouldBe a[api.BasicTypeInfo]
            info.declPos should matchPattern { case Some(OffsetSourcePosition(f, 22)) if f.getName == "Test2.java" => }
          case "6" =>
            info.name shouldBe "org.example.Test2.compute()"
            info.localName shouldBe "compute"
            info.`type` shouldBe ArrowTypeInfo(
              "int compute()", "int compute()",
              BasicTypeInfo("int", DeclaredAs.Class, "int"),
              ParamSectionInfo(
                Nil,
                isImplicit = false
              ) :: Nil, Nil
            )
            info.declPos should matchPattern {
              case Some(LineSourcePosition(f, 8)) if f.getName == "Test2.java" =>
              case Some(OffsetSourcePosition(f, 48)) if f.getName == "Test2.java" =>
            }
          case "7" =>
            {}
            info.name shouldBe "org.example.Test1.compute(int,int)"
            info.localName shouldBe "compute"
            info.`type` shouldBe ArrowTypeInfo(
              "int compute(int arg0, int arg1)", "int compute(int arg0, int arg1)",
              BasicTypeInfo("int", DeclaredAs.Class, "int"),
              ParamSectionInfo(
                ("arg0" -> BasicTypeInfo(
                  "int",
                  DeclaredAs.Class,
                  "int"
                )) ::
                  ("arg1" -> BasicTypeInfo(
                    "int",
                    DeclaredAs.Class,
                    "int"
                  )) :: Nil,
                isImplicit = false
              ) :: Nil, Nil
            )
            // "private static int compute(int a, int b)"
            info.declPos should matchPattern { case Some(OffsetSourcePosition(f, 481)) if f.getName == "Test1.java" => }
          case "8" =>
            info.name shouldBe "org.example.Test1.CONST"
            info.localName shouldBe "CONST"
            info.`type`.name shouldBe "int"
            info.`type` shouldBe a[api.BasicTypeInfo]
            info.declPos should matchPattern { case Some(OffsetSourcePosition(f, 98)) if f.getName == "Test1.java" => }
          case "9" =>
            info.name shouldBe "org.example.Test1.Day"
            info.localName shouldBe "Day"
            info.`type`.name shouldBe "org.example.Test1.Day"
            info.`type` shouldBe a[api.BasicTypeInfo]
            info.declPos should matchPattern { case Some(OffsetSourcePosition(f, 653)) if f.getName == "Test1.java" => }
          case "10" =>
            info.name shouldBe "org.example.Test1.Day.MON"
            info.localName shouldBe "MON"
            info.`type`.name shouldBe "org.example.Test1.Day"
            info.`type` shouldBe a[api.BasicTypeInfo]
            // Don't specify offset pos here as Java 6 seems to have a problem locating enums
            info.declPos should matchPattern { case Some(OffsetSourcePosition(f, i: Int)) if f.getName == "Test1.java" => }
          case "13" | "14" =>
            info.name shouldBe "k"
            info.`type`.name shouldBe "int"
            info.`type` shouldBe a[api.BasicTypeInfo]
        }
      }
  }

  it should "find completions at point" in {
    withJavaCompiler { (_, config, cc, store, search) =>
      runForPositionInCompiledSource(config, cc,
        "import java.io.File;",
        "import java.lang.Str@5@;",
        "import java.util.Map.E@6@;",
        "import java.util.Map.E@7@blablabla;",
        "class Test1 {",
        "  public static final int MAX_VALUE = 10;",
        "  public static class TestInner {",
        "    public int maxValue = 10;",
        "    private int privateValue = 10;",
        "    private void main(String foo, String bar) {",
        "      File f = new File(\".\");",
        "      f.toSt@0@;",
        "      System.out.println(f.toStr@1@);",
        "      System.out.println((f).toStr@2@);",
        "      System.out.println(f.toString().substr@3@);",
        "      f.@4@;",
        "      new Fi@8@",
        "      System.out.println(fo@9@ + bar);",
        "      System.out.println(maxV@10@);",
        "      System.out.println(MAX_@11@);",
        "      System.out.println(new Inte@12@);",
        "      int testinner = 5;",
        "      System.out.println(f.toStr@1@);",
        "      System.out.@14@",
        "      privateVa@15@",
        "      int hashCode = \"Blah\".has@16@;",
        "    }",
        "  }",
        "}") { (sf, offset, label, cc) =>
          val info = cc.askCompletionsAtPoint(sf, offset, 0, false)
          label match {
            case "0" => forAtLeast(1, info.completions)(_.name shouldBe "toString")
            case "1" => forAtLeast(1, info.completions)(_.name shouldBe "toString")
            case "2" => forAtLeast(1, info.completions)(_.name shouldBe "toString")
            case "3" => forAtLeast(1, info.completions)(_.name shouldBe "substring")
            case "4" =>
              forAtLeast(1, info.completions)(_.name shouldBe "createTempFile")
              forAtLeast(1, info.completions)(_.name shouldBe "wait")
            case "5" => forAtLeast(1, info.completions)(_.name shouldBe "String")
            case "6" => forAtLeast(1, info.completions)(_.name shouldBe "Entry")
            case "7" => forAtLeast(1, info.completions)(_.name shouldBe "Entry")
            case "8" => forAtLeast(1, info.completions)(_.name shouldBe "File")
            case "9" => forAtLeast(1, info.completions)(_.name shouldBe "foo")
            case "10" => forAtLeast(1, info.completions)(_.name shouldBe "maxValue")
            case "11" => forAtLeast(1, info.completions)(_.name shouldBe "MAX_VALUE")
            case "12" => forAtLeast(1, info.completions)(_.name shouldBe "Integer")

            case "13" =>
              // exact matches should be preferred
              info.completions(0).name shouldBe "TestInner"
              info.completions(1).name shouldBe "testinner"

            case "14" => forAtLeast(1, info.completions)(_.name shouldBe "println")
            case "15" => forAtLeast(1, info.completions)(_.name shouldBe "privateValue")
            case "16" => forAll(info.completions)(_.name shouldNot be("hash"))
          }
        }
    }
  }

  it should "find completion at beginning of file" in {
    withJavaCompiler { (_, config, cc, store, search) =>
      runForPositionInCompiledSource(config, cc, "Sys@0@") { (sf, offset, label, cc) =>
        val info = cc.askCompletionsAtPoint(sf, offset, 0, false)
        label match {
          case "0" => forAtLeast(1, info.completions)(_.name shouldBe "System")
        }
      }
    }
  }

  it should "find doc sig at point" in withJavaCompiler { (_, config, cc, store, search) =>
    runForPositionInCompiledSource(config, cc,
      "import java.io.Fi@5@le;",
      "class Test1 {",
      "  private void main() {",
      "    File f = new F@1@ile(\".\")",
      "    System.out.println(f.toStr@2@ing());",
      "    File.create@3@TempFile(\"bla\", \"foo\");",
      "    File.create@4@TempFile(\"bla\", \"foo\", f);",
      "    System.out.println(\"bla\".ind@6@exOf(\"b\"));",
      "    System.out.println(\"bla\".index@7@Of(\"b\", 1));",
      "    System.out.println(\"bla\".index@8@Of(1));",
      "  }",
      "}") { (sf, offset, label, cc) =>
        val sig = cc.askDocSignatureAtPoint(sf, offset).get.java
        label match {
          case "0" => sig.fqn shouldBe DocFqn("", "Test1")
          case "1" => sig.fqn shouldBe DocFqn("java.io", "File")
          case "2" => sig shouldBe DocSig(DocFqn("java.io", "File"), Some("toString()"));
          case "3" => sig shouldBe DocSig(DocFqn("java.io", "File"), Some("createTempFile(java.lang.String,java.lang.String)"));
          case "4" => sig shouldBe DocSig(DocFqn("java.io", "File"), Some("createTempFile(java.lang.String,java.lang.String,java.io.File)"));
          case "5" => sig.fqn shouldBe DocFqn("java.io", "File")
          case "6" => sig shouldBe DocSig(DocFqn("java.lang", "String"), Some("indexOf(java.lang.String)"));
          case "7" => sig shouldBe DocSig(DocFqn("java.lang", "String"), Some("indexOf(java.lang.String,int)"));
          case "8" => sig shouldBe DocSig(DocFqn("java.lang", "String"), Some("indexOf(int)"));
        }
      }
  }

  it should "support Java 7 syntax features" in {
    withJavaCompiler { (_, config, cc, store, search) =>
      runForPositionInCompiledSource(config, cc, """
        | import java.io.File;
        | import java.util.HashMap;
        | import java.util.Map;
        | import java.io.FileInputStream;
        | import java.io.DataOutputStream;
        | import java.io.IOException;
        | import java.lang.ArrayIndexOutOfBoundsException;
        | class Java7Test {
        |   private void main() {
        |     Map<Long, String> aMap = new HashMap<>(); // diamond operator
        |     aM@0@ap.put(1L, "ONE");
        |     String one = aMap.get(1L);
        |     switch(o@1@ne) { // switch over strings
        |       case "O@2@NE":
        |         break;
        |       default:
        |        break
        |     }
        |     int m@3@illion = 1_000_000; //numeric literals with underscores
        |     try(FileOutputStream f@4@os = new FileOutputStream("movies.txt"); //resource mamagement
        |       DataOutputStream dos = new DataOutputStream(fos)) {
        |     } catch(ArrayIndexOutOfBoundsException | IOException e@5@x) { } //multi catch block
        |   }
        |}""".stripMargin) { (sf, offset, label, cc) =>
        val info = cc.askTypeAtPoint(sf, offset).get
        label match {
          case "0" => info.name shouldBe "java.util.Map<java.lang.Long,java.lang.String>"
          case "1" => info.name shouldBe "java.lang.String"
          case "2" => info.name shouldBe "java.lang.String"
          case "3" => info.name shouldBe "int"
          case "4" => info.name shouldBe "FileOutputStream"
          case "5" => info.name shouldBe "java.lang.Exception"
        }
      }
    }
  }
}
