// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import org.ensime.fixture._
import org.ensime.indexer.FullyQualifiedName
import org.ensime.util.EnsimeSpec

class FqnToSymbolSpec extends EnsimeSpec
    with IsolatedRichPresentationCompilerFixture
    with RichPresentationCompilerTestUtils
    with ReallyRichPresentationCompilerFixture {

  import ReallyRichPresentationCompilerFixture._

  val original = EnsimeConfigFixture.EmptyTestProject

  "FqnToSymbol" should "convert scala type names into Symbols" in withPresCompiler { (config, cc) =>
    runForPositionInCompiledSource(
      config, cc,
      "package com.example",
      "object @1@A { ",
      "   val @1.1@x: Int = 1",
      "   class  @1.2@Y {} ",
      "   object @1.3@X {} ",
      "}"
    ) { (p, label, cc) =>
        cc.askTypeSymbolAt(p).get.fullName shouldBe {
          label match {
            case "1" => cc.askSymbolByScalaName("com.example.A")
            case "1.1" => cc.askSymbolByScalaName("scala.Int")
            case "1.2" => cc.askSymbolByScalaName("com.example.A.Y")
            case "1.3" => cc.askSymbolByScalaName("com.example.A.X")
          }
        }.get.fullName
      }
  }

  it should "convert scala member names into Symbols" in withPresCompiler { (config, cc) =>
    runForPositionInCompiledSource(
      config, cc,
      "package com.example",
      "object A { ",
      "   val @x@x: Int = 1",
      "}"
    ) { (p, label, cc) =>
        cc.askSymbolAt(p).get.fullName shouldBe {
          label match {
            case "x" => cc.askSymbolByScalaName("com.example.A.x")
          }
        }.get.fullName
      }
  }

  it should "convert class FQNs to symbols" in withPresCompiler { (config, cc) =>
    def verify(fqn: FullyQualifiedName, expected: String) = withClue(s"$fqn") {
      cc.askSymbolByFqn(fqn).get.fullName shouldBe expected
    }

    verify(clazz(Seq("java", "lang"), "String"), "java.lang.String")
    verify(clazz(Seq("scala"), "Some$"), "scala.Some")

    runForPositionInCompiledSource(
      config, cc,
      "package com.example",
      "object A {}",
      "class A { ",
      "  object F@foo@oo {",
      "    class Qux { class Te@test@st }",
      "    object Qux { }",
      "}",
      "  class Foo",
      "  class B@bar@ar {}",
      "}"
    ) { (p, label, cc) =>
        cc.askSymbolByFqn(cc.askSymbolFqn(p).get).get shouldBe {
          label match {
            case "test" => cc.askSymbolByScalaName("com.example.A#Foo.Qux#Test")
            case "foo" => cc.askSymbolByScalaName("com.example.A#Foo")
            case "bar" => cc.askSymbolByScalaName("com.example.A#Bar")
          }
        }.get
      }
  }

  it should "convert field FQNs to symbols" in withPresCompiler { (config, cc) =>
    def verify(fqn: FullyQualifiedName, expected: String) = withClue(s"$fqn") {
      cc.askSymbolByFqn(fqn).get.fullName shouldBe expected
    }

    verify(field(Seq("java", "awt"), "Point", "x"), "java.awt.Point.x")

    // static field
    verify(field(Seq("java", "io"), "File", "separator"), "java.io.File.separator")

  }

  it should "convert method FQNs to symbols" in withPresCompiler { (config, cc) =>
    def verify(fqn: FullyQualifiedName, expected: String) = withClue(s"$fqn") {
      cc.askSymbolByFqn(fqn).get.fullName shouldBe expected
    }

    verify(
      method(Seq("java", "lang"), "String", "startsWith", "(Ljava/lang/String;)Z"),
      "java.lang.String.startsWith"
    )

    verify(
      // static method
      method(Seq("java", "lang"), "String", "valueOf", "(Ljava/lang/Object;)Ljava/lang/String;"),
      "java.lang.String.valueOf"
    )
  }

  it should "resolve overloaded symbols correctly" in withPresCompiler { (config, cc) =>
    runForPositionInCompiledSource(
      config, cc,
      "package com.example",
      "class Foo(val test: Boolean) {",
      "  object bar {",
      "     class B@baz@az",
      "  }",
      "  def bar(i: Int): Unit = ???",
      "  object test {",
      "    def q@qux@ux(): Unit = ???",
      "  }",
      "  def test(s: String): Unit = ???",
      "}"
    ) { (p, label, cc) =>
        cc.askSymbolByFqn(cc.askSymbolFqn(p).get).get shouldBe {
          label match {
            case "baz" =>
              cc.askSymbolByScalaName("com.example.Foo#bar.Baz")
            case "qux" =>
              cc.askSymbolByScalaName("com.example.Foo#test.qux")
          }
        }.get
      }
  }

  it should "convert class FQNs with special characters to symbols" in withPresCompiler { (config, cc) =>
    runForPositionInCompiledSource(
      config, cc,
      "package com.example",
      "package <#>.>>=",
      "class <@outer_symbolic@:< {",
      "  class =@inner_symbolic@:=",
      "}",
      "object ~@sym_object@~~ {",
      "  class B@bar@ar",
      "}"
    ) { (p, label, cc) =>
        cc.askSymbolByFqn(cc.askSymbolFqn(p).get).get shouldBe {
          label match {
            case "outer_symbolic" =>
              cc.askSymbolByScalaName("com.example.$less$hash$greater.$greater$greater$eq.$less$colon$less")
            case "inner_symbolic" =>
              cc.askSymbolByScalaName("com.example.$less$hash$greater.$greater$greater$eq.$less$colon$less#$eq$colon$eq")
            case "sym_object" =>
              cc.askSymbolByScalaName("com.example.$less$hash$greater.$greater$greater$eq.$tilde$tilde$tilde")
            case "bar" =>
              cc.askSymbolByScalaName("com.example.$less$hash$greater.$greater$greater$eq.$tilde$tilde$tilde.Bar")
          }
        }.get
      }
  }

}
