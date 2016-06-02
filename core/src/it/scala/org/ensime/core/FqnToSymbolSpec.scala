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

}
