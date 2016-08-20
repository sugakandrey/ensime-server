// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import org.ensime.api.{ Note, NoteError }
import org.ensime.fixture._
import org.ensime.util.EnsimeSpec
import ReallyRichPresentationCompilerFixture.runForPositionInCompiledSource

// RichPresentationCompiler tests using the macros project. We can't
// define the macros inline because they have to be in a different
// compile run.
class RichPresentationCompilerMacroSpec extends EnsimeSpec
    with IsolatedRichPresentationCompilerFixture
    with RichPresentationCompilerTestUtils
    with ReallyRichPresentationCompilerFixture {

  val original = EnsimeConfigFixture.MacrosTestProject

  "RichPresentationCompiler when handling macros" should "not flag the entire file when the compiler crashes" in {
    // sadly a rogue println in Global.scala means this is always a
    // noisy test. I've tried setting Console.out here and in the
    // presentation compiler thread but it's quite elusive.
    withPresCompiler { (config, cc) =>
      runForPositionInCompiledSource(
        config, cc,
        "import org.ensime.testing.macros.bad._",
        "object Foo {",
        "  val foo: String = BadMacro.ba@bad@d",
        "}"
      ) { (p, label, cc) =>
          val handler = cc.reporter.asInstanceOf[TestReporter].handler

          val notes = eventually {
            val notes = handler.notes
            notes should not be 'empty
            notes
          }
          notes.head should matchPattern {
            case Note(_, msg, NoteError, 0, 0, 1, 1) if msg.contains("compiler crashed") =>
          }

        }
    }
  }

}
