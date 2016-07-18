// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.sexp.formats

import org.ensime.sexp._
import org.ensime.util.EnsimeSpec
import org.scalactic.source.Position

trait FormatSpec extends EnsimeSpec {
  def assertFormat[T: SexpFormat](start: T, expect: Sexp)(implicit p: Position): Unit = {
    val sexp = start.toSexp
    assert(sexp === expect, s"${sexp.compactPrint} was not ${expect.compactPrint}")
    expect.convertTo[T] should be(start)
  }
}
