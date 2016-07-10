// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.swanky

import org.ensime.api._
import org.ensime.sexp._
import org.ensime.sexp.formats._
import shapeless._

private object SwankyConversions
    extends BasicFormats
    with StandardFormats
    with CollectionFormats
    with SymbolAltFormat
    with OptionAltFormat
    with FamilyFormats {

  implicit val RpcRequestEnvelopeFormat: SexpFormat[RpcRequestEnvelope] = cachedImplicit
  implicit val RpcResponseEnvelopeFormat: SexpFormat[RpcResponseEnvelope] = cachedImplicit

}

object SwankyFormats {
  implicit val RpcRequestEnvelopeFormat: SexpFormat[RpcRequestEnvelope] =
    SwankyConversions.RpcRequestEnvelopeFormat
  implicit val RpcResponseEnvelopeFormat: SexpFormat[RpcResponseEnvelope] =
    SwankyConversions.RpcResponseEnvelopeFormat
}
