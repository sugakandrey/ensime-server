// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.swanky

import org.ensime.api._
import org.ensime.sexp._
import org.ensime.sexp.formats._
import org.ensime.util.ensimefile._
import shapeless._

private object SwankyConversions
    extends BasicFormats
    with StandardFormats
    with CollectionFormats
    with SymbolAltFormat
    with OptionAltFormat
    with FamilyFormats {

  def dashify(field: String): String =
    field.replaceAll("([A-Z])", "-$1").toLowerCase.replaceAll("^-", "")

  implicit override def coproductHint[T: Typeable]: CoproductHint[T] = new NestedCoproductHint[T] {
    override def field(orig: String): SexpSymbol = SexpSymbol(":ensime-api-" + dashify(orig))
  }

  implicit override def productHint[T]: ProductHint[T] = new BasicProductHint[T] {
    override def field[Key <: Symbol](key: Key): SexpSymbol = SexpSymbol(":" + dashify(key.name))
  }

  implicit object DebugThreadIdFormat extends SexpFormat[DebugThreadId] {
    override def read(s: Sexp): DebugThreadId = DebugThreadId(LongFormat.read(s))
    override def write(t: DebugThreadId): Sexp = LongFormat.write(t.id)
  }

  implicit object DebugObjectIdFormat extends SexpFormat[DebugObjectId] {
    override def read(s: Sexp): DebugObjectId = DebugObjectId(LongFormat.read(s))
    override def write(t: DebugObjectId): Sexp = LongFormat.write(t.id)
  }

  implicit object EnsimeFileFormat extends SexpFormat[EnsimeFile] {
    def write(ef: EnsimeFile): Sexp = ef match {
      case RawFile(path) => SexpString(path.toString)
      case a: ArchiveFile => SexpString(a.uri.toASCIIString)
    }
    def read(sexp: Sexp): EnsimeFile = sexp match {
      case SexpString(uri) => EnsimeFile(uri)
      case got => deserializationError(got)
    }
  }

  implicit val RpcRequestEnvelopeFormat: SexpFormat[RpcRequestEnvelope] = cachedImplicit
  implicit val RpcResponseEnvelopeFormat: SexpFormat[RpcResponseEnvelope] = cachedImplicit

}

object SwankyFormats {
  implicit val RpcRequestEnvelopeFormat: SexpFormat[RpcRequestEnvelope] =
    SwankyConversions.RpcRequestEnvelopeFormat
  implicit val RpcResponseEnvelopeFormat: SexpFormat[RpcResponseEnvelope] =
    SwankyConversions.RpcResponseEnvelopeFormat
}
