// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.jerky

import spray.json._
import fommil.sjs._
import shapeless._

import org.ensime.api._

import org.ensime.util.file._

private object JerkyConversions extends DefaultJsonProtocol with FamilyFormats {
  // This part of the code is brought to you by the words "accidental"
  // and "complexity".
  //
  // Lack of definition in scalac's implicit resolution rules means
  // that we have to redefine some things here.
  implicit override def eitherFormat[A: JsonFormat, B: JsonFormat]: JsonFormat[Either[A, B]] = super.eitherFormat[A, B]
  // Note that its not possible to override an object in scala, so we
  // just define a new one that wins the race.
  implicit val symbolFormat: SymbolJsonFormat.type = SymbolJsonFormat

  // move to somewhere more general
  implicit object FileFormat extends JsonFormat[File] {
    def read(j: JsValue): File = j match {
      case JsString(path) => File(path)
      case other => unexpectedJson[File](other)
    }
    def write(f: File): JsValue = JsString(f.getPath)
  }

  // keeps the JSON a little bit cleaner
  implicit object DebugThreadIdFormat extends JsonFormat[DebugThreadId] {
    def read(j: JsValue): DebugThreadId = j match {
      case JsNumber(id) => new DebugThreadId(id.longValue)
      case other => unexpectedJson[DebugThreadId](other)
    }
    def write(dtid: DebugThreadId): JsValue = JsNumber(dtid.id)
  }

  // some of the case classes use the keyword `type`, so we need a better default
  override implicit def coproductHint[T: Typeable]: CoproductHint[T] = new FlatCoproductHint[T]("typehint")

  implicit val RpcRequestEnvelopeFormat: RootJsonFormat[RpcRequestEnvelope] = cachedImplicit
  implicit val RpcResponseEnvelopeFormat: RootJsonFormat[RpcResponseEnvelope] = cachedImplicit

}

object JerkyFormats {
  implicit val RpcRequestEnvelopeFormat: RootJsonFormat[RpcRequestEnvelope] =
    JerkyConversions.RpcRequestEnvelopeFormat
  implicit val RpcResponseEnvelopeFormat: RootJsonFormat[RpcResponseEnvelope] =
    JerkyConversions.RpcResponseEnvelopeFormat
}
