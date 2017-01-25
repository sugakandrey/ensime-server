// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.server

import akka.util.ByteString
import org.ensime.api._
import org.ensime.sexp._
import org.ensime.swanky._

@deprecating("use SWANKY on WebSockets")
class SwankiProtocol extends FramedStringProtocol {
  import SwankyFormats._

  override def decode(bytes: ByteString): (Option[RpcRequestEnvelope], ByteString) = {
    tryReadString(bytes) match {
      case (Some(message), remainder) =>
        val parsedMessage = message.parseSexp.convertTo[RpcRequestEnvelope]
        (Some(parsedMessage), remainder)
      case (None, remainder) =>
        (None, remainder)
    }
  }

  override def encode(resp: RpcResponseEnvelope): ByteString = writeString(resp.toSexp.prettyPrint)

}
