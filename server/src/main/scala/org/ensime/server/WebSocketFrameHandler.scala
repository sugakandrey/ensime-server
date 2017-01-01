// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.server

import io.netty.channel.{ Channel, ChannelHandlerContext, SimpleChannelInboundHandler }
import io.netty.handler.codec.http.websocketx.WebSocketServerProtocolHandler.HandshakeComplete
import io.netty.handler.codec.http.websocketx.{ TextWebSocketFrame, WebSocketFrame }
import io.netty.util.AttributeKey
import org.slf4j.LoggerFactory

import org.ensime.api._
import org.ensime.server.WebServer._

class WebSocketFrameHandler(
    hookHandlers: HookHandlers
) extends SimpleChannelInboundHandler[WebSocketFrame] {

  val log = LoggerFactory.getLogger(this.getClass)

  val inHandlerKey: AttributeKey[String => Unit] =
    AttributeKey.valueOf(classOf[String => Unit], "INHANDLER");

  val outHandlerKey: AttributeKey[OutgoingHandler] =
    AttributeKey.valueOf(classOf[OutgoingHandler], "OUTHANDLER");

  private def setInHandler(ctx: ChannelHandlerContext, inHandler: String => Unit): Unit =
    ctx.channel().attr(inHandlerKey).set(inHandler)

  private def getInHandler(ctx: ChannelHandlerContext): String => Unit =
    ctx.channel().attr(inHandlerKey).get()

  private def setOutHandler(ctx: ChannelHandlerContext, outHandler: OutgoingHandler): Unit =
    ctx.channel().attr(outHandlerKey).set(outHandler)

  private def getOutHandler(ctx: ChannelHandlerContext): OutgoingHandler =
    ctx.channel().attr(outHandlerKey).get()

  private def encoderFor(subprotocol: String): SubprotocolEncoder =
    subprotocol match {
      case "jerky" => JerkySubprotocolEncoder
      case "swanky" => SwankySubprotocolEncoder
    }

  private def encodedOutHandler(ch: Channel, encoder: SubprotocolEncoder): OutgoingHandler = {
    rpcResp =>
      val response = encoder.writeFrame(rpcResp)
      ch.writeAndFlush(new TextWebSocketFrame(response))
  }

  private def encodedInHandler(inHandler: IncomingHandler, encoder: SubprotocolEncoder): String => Unit = {
    frameText =>
      val rpcReq = encoder.readFrame(frameText)
      inHandler(rpcReq)
  }

  override protected def userEventTriggered(ctx: ChannelHandlerContext, evt: Object): Unit =
    if (evt.isInstanceOf[HandshakeComplete]) {
      val serverHandshakeComplete = evt.asInstanceOf[HandshakeComplete];
      val subprotocol = serverHandshakeComplete.selectedSubprotocol
      val encoder = encoderFor(subprotocol)
      val outHandler = encodedOutHandler(ctx.channel(), encoder)
      val inHandler = encodedInHandler(hookHandlers(outHandler), encoder)
      setInHandler(ctx, inHandler)
      setOutHandler(ctx, outHandler)
      log.info("Handlers ready")
    }

  override protected def channelRead0(ctx: ChannelHandlerContext, frame: WebSocketFrame): Unit = {
    frame match {
      case txtFrame: TextWebSocketFrame =>
        getInHandler(ctx)(txtFrame.text)
      case _ =>
        val message = "Unsupported frame type: " + frame.getClass().getName()
        throw new UnsupportedOperationException(message)
    }
  }

  override protected def exceptionCaught(ctx: ChannelHandlerContext, t: Throwable): Unit = {
    log.error("Error while processing WebSocket message", t)
    val error = RpcResponseEnvelope(
      callId = None,
      payload = EnsimeServerError(t.toString)
    )
    getOutHandler(ctx)(error)
  }

}

trait SubprotocolEncoder {
  def readFrame(request: String): RpcRequestEnvelope
  def writeFrame(response: RpcResponseEnvelope): String
}

object JerkySubprotocolEncoder extends SubprotocolEncoder {
  import spray.json._
  import org.ensime.jerky.JerkyFormats._

  override def readFrame(request: String): RpcRequestEnvelope =
    request.parseJson.convertTo[RpcRequestEnvelope]
  override def writeFrame(response: RpcResponseEnvelope): String =
    response.toJson.prettyPrint
}

object SwankySubprotocolEncoder extends SubprotocolEncoder {
  import org.ensime.sexp._
  import org.ensime.swanky.SwankyFormats._

  override def readFrame(request: String): RpcRequestEnvelope =
    request.parseSexp.convertTo[RpcRequestEnvelope]
  override def writeFrame(response: RpcResponseEnvelope): String =
    response.toSexp.prettyPrint
}
