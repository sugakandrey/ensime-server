// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.server

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import io.netty.channel.{ Channel, ChannelHandlerContext, SimpleChannelInboundHandler }
import io.netty.handler.codec.http.websocketx.WebSocketServerProtocolHandler.ServerHandshakeStateEvent.HANDSHAKE_COMPLETE
import io.netty.handler.codec.http.websocketx.{ WebSocketServerHandshaker, TextWebSocketFrame, WebSocketFrame }
import io.netty.util.AttributeKey
import org.slf4j.LoggerFactory

import org.ensime.api._
import org.ensime.server.WebServer._

class WebSocketFrameHandler(
    hookHandlers: HookHandlers
) extends SimpleChannelInboundHandler[WebSocketFrame] {

  val log = LoggerFactory.getLogger("WebSocketHandler")

  val handshakerKey: AttributeKey[WebSocketServerHandshaker] =
    AttributeKey.valueOf(classOf[WebSocketServerHandshaker], "HANDSHAKER");

  val inHandlerKey: AttributeKey[String => Unit] =
    AttributeKey.valueOf(classOf[String => Unit], "INHANDLER");

  val outHandlerKey: AttributeKey[OutgoingHandler] =
    AttributeKey.valueOf(classOf[OutgoingHandler], "OUTHANDLER");

  val stashKey: AttributeKey[Vector[String]] =
    AttributeKey.valueOf(classOf[Vector[String]], "STASH");

  private def stashInHandler(ch: Channel): String => Unit =
    {
      msg =>
        val stashAttr = ch.attr(stashKey)
        stashAttr.set(stashAttr.get() :+ msg)
    }

  private def unstash(ctx: ChannelHandlerContext): Unit = {
    val inHandler = getInHandler(ctx)
    val stashAttr = ctx.channel().attr(stashKey)
    stashAttr.getAndRemove().foreach(inHandler)
  }

  private def getSelectedSubprotocol(ctx: ChannelHandlerContext): String =
    ctx.channel().attr(handshakerKey).get().selectedSubprotocol()

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

  override protected def userEventTriggered(ctx: ChannelHandlerContext, evt: Object): Unit = {
    if (HANDSHAKE_COMPLETE == evt) {
      ctx.channel().attr(stashKey).set(Vector())
      setInHandler(ctx, stashInHandler(ctx.channel()))
      Future {
        // Sorry about this tricky hack, it's the only way I found to access the handshaker
        // associated with the channel so I can ask it for the selected subprotocol.
        // The thing is that for the time when we receive this event the handshaker wasn't
        // set as a channel attribute yet. So we MUST wait for it for a few millis, and for
        // some reason we MUST do it in a separate thread.
        // See: [WebSocketServerProtocolHandshakeHandler.java:88](https://git.io/v6DcJ)
        Thread.sleep(500)
        val subprotocol = getSelectedSubprotocol(ctx)
        val encoder = encoderFor(subprotocol)
        val outHandler = encodedOutHandler(ctx.channel(), encoder)
        val inHandler = encodedInHandler(hookHandlers(outHandler), encoder)
        setInHandler(ctx, inHandler)
        setOutHandler(ctx, outHandler)
        log.info("Handlers ready")
        unstash(ctx)
      }
    }
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

