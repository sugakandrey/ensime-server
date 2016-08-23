// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.server

import java.nio.file.{ Files, Paths }

import io.netty.buffer.{ ByteBuf, Unpooled }
import io.netty.channel.{ ChannelFuture, ChannelFutureListener, ChannelHandlerContext, SimpleChannelInboundHandler }
import io.netty.handler.codec.http.HttpHeaderNames.CONTENT_TYPE
import io.netty.handler.codec.http.HttpMethod.GET
import io.netty.handler.codec.http.HttpResponseStatus
import io.netty.handler.codec.http.HttpResponseStatus.{ BAD_REQUEST, FORBIDDEN, NOT_FOUND, OK }
import io.netty.handler.codec.http.HttpVersion.HTTP_1_1
import io.netty.handler.codec.http.{ DefaultFullHttpResponse, FullHttpRequest, HttpUtil }
import io.netty.util.CharsetUtil
import scalatags.Text.all._

import org.ensime.core.DocJarReading

class DocsHandler(docs: DocJarReading) extends SimpleChannelInboundHandler[FullHttpRequest] {

  val DocList = """/docs/?""".r
  val DocEntry = """/docs/([^/]+\.jar)/(.+?)(?:#.*)?"""r

  override def channelRead0(ctx: ChannelHandlerContext, req: FullHttpRequest): Unit = {

    // Handle a bad request.
    if (!req.decoderResult().isSuccess())
      sendHttpResponse(ctx, req, Left(BAD_REQUEST))

    // Allow only GET methods.
    else if (req.method() != GET)
      sendHttpResponse(ctx, req, Left(FORBIDDEN))

    else req.uri() match {

      // Send the index page listing the doc jars
      case DocList() =>
        val content: ByteBuf = docJarsPage
        sendHttpResponse(ctx, req, Right((Some("text/html"), content)))

      // Send the content for the requested filename and entry pair
      case DocEntry(filename, entry) =>
        docs.docJarContent(filename, entry)
          .fold(sendHttpResponse(ctx, req, Left(NOT_FOUND))) {
            jarContent =>
              val mediaOpt = Option(Files.probeContentType(Paths.get(entry)))
              val content = Unpooled.copiedBuffer(jarContent)
              sendHttpResponse(ctx, req, Right((mediaOpt, content)))
          }

      // Can't handle this request
      case _ => sendHttpResponse(ctx, req, Left(NOT_FOUND))
    }
  }

  private def sendHttpResponse(
    ctx: ChannelHandlerContext, req: FullHttpRequest,
    statusOrContent: Either[HttpResponseStatus, (Option[String], ByteBuf)]
  ): Unit = {

    val res = statusOrContent match {
      case Left(status) =>
        // Generate an error page if response status code is not OK (200).
        val res = new DefaultFullHttpResponse(HTTP_1_1, status)
        val buf: ByteBuf = Unpooled.copiedBuffer(status.toString(), CharsetUtil.UTF_8)
        res.content().writeBytes(buf)
        buf.release()
        res
      case Right((mimeTypeOpt, content)) =>
        val res = new DefaultFullHttpResponse(HTTP_1_1, OK, content)
        mimeTypeOpt.foreach {
          mimeType => res.headers().set(CONTENT_TYPE, s"$mimeType; charset=UTF-8")
        }
        res
    }
    HttpUtil.setContentLength(res, res.content().readableBytes())

    // Send the response and close the connection if necessary.
    val f: ChannelFuture = ctx.channel().writeAndFlush(res)
    if (!HttpUtil.isKeepAlive(req) || res.status().code() != OK.code())
      f.addListener(ChannelFutureListener.CLOSE)
  }

  private val docJarsPage: ByteBuf = {
    def jars = docs.docJars().toList.map(_.getName).sorted
    val content =
      html(
        body(
          h1("ENSIME: Your Project's Documentation"),
          ul(for (jar <- jars) yield li(a(href := s"docs/$jar/index.html")(jar)))
        )
      )
    Unpooled.copiedBuffer(content.toString, CharsetUtil.UTF_8)
  }

}
