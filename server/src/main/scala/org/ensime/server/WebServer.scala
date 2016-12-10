// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.server

import scala.concurrent.{ Future, Promise }

import io.netty.bootstrap.ServerBootstrap
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.nio.NioServerSocketChannel
import io.netty.channel.socket.SocketChannel
import io.netty.channel.{ Channel, ChannelFuture, ChannelFutureListener, ChannelInitializer, EventLoopGroup, ChannelPipeline }
import io.netty.handler.codec.http.websocketx.WebSocketServerProtocolHandler
import io.netty.handler.codec.http.{ HttpObjectAggregator, HttpServerCodec }
import io.netty.handler.logging.LoggingHandler;

import org.ensime.api.{ RpcRequestEnvelope, RpcResponseEnvelope }
import org.ensime.core.DocJarReading

object WebServer {

  type IncomingHandler = RpcRequestEnvelope => Unit
  type OutgoingHandler = RpcResponseEnvelope => Unit
  type HookHandlers = OutgoingHandler => IncomingHandler

  private[server] def initPipeline(
    pipeline: ChannelPipeline,
    docs: DocJarReading,
    hookHandlers: HookHandlers
  ): Unit = {
    pipeline.addLast(new HttpServerCodec())
    pipeline.addLast(new HttpObjectAggregator(65536))
    pipeline.addLast(new WebSocketServerProtocolHandler("/websocket", "jerky, swanky"))
    pipeline.addLast(new WebSocketFrameHandler(hookHandlers))
    pipeline.addLast(new DocsHandler(docs))
  }

  def start(docs: DocJarReading, port: Int, hookHandlers: HookHandlers): Future[Channel] = {

    val bossGroup: EventLoopGroup = new NioEventLoopGroup(1)
    val workerGroup: EventLoopGroup = new NioEventLoopGroup

    val channelInitializer = new ChannelInitializer[SocketChannel]() {
      override def initChannel(ch: SocketChannel): Unit = {
        initPipeline(ch.pipeline(), docs, hookHandlers)
      }
    }

    val b = new ServerBootstrap()
    b.group(bossGroup, workerGroup)
      .channel(classOf[NioServerSocketChannel])
      .handler(new LoggingHandler())
      .childHandler(channelInitializer)

    val p = Promise[Channel]
    b.bind("127.0.0.1", port).addListener(new ChannelFutureListener() {
      def operationComplete(ftr: ChannelFuture): Unit = {
        if (!ftr.isSuccess) {
          p.failure(ftr.cause())
          bossGroup.shutdownGracefully()
          workerGroup.shutdownGracefully()
        } else {
          val ch = ftr.channel()
          ch.closeFuture().addListener(new ChannelFutureListener() {
            def operationComplete(ftr2: ChannelFuture): Unit = {
              bossGroup.shutdownGracefully()
              workerGroup.shutdownGracefully()
            }
          })
          p.success(ch)
        }
      }
    })

    p.future
  }
}
