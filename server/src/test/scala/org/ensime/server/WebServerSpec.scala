// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.server

import java.io.File
import java.nio.charset.Charset

import org.scalatest.{ FlatSpec, Matchers }
import org.scalamock.scalatest.MockFactory
import io.netty.channel.embedded.EmbeddedChannel
import io.netty.handler.codec.http.{ HttpMethod, DefaultFullHttpRequest, FullHttpResponse }
import io.netty.handler.codec.http.HttpVersion.HTTP_1_1
import io.netty.handler.codec.http.HttpMethod.GET
import io.netty.handler.codec.http.HttpResponseStatus.{ OK, NOT_FOUND }

import org.ensime.core.DocJarReading

class WebServerSpec extends FlatSpec with Matchers with MockFactory {

  val docs = new DocJarReading {
    def docJarContent(filename: String, entry: String): Option[Array[Byte]] =
      if (filename != "foo-1.0-javadoc.jar" || entry != "bar/Baz.html") None
      else Some("hello".getBytes)

    def docJars(): Set[File] = Set(new File("foo-javadoc.jar"), new File("bar-javadoc.jar"))
  }

  "WebServer" should "serve contents of documentation archives" in {

    val ch = channel()
    val resp = request(ch, GET, "/docs/foo-1.0-javadoc.jar/bar/Baz.html#thingy()")

    resp should not be null
    resp.status shouldBe OK
    resp.headers.get("Content-Type") shouldEqual "text/html; charset=UTF-8"
    resp.content.toString(Charset.forName("UTF-8")) shouldEqual "hello"

    ch.finish()
  }

  it should "respond with status NOT_FOUND for missing archive entries" in {

    val ch = channel()
    val resp = request(ch, GET, "/docs/foo-1.0-javadoc.jar/bar/Bag.html#thingy()")

    resp should not be null
    resp.status shouldBe NOT_FOUND

    ch.finish()
  }

  it should "provide a list of available documentation" in {

    val ch = channel()
    val resp = request(ch, GET, "/docs")
    val resp2 = request(ch, GET, "/docs/")

    resp shouldEqual resp2

    resp should not be null
    resp.status shouldBe OK
    resp.headers.get("Content-Type") shouldEqual "text/html; charset=UTF-8"

    val expectedContent =
      """<html><body><h1>ENSIME: Your Project's Documentation</h1><ul>""" +
        """<li><a href="docs/bar-javadoc.jar/index.html">bar-javadoc.jar</a></li>""" +
        """<li><a href="docs/foo-javadoc.jar/index.html">foo-javadoc.jar</a></li>""" +
        """</ul></body></html>"""
    resp.content.toString(Charset.forName("UTF-8")) shouldEqual expectedContent

    ch.finish()
  }

  def channel() = new EmbeddedChannel(new DocsHandler(docs))
  def request(ch: EmbeddedChannel, method: HttpMethod, uri: String) = {
    val req = new DefaultFullHttpRequest(HTTP_1_1, method, uri)
    ch.writeInbound(req)
    ch.readOutbound[FullHttpResponse]()
  }

}
