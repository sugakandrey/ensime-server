// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.util

import java.sql.Timestamp

import org.scalatest.{ Entry, FlatSpec, Matchers }
import org.scalatest.enablers.Messaging._
import org.ensime.util.stringymap.syntax._
import org.ensime.util.stringymap.api._
import org.ensime.util.stringymap.impl._

class StringyMapSpec extends FlatSpec with Matchers {

  sealed trait Foo
  case class Bar(x: Int, s: String, l: Long) extends Foo
  case class Baz(o: Option[Int], t: Timestamp) extends Foo
  case object Qux extends Foo

  class NotSupported
  case class FooBar(ns: NotSupported)

  private val bar = Bar(123, "bar", 321l)
  private val baz = Baz(Some(123), new Timestamp(100))

  "StringyMap" should "marshall case class with supported field types" in {
    val expected = Map("x" -> 123, "s" -> "bar", "l" -> 321l)
    val map = bar.toProperties

    expected.foreach {
      case (k, v) =>
        map should contain(Entry(k, v))
    }
  }

  it should "unmarshall case class" in {
    val props = new StringyMap()
    props.put("o", new Integer(123))
    props.put("t", new java.lang.Long(100))
    props.as[Baz] should ===(baz)
  }

  it should "fail to marshall case classes with unsupported field types" in {
    "FooBar(new NotSupported()).toProperties" shouldNot typeCheck
  }

  it should "correctly fail to unmarshall case class" in {
    val props = new StringyMap()
    props.put("invalidFieldName", "123")
    the[IllegalArgumentException] thrownBy props.as[Baz] should have message "Null encountered in non-optional field"

    val props2 = new StringyMap()
    props2.put("x", new Integer(123))
    props2.put("s", "bar")
    props2.put("l", "notALong")
    the[ClassCastException] thrownBy props2.as[Bar] should have message "java.lang.String cannot be cast to java.lang.Long"
  }

  it should "marshall sealed trait hierarchies" in {
    bar.toProperties.as[Foo] should ===(bar)
    baz.toProperties.as[Foo] should ===(baz)
    Qux.toProperties.as[Foo] should ===(Qux)
  }
}
