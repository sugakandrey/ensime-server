// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.util

import org.scalatest.{ FlatSpec, Matchers }
import org.ensime.util.enums._

class EnumsSpec extends FlatSpec with Matchers {

  private def marshall[T](expected: Map[String, T])(implicit adtToMap: AdtToMap[T]): Unit = {
    val result = adtToMap.lookup
    result should ===(expected)
  }

  "AdtToMap" should "marshall enum-like adt" in {
    sealed trait AllSingletons
    case object Foo extends AllSingletons
    case object Bar extends AllSingletons
    case object Baz extends AllSingletons

    val allSingletonsMap = Map("Foo" -> Foo, "Bar" -> Bar, "Baz" -> Baz)
    marshall[AllSingletons](allSingletonsMap)

    sealed abstract class Enums(val s: String)
    object Qux extends Enums("qux")
    object Quux extends Enums("quux")
    object FooBar extends Enums("foobar")
    object BarBaz extends Enums("barbaz")

    val enumsMap = Map("Qux" -> Qux, "Quux" -> Quux, "FooBar" -> FooBar, "BarBaz" -> BarBaz)
    marshall[Enums](enumsMap)
  }

  it should "not be able to convert non-singleton types" in {
    sealed trait NotAllSingletons
    case object Foo extends NotAllSingletons
    case object Bar extends NotAllSingletons
    case class FooBar(a: Int) extends NotAllSingletons

    "implicitly[AdtToMap[NotAllSingletons]]" shouldNot typeCheck
  }

}
