package variables

import debug.Helper._

object ReadVariables {

  def main(args: Array[String]) {
    val a = true
    val b = 'c'
    val c = 3.asInstanceOf[Short]
    val d = 4
    val e = 5L
    val f = 1.0f
    val g = 2.0
    val h = "test"
    val i = Array(1, 2, 3)
    val j = List(4, 5, 6)
    val k = Array(One("one"), 1, true)
    val l = NullToString
    val m = SimpleTestClass(Some("foo"), List(3, 2, 1))

    noop(None)
  }

  case class SimpleTestClass(x: Option[String], xs: List[Int])

}
