package org.reverselookups

class Overloads(l: Long) {
  val rl = new ReverseLookups(123)
  rl.catches()

  def foo(): Unit = ???
  def foo(i: Int): Unit = ???
  def foo(s: String, i: Int): Unit = ???
  def foo(ann: MyAnnotation): Unit = ???
  def foo[T <: MyException](t: T): Unit = ???
  def asDefaultArg(i: Int = rl.catches()): Unit = ???
}

object Overloads {
  new MyAnnotation

  def foo(): Unit = ???
  def foo(i: Int): Unit = ???
}
