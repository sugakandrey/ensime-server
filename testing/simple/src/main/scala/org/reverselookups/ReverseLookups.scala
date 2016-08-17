package org.reverselookups

class MyException extends RuntimeException

@MyAnnotation
class ReverseLookups(val i: Int) {
  val fieldType: MyAnnotation = null

  val fieldInit: String = new MyAnnotation().toString

  val intField = catches()

  def methodUsage: Unit = returns(123)

  @MyAnnotation
  val annotatedField = 1

  def takesAsParam(ann: MyAnnotation): Unit = {
    println(intField)
    ???
  }

  def returns(i: Int = intField): MyAnnotation = ???

  def usesInBody(): Unit = {
    val ann = new MyAnnotation
  }

  @MyAnnotation
  def annotatedMethod(): Unit = ???

  def polyMethod[A <: MyAnnotation](a: A): Unit = ???

  def throws(): Unit = {
    val a = catches()
    throw new MyException
  }

  def catches(): Int = {
    try {
      1
    } catch {
      case e: MyException => 2
    }
  }
}

@MyAnnotation
object ReverseLookups {
  new ReverseLookups(1).returns(123)
  new MyAnnotation

  val staticField = new MyAnnotation

  def staticMethod[A >: MyAnnotation](a: A): Unit = ???
}

abstract class Extends extends MyException

class SelfType {
  this: MyAnnotation with MyException =>
  val usesField = new ReverseLookups(1).intField
}
