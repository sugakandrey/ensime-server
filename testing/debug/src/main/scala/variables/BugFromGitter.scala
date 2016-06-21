package variables

/**
 * Bug from Gitter discussion with @dickwall and @rorygraves.
 *
 * Need to be able to inspect variables from the specified breakpoint.
 */
object BugFromGitter {
  def main(args: Array[String]): Unit = {
    val h = new BugFromGitter("Rory", 5)
    h.sayHello()
  }
}

case class Person(first: String, last: String, age: Int)

class BugFromGitter(name: String, times: Int) {
  def sayHello(): Unit = {
    val actualTimes = times * 2

    val arrayTest = Array(1, 2, 3, 4)
    val listTest = List(1, 2, 3, 4)

    val person = Person(name, "Smith", 25)

    println(arrayTest)
    println(listTest)
    println(person)
    println(actualTimes)

    for (i <- 1 to actualTimes) // Put breakpoint on this line
      println(s"Hello $name")
  }
}
