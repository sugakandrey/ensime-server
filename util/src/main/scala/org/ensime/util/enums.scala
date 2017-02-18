// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.util

import shapeless._

// workaround lack of automatic enums for sealed traits by using shapeless
package enums {

  trait SingletonByName[A, C <: Coproduct] {
    def lookup: Map[String, A]
  }
  object SingletonByName {
    implicit def CNilSingleton[A]: SingletonByName[A, CNil] =
      new SingletonByName[A, CNil] { override def lookup: Map[String, A] = Map.empty }

    implicit def coproductSingletons[A, H <: A, T <: Coproduct](
      implicit
      tsbn: SingletonByName[A, T],
      witness: Witness.Aux[H],
      tpe: Typeable[H]
    ): SingletonByName[A, H :+: T] = new SingletonByName[A, H :+: T] {
      override def lookup: Map[String, A] = {
        val label = tpe.describe.replaceAll(".type", "")
        tsbn.lookup + (label -> witness.value)
      }
    }
  }

  trait AdtToMap[A] {
    def lookup: Map[String, A]
  }
  object AdtToMap {
    implicit def fromSingletonByName[A, C <: Coproduct](
      implicit
      gen: Generic.Aux[A, C],
      singletonByName: SingletonByName[A, C]
    ): AdtToMap[A] = new AdtToMap[A] {
      override def lookup: Map[String, A] = singletonByName.lookup
    }
  }

}
