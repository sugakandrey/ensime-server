package org.example2

class Baz {
  class Wibble {
    class baz
    object baz
  }
  object Wibble
}

object Baz {
  new ToBeUnloaded().procedure()
}
