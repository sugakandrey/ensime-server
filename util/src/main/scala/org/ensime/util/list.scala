// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.util

import scala.Predef.{ any2stringadd => _, _ }

package object list {

  implicit class RichList[T](val list: List[T]) extends AnyVal {

    // two list creations. Could be optimised to an array and a list creation
    /**
     * @return everything up to the last element, and the last element.
     */
    def initLast: (List[T], T) = list.reverse match {
      case head :: tail => (tail.reverse, head)
      case _ => throw new IllegalArgumentException("list was empty")
    }

    /**
     * Like `distinct` but by a property on the elements. Keeps the
     * first of any duplicates.
     */
    def distinctBy[U](key: T => U): List[T] = {
      var builder = List[T]()
      var seen = Set[U]()
      list.foreach { entry =>
        val k = key(entry)
        if (!seen(k)) {
          seen += k
          builder ::= entry
        }
      }
      builder.reverse
    }
  }

  implicit class RichListTuple2[K, V](val list: List[(K, V)]) extends AnyVal {
    /**
     * Whereas a list of tuples may often be treated as a `Map`, here
     * we wish to treat it as a multimap of sets.
     */
    def toMultiMapSet: Map[K, Set[V]] = {
      import collection.mutable
      val builder = new mutable.HashMap[K, mutable.Set[V]] with mutable.MultiMap[K, V]

      list.foreach {
        case (k, v) => builder.addBinding(k, v)
      }
      // its all a bit awkward to work with because the signature of
      // the MultiMap has a mutable Set as the value, *sigh*
      builder.map {
        case (k, vs) => (k, vs.toSet)
      }(collection.breakOut)
    }
  }

}
