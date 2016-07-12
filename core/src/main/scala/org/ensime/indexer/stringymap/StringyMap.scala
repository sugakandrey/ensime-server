// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
/**
 * TypeClass (api/impl/syntax) for marshalling objects into
 * `java.util.HashMap<String,Object>` (yay, big data!).
 */
package org.ensime.indexer.stringymap

import shapeless._
import shapeless.labelled._

package object api {
  type StringyMap = java.util.HashMap[String, AnyRef]
  type BigResult[T] = Either[String, T] // aggregating errors doesn't add much
}

package api {
  trait BigDataFormat[T] {
    def label: String
    def toProperties(t: T): StringyMap
    def fromProperties(m: StringyMap): BigResult[T]
  }

  trait BigDataFormatId[T, P] {
    def key: String
    def value(t: T): P
  }

  trait SPrimitive[T] {
    def toValue(v: T): AnyRef
    def fromValue(v: AnyRef): T
  }

  // defining really basic implementations on the companion
  object SPrimitive {
    implicit object StringSPrimitive extends SPrimitive[String] {
      def toValue(v: String): String = v
      def fromValue(v: AnyRef): String = v.asInstanceOf[String]
    }
    implicit object IntSPrimitive extends SPrimitive[Int] {
      def toValue(v: Int): java.lang.Integer = v
      def fromValue(v: AnyRef): Int = v.asInstanceOf[java.lang.Integer]
    }
    implicit object LongSPrimitive extends SPrimitive[Long] {
      def toValue(v: Long): java.lang.Long = v
      def fromValue(v: AnyRef): Long = v.asInstanceOf[java.lang.Long]
    }
    implicit def OptionSPrimitive[T](
      implicit
      p: SPrimitive[T]
    ) = new SPrimitive[Option[T]] {
      def toValue(v: Option[T]): AnyRef = v match {
        case None => null
        case Some(t) => p.toValue(t)
      }
      def fromValue(v: AnyRef): Option[T] =
        if (v == null) None
        else Some(p.fromValue(v))
    }
  }

}

package object impl {
  import api._

  implicit def hNilBigDataFormat[T]: BigDataFormat[HNil] = new BigDataFormat[HNil] {
    def label: String = ???
    def toProperties(t: HNil): StringyMap = new java.util.HashMap()
    def fromProperties(m: StringyMap) = Right(HNil)
  }

  implicit def hListBigDataFormat[Key <: Symbol, Value, Remaining <: HList](
    implicit
    key: Witness.Aux[Key],
    prim: SPrimitive[Value],
    remV: Lazy[BigDataFormat[Remaining]]
  ): BigDataFormat[FieldType[Key, Value] :: Remaining] =
    new BigDataFormat[FieldType[Key, Value] :: Remaining] {
      def label: String = ???

      def toProperties(t: FieldType[Key, Value] :: Remaining): StringyMap = {
        val value = prim.toValue(t.head)
        val map = remV.value.toProperties(t.tail)
        if (value != null) map.put(key.value.name, value)
        map
      }

      def fromProperties(m: StringyMap) = {
        import scala.util.{ Try, Success, Failure }
        val value = m.get(key.value.name)
        /*
        This is a pretty hacky way to handle null => Empty option case, i'd love
        to have a more typesafe way to do this.
         */
        val errorMessage = s"Missing key ${key.value.name} in $m"
        val resolved = Try(prim.fromValue(value)) match {
          case Success(v) => Right(v)
          case Failure(exc) => Left(errorMessage)
        }
        for {
          remaining <- remV.value.fromProperties(m).right
          current <- resolved.right
        } yield field[Key](current) :: remaining
      }
    }

  // TODO: coproducts

  implicit def familyBigDataFormat[T, Repr](
    implicit
    gen: LabelledGeneric.Aux[T, Repr],
    sg: Lazy[BigDataFormat[Repr]],
    tpe: Typeable[T]
  ): BigDataFormat[T] = new BigDataFormat[T] {
    // HACK: really need a Wrapper like sjs
    // WORKAROUND: edge names cannot have dots in them
    def label: String = tpe.describe.replace(".type", "")
    def toProperties(t: T): StringyMap = sg.value.toProperties(gen.to(t))
    def fromProperties(m: StringyMap): BigResult[T] =
      sg.value.fromProperties(m).right.map(gen.from)
  }
}

package impl {
  import api._

  // I tried and failed to have nice syntax for this
  // see https://gist.github.com/fommil/784726514fed98e4c802
  object LensId {
    def apply[T, P](
      field: String,
      getter: Lens[T, P]
    ): BigDataFormatId[T, P] = new BigDataFormatId[T, P] {
      def key = field
      def value(t: T): P = getter.get(t)
    }
  }
}

package object syntax {
  import api._

  implicit class RichBigResult[R](val e: BigResult[R]) extends AnyVal {
    def getOrThrowError: R = e match {
      case Left(error) => throw new IllegalArgumentException(error)
      case Right(r) => r
    }
  }

  /** Syntactic helper for serialisables. */
  implicit class RichBigDataFormat[T](val t: T) extends AnyVal {
    def label(implicit s: BigDataFormat[T]): String = s.label
    def toProperties(implicit s: BigDataFormat[T]): StringyMap = s.toProperties(t)
    def idKey[P](implicit lens: Lens[T, P]): String = ???
    def idValue[P](implicit lens: Lens[T, P]): P = lens.get(t)
  }

  implicit class RichProperties(val props: StringyMap) extends AnyVal {
    def as[T](implicit s: BigDataFormat[T]): T = s.fromProperties(props).getOrThrowError
  }
}
