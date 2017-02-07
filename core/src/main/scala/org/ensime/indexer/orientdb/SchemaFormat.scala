// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.indexer.orientdb.schema

import java.sql.Timestamp

import com.orientechnologies.orient.core.metadata.schema.OType
import org.ensime.indexer.Access
import org.ensime.indexer.orientdb.api.OrientProperty
import shapeless._
import shapeless.labelled._

package api {

  import org.ensime.api.DeclaredAs

  trait SchemaFormat[T] {
    def toSchema: Map[String, OrientProperty]
  }

  trait OrientPropertyFormat[T] {
    def toOrientProperty: OrientProperty
  }

  trait OrientIdFormat[T, P] {
    def key: String
    def value(t: T): P
  }

  // defining really basic implementations on the companion
  object OrientPropertyFormat {
    implicit object StringOrientPropertyFormat extends OrientPropertyFormat[String] {
      def toOrientProperty: OrientProperty = OrientProperty(OType.STRING)
    }
    implicit object IntOrientPropertyFormat extends OrientPropertyFormat[Int] {
      def toOrientProperty: OrientProperty = OrientProperty(OType.INTEGER)
    }
    implicit object LongOrientPropertyFormat extends OrientPropertyFormat[Long] {
      def toOrientProperty: OrientProperty = OrientProperty(OType.LONG)
    }
    implicit object TimeStampOrientPropertyFormat extends OrientPropertyFormat[Timestamp] {
      def toOrientProperty: OrientProperty = OrientProperty(OType.LONG)
    }

    // domain specific
    implicit object AccessOrientPropertyFormat extends OrientPropertyFormat[Access] {
      def toOrientProperty: OrientProperty = OrientProperty(OType.INTEGER)
    }
    implicit object DeclaredAsPropertyFormat extends OrientPropertyFormat[DeclaredAs] {
      def toOrientProperty: OrientProperty = OrientProperty(OType.STRING)
    }

    implicit def OptionOrientPropertyFormat[T](
      implicit
      p: OrientPropertyFormat[T]
    ) = new OrientPropertyFormat[Option[T]] {
      def toOrientProperty: OrientProperty = OrientProperty(p.toOrientProperty.oType, isMandatory = false)
    }
  }
}

package object impl {
  import org.ensime.indexer.orientdb.schema.api._

  implicit def hNilSchemaFormat[T]: SchemaFormat[HNil] = new SchemaFormat[HNil] {
    def toSchema: Map[String, OrientProperty] = Map.empty
  }

  implicit def hListSchemaFormat[Key <: Symbol, Value, Remaining <: HList](
    implicit
    key: Witness.Aux[Key],
    prim: OrientPropertyFormat[Value],
    remV: Lazy[SchemaFormat[Remaining]]
  ): SchemaFormat[FieldType[Key, Value] :: Remaining] =
    new SchemaFormat[FieldType[Key, Value] :: Remaining] {
      def toSchema: Map[String, OrientProperty] = {
        val otype = prim.toOrientProperty
        val map = remV.value.toSchema
        map + (key.value.name -> otype)
      }
    }

  implicit def familySchemaFormat[T, Repr](
    implicit
    gen: LabelledGeneric.Aux[T, Repr],
    sg: Lazy[SchemaFormat[Repr]],
    tpe: Typeable[T]
  ): SchemaFormat[T] = new SchemaFormat[T] {
    def toSchema: Map[String, OrientProperty] = sg.value.toSchema
  }

}
