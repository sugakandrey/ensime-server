// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
/**
 * A typesafe wrapper around OrientDB.
 */
package org.ensime.indexer.orientdb

import scala.Predef.{ any2stringadd => _, _ }
import scala.collection.JavaConverters._
import scala.concurrent.{ ExecutionContext, Future, blocking }
import akka.event.slf4j.SLF4JLogging
import com.orientechnologies.orient.core.metadata.schema.OClass
import com.tinkerpop.blueprints._
import com.tinkerpop.blueprints.impls.orient._
import org.ensime.indexer.graph.GraphService.{ IsParent, UsedIn, EnclosingClass }
import org.ensime.indexer.graph._
import org.ensime.indexer.orientdb.api._
import org.ensime.indexer.orientdb.schema.api._
import org.ensime.indexer.stringymap.api._
import org.ensime.indexer.stringymap.syntax._
import shapeless.Typeable

// NOTE: this uses the v2 version of tinkerpop. v3 is on Java 8.
//       https://github.com/mpollmeier/gremlin-scala/issues/102
//
// gremlin-scala uses Dynamic, which effectively makes it useless
// https://github.com/mpollmeier/gremlin-scala/blob/2.x/src/main/scala/com/tinkerpop/gremlin/scala/GremlinScalaPipeline.scala

// prefer the OrientDB docs to understand this file
//   http://orientdb.com/docs/2.1/Graph-Database-Tinkerpop.html
// as the Gremlin docs are terrible:
// https://github.com/tinkerpop/gremlin/wiki
// https://github.com/tinkerpop/blueprints/wiki/Code-Examples

package api {
  import com.orientechnologies.orient.core.metadata.schema.OType

  case class OrientProperty(oType: OType, isMandatory: Boolean = true)

  // assigns some type info to a vertex that we have just created
  case class VertexT[+T](underlying: Vertex)

  trait EdgeT[+Out, +In]

  sealed trait IndexT {
    def orientKey: String
  }

  case object Unique extends IndexT {
    override def orientKey = "unique"
  }

  case object NotUnique extends IndexT {
    override def orientKey = "notunique"
  }

  case object FullText extends IndexT {
    override def orientKey = "fulltext"
  }

  trait Indexable[T <: Element] {
    def aClass: Class[T]
  }

  object Indexable {
    implicit object VertexIndexable extends Indexable[Vertex] {
      override def aClass: Class[Vertex] = classOf[Vertex]
    }
    implicit object EdgeIndexable extends Indexable[Edge] {
      override def aClass: Class[Edge] = classOf[Edge]
    }
  }
}

package object syntax {
  import org.ensime.indexer.orientdb.api.{ IndexT, Indexable }

  implicit class RichOrientGraph(graph: OrientExtendedGraph) {
    private val schema = graph.getRawGraph.getMetadata.getSchema

    def createIndexOn[E <: Element, T, F](idx: IndexT = NotUnique)(
      implicit
      tag: Indexable[E],
      tpe: Typeable[T],
      id: BigDataFormatId[T, F]
    ): RichOrientGraph = {
      val label = tpe.describe.replace(".type", "")
      graph.createKeyIndex(id.key, tag.aClass, "type" -> idx.orientKey, "class" -> label)
      this
    }

    def createVertexFrom[T](superClass: Option[OClass] = None)(
      implicit
      bdf: BigDataFormat[T],
      sg: SchemaFormat[T]
    ): OClass = {
      graph.createVertexType(bdf.label)
      val schemaClass = schema.getClass(bdf.label)
      superClass.foreach(schemaClass.addSuperClass)
      sg.toSchema.foreach {
        case (key, OrientProperty(oType, isMandatory)) =>
          if (schemaClass.getProperty(key) == null) {
            schemaClass.createProperty(key, oType)
              .setMandatory(isMandatory)
              .setNotNull(true)
          }
      }
      schemaClass
    }

    def createEdge[T](implicit bdf: BigDataFormat[T]): RichOrientGraph = {
      graph.createEdgeType(bdf.label)
      this
    }
  }

  import org.ensime.indexer.orientdb.api._

  implicit def RichParameter(props: (String, String)): Parameter[String, String] =
    new Parameter(props._1, props._2)

  implicit class RichVertex(val v: Vertex) extends AnyVal {
    def getPropertyMap: java.util.HashMap[String, AnyRef] = {
      val props = new java.util.HashMap[String, AnyRef]()
      v.getPropertyKeys().asScala.foreach { key =>
        props.put(key, v.getProperty[AnyRef](key))
      }
      props
    }
    def to[T](implicit s: BigDataFormat[T]): T =
      s.fromProperties(getPropertyMap).getOrThrowError
  }

  implicit class RichVertexT[T](val v: VertexT[T]) extends AnyVal {
    def toDomain(implicit s: BigDataFormat[T]): T = v.underlying.to[T]

    def remove(implicit graph: OrientBaseGraph): Unit = graph.removeVertex(v.underlying)

    def getProperty[P](key: String): P = v.underlying.getProperty[P](key)

    def setProperty[P](key: String, p: P): Unit = v.underlying.setProperty(key, p)

    def getInVertices[S, E <: EdgeT[S, T]](
      implicit
      bdf: BigDataFormat[E]
    ): Iterable[VertexT[S]] = v.underlying.getVertices(Direction.IN, bdf.label).asScala.map(VertexT[S])

    def getOutVertices[S, E <: EdgeT[T, S]](
      implicit
      bdf: BigDataFormat[E]
    ): Iterable[VertexT[S]] = v.underlying.getVertices(Direction.OUT, bdf.label).asScala.map(VertexT[S])
  }

  /**
   * Creates a context to communicate with the Graph.
   *  Commits are not automatic for performance reasons.
   */
  def withGraph[T](f: OrientBaseGraph => T)(implicit factory: OrientGraphFactory): T = {
    f(factory.getNoTx())
  }

  // NOTE: although providing isolation, this destroys stacktraces
  def withGraphAsync[T](
    f: OrientBaseGraph => T
  )(
    implicit
    factory: OrientGraphFactory,
    ec: ExecutionContext
  ): Future[T] = Future { blocking { withGraph(f) } }

  // the presentation complier doesn't like it if we enrich the Graph,
  // so do it this way instead
  object RichGraph extends SLF4JLogging {
    /** Side-effecting vertex insertion. */
    def insertV[T](t: T)(implicit graph: Graph, s: BigDataFormat[T]): VertexT[T] = {
      val props = t.toProperties
      val v = graph.addVertex("class:" + t.label)
      props.asScala.foreach {
        case (key, value) => v.setProperty(key, value)
      }
      VertexT[T](v)
    }

    def insertE[O, I, E <: EdgeT[O, I]](out: VertexT[O], in: VertexT[I], e: E)(
      implicit
      graph: Graph,
      tpe: shapeless.Typeable[E],
      ser: BigDataFormat[E]
    ): Unit = {
      graph.addEdge("class:" + ser.label, out.underlying, in.underlying, null)
    }

    /**
     * Obtain a unique vertex representing an entity of type `T` with a field equal to value.
     *  Uniqueness must be enforced by convention. (maybe we should force the field name?)
     */
    def readUniqueV[T, P](
      value: P
    )(
      implicit
      graph: OrientBaseGraph,
      s: BigDataFormat[T],
      u: BigDataFormatId[T, P],
      p: SPrimitive[P]
    ): Option[VertexT[T]] = {
      graph.getVertices(s.label + "." + u.key, p.toValue(value))
        .asScala.toList match {
          case Nil => None
          case head :: Nil => Some(VertexT(head))
          case multi => throw new IllegalStateException(s"multiple hits ${multi.size}")
        }
    }

    def upsertV[T, P](
      t: T
    )(
      implicit
      graph: OrientBaseGraph,
      s: BigDataFormat[T],
      u: BigDataFormatId[T, P],
      p: SPrimitive[P]
    ): VertexT[T] = {
      readUniqueV(u.value(t)) match {
        case None => insertV(t)
        case Some(existing) =>
          val v = existing.underlying
          val old = v.getPropertyMap.asScala
          val updates = t.toProperties.asScala

          updates.foreach {
            case (key, value) =>
              if (!old.contains(key) || old(key) != value)
                v.setProperty(key, value)
          }

          existing
      }
    }

    def insertIfNotExists[T, P](
      t: T
    )(
      implicit
      graph: OrientBaseGraph,
      s: BigDataFormat[T],
      u: BigDataFormatId[T, P],
      p: SPrimitive[P]
    ): VertexT[T] = {
      readUniqueV(u.value(t)) match {
        case None => insertV(t)
        case Some(existing) => existing
      }
    }

    /**
     * Removes a vertex, representing `t`, from the graph.
     *
     * @return `true` if the graph contained the specified element
     */
    def removeV[T, P](
      t: T
    )(
      implicit
      graph: OrientBaseGraph,
      s: BigDataFormat[T],
      u: BigDataFormatId[T, P],
      p: SPrimitive[P],
      cdefFormat: BigDataFormat[ClassDef]
    ): Boolean = {
      import GraphService.{ DefinedInS, EnclosingClassS }

      def removeRecursive(
        v: Vertex
      ): Unit = {
        v.getVertices(Direction.IN, DefinedInS.label)
          .asScala
          .foreach(removeRecursive)

        v.getVertices(Direction.IN, EnclosingClassS.label)
          .asScala
          .filter(_.getProperty[String]("typehint") != cdefFormat.label)
          .foreach(removeRecursive)

        graph.removeVertex(v)
      }

      readUniqueV[T, P](u.value(t)) match {
        case Some(vertexT) =>
          removeRecursive(vertexT.underlying)
          true
        case None => false
      }
    }

    /**
     * Removes all vertices, representing domain objects in `ts`, from graph.
     *
     * @param ts elements to be deleted
     * @return amount of vertices deleted
     */
    def removeV[T, P](
      ts: Seq[T]
    )(
      implicit
      graph: OrientBaseGraph,
      s: BigDataFormat[T],
      u: BigDataFormatId[T, P],
      p: SPrimitive[P],
      cdefFormat: BigDataFormat[ClassDef]
    ): Int = ts.map(removeV(_)).count(_ == true)

    def allV[T](
      implicit
      graph: OrientBaseGraph,
      s: BigDataFormat[T]
    ): List[T] = {
      graph.getVerticesOfClass(s.label)
        .asScala.map(_.to[T])(collection.breakOut)
    }

    def classHierarchy[P: Ordering](
      value: P,
      hierarchyType: Hierarchy.Direction
    )(
      implicit
      graph: OrientBaseGraph,
      s: BigDataFormat[ClassDef],
      u: BigDataFormatId[ClassDef, P],
      p: SPrimitive[P]
    ): Option[Hierarchy] = {
      import GraphService.IsParentS

      def traverseClassHierarchy(
        v: VertexT[ClassDef]
      ): Hierarchy = {
        val vertices: Iterable[VertexT[ClassDef]] = hierarchyType match {
          case Hierarchy.Subtypes => v.getInVertices[ClassDef, IsParent.type]
          case Hierarchy.Supertypes => v.getOutVertices[ClassDef, IsParent.type]
        }

        vertices.toList match {
          case Nil => v.toDomain
          case xs => TypeHierarchy(v.toDomain, xs.sortBy(_.getProperty[P](u.key)).map(traverseClassHierarchy))
        }
      }

      readUniqueV[ClassDef, P](value) match {
        case Some(vertexT) => Some(traverseClassHierarchy(vertexT))
        case None => None
      }
    }

    def findUsages[P](
      value: P
    )(
      implicit
      graph: OrientBaseGraph,
      bdf: BigDataFormat[FqnSymbol],
      bdfId: BigDataFormatId[FqnSymbol, P],
      p: SPrimitive[P]
    ): Seq[VertexT[FqnSymbol]] = {
      import GraphService.{ UsedInS, EnclosingClassS }

      def traverseEnclosingClasses(v: VertexT[FqnSymbol]): Iterable[VertexT[FqnSymbol]] = {
        val vertices = v.getInVertices[FqnSymbol, EnclosingClass.type]
        vertices ++ vertices.flatMap(traverseEnclosingClasses)
      }

      readUniqueV[FqnSymbol, P](value) match {
        case Some(vertexT) =>
          val innerClasses = traverseEnclosingClasses(vertexT).toList
          (vertexT :: innerClasses).flatMap(_.getOutVertices[FqnSymbol, UsedIn.type]).distinct
        case None => Seq.empty
      }
    }
  }
}
