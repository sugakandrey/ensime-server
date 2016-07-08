// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
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
import org.ensime.indexer.graph.{ ClassDef, ClassHierarchy, Hierarchy, HierarchyType }
import org.ensime.indexer.stringymap.api._
import org.ensime.indexer.stringymap.syntax._

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
  // assigns some type info to a vertex that we have just created
  case class VertexT[T](underlying: Vertex)

  trait EdgeT[+Out, +In]

  sealed trait IndexT {
    def orientKey: String
  }

  object IndexT {
    case object Unique extends IndexT {
      override def orientKey = "unique"
    }

    case object NotUnique extends IndexT {
      override def orientKey = "notunique"
    }

    case object FullText extends IndexT {
      override def orientKey = "fulltext"
    }

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

    def createIndexOn[E <: Element, T, F](idx: IndexT = IndexT.NotUnique)(
      implicit
      tag: Indexable[E],
      bdf: BigDataFormat[T],
      id: BigDataFormatId[T, F]
    ): RichOrientGraph = {
      graph.createKeyIndex(id.key, tag.aClass, "type" -> idx.orientKey, "class" -> bdf.label)
      this
    }

    def createVertexFrom[T](superClass: Option[OClass] = None)(implicit bdf: BigDataFormat[T]): OClass = {
      graph.createVertexType(bdf.label)
      val schemaClass = schema.getClass(bdf.label)
      superClass.foreach(schemaClass.addSuperClass)
      bdf.toSchema.foreach {
        case (key, (otype, mandatory)) =>
          if (schemaClass.getProperty(key) == null) {
            schemaClass.createProperty(key, otype)
              .setMandatory(mandatory)
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
  // FIXME: all interaction with OrientDB should be from a dedicated thread pool
  //        see http://orientdb.com/docs/last/Java-Multi-Threading.html

  // the presentation complier doesn't like it if we pimp the Graph,
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

    // TODO: handle traits in the O and I
    //       requires higher kinded types or .Aux, beyond my foo.
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

      // TODO: minimal gremlin-scala syntax for this sort of thing
      // new GremlinPipeline[Vertex, Vertex](graph)
      //   //.property("typehint").cast(classOf[String])
      //   // .filter(new PipeFunction[String, java.lang.Boolean]() {
      //   //   def compute(argument: String) = argument == s.label
      //   // })
      //   .map().transform(
      //     new PipeFunction[java.util.Map[String, AnyRef], T] {
      //       def compute(argument: java.util.Map[String, AnyRef]): T =
      //         s.fromProperties(argument.asScala.toMap)
      //     }
      //   ).iterator().asScala.toList

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
          val keys = v.getPropertyKeys.asScala
          val old = v.getPropertyMap.asScala
          val updates = t.toProperties.asScala

          updates.foreach {
            case (key, value) =>
              if (!old.contains(key) || old(key) != value)
                v.setProperty(key, value)
          }

          //          (keys -- updates.keySet).foreach { nulled =>
          //            v.removeProperty[AnyRef](nulled)
          //          }

          existing
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
      p: SPrimitive[P]
    ): Boolean = {
      def removeRecursive(
        v: Vertex
      ): Unit = {
        v.getVertices(Direction.IN).asScala.foreach(removeRecursive)
        graph.removeVertex(v)
      }

      readUniqueV(u.value(t)) match {
        case Some(vertexT) =>
          val v = vertexT.underlying
          removeRecursive(v)
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
      p: SPrimitive[P]
    ): Int = ts.map(removeV(_)).count(_ == true)

    def allV[T](
      implicit
      graph: OrientBaseGraph,
      s: BigDataFormat[T]
    ): List[T] = {
      graph.getVerticesOfClass(s.label)
        .asScala.map(_.to[T])(collection.breakOut)
    }

    def classHierarchy[P](
      value: P,
      hierarchyType: HierarchyType
    )(
      implicit
      graph: OrientBaseGraph,
      s: BigDataFormat[ClassDef],
      u: BigDataFormatId[ClassDef, P],
      p: SPrimitive[P]
    ): Option[Hierarchy] = {
      val direction = hierarchyType match {
        case HierarchyType.Subclasses => Direction.IN
        case HierarchyType.Superclasses => Direction.OUT
      }

      def traverseClassHierarchy(
        v: Vertex
      ): Hierarchy = v.getVertices(direction, "IsParent").asScala.toList match {
        case Nil => v.to[ClassDef]
        case xs => ClassHierarchy(v.to[ClassDef], xs.sortBy(_.getProperty[String]("fqn")).map(traverseClassHierarchy))
      }

      readUniqueV(value) match {
        case Some(vertexT) => Some(traverseClassHierarchy(vertexT.underlying))
        case None => None
      }
    }
  }
}
