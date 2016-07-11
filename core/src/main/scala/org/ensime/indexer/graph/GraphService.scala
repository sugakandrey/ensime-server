// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.indexer.graph

import java.sql.Timestamp
import java.util.concurrent.Executors

import scala.Predef.{ any2stringadd => _, _ }
import scala.concurrent._
import akka.event.slf4j.SLF4JLogging
import com.orientechnologies.orient.core.Orient
import com.orientechnologies.orient.core.config.OGlobalConfiguration
import com.orientechnologies.orient.core.intent.OIntentMassiveInsert
import com.orientechnologies.orient.core.metadata.schema.OType
import com.tinkerpop.blueprints.Vertex
import com.tinkerpop.blueprints.impls.orient.OrientGraphFactory
import org.apache.commons.vfs2.FileObject
import org.ensime.api.DeclaredAs
import org.ensime.indexer.IndexService.FqnIndex
import org.ensime.indexer.SearchService.SourceInfo
import org.ensime.indexer._
import org.ensime.indexer.orientdb.api._
import org.ensime.indexer.orientdb.syntax._
import org.ensime.indexer.stringymap.api._
import org.ensime.indexer.stringymap.impl._
import org.ensime.util.file._
import org.ensime.vfs._
import shapeless.cachedImplicit

sealed trait FqnSymbol {
  def fqn: String
  def line: Option[Int]
  def source: Option[String]
  def declAs: DeclaredAs
  def access: Access

  def sourceFileObject(implicit vfs: EnsimeVFS): Option[FileObject] = source.map(vfs.vfile)
}

sealed trait Hierarchy {
  def toSet: Set[ClassDef] = this match {
    case cdef @ ClassDef(_, _, _, _, _, _) => Set(cdef)
    case ClassHierarchy(cdef, refs) => Set(cdef) ++ refs.flatMap(_.toSet)
  }
}

final case class ClassHierarchy(aClass: ClassDef, classRefs: Seq[Hierarchy]) extends Hierarchy

sealed trait HierarchyType
object HierarchyType {
  case object Subclasses extends HierarchyType
  case object Superclasses extends HierarchyType
}

final case class ClassDef(
    fqn: String,
    file: String,
    path: String,
    source: Option[String],
    line: Option[Int],
    access: Access
) extends FqnSymbol with Hierarchy {
  override def declAs: DeclaredAs = DeclaredAs.Class
}

sealed trait Member extends FqnSymbol

final case class Field(
    fqn: String,
    internal: Option[String],
    line: Option[Int],
    source: Option[String],
    access: Access
) extends Member {
  override def declAs: DeclaredAs = DeclaredAs.Field
}

final case class Method(
    fqn: String,
    line: Option[Int],
    source: Option[String],
    access: Access
) extends Member {
  override def declAs: DeclaredAs = DeclaredAs.Method
}

final case class FileCheck(filename: String, timestamp: Timestamp) {
  def file(implicit vfs: EnsimeVFS): FileObject = vfs.vfile(filename)
  def lastModified: Long = timestamp.getTime
  def changed(implicit vfs: EnsimeVFS): Boolean = file.getContent.getLastModifiedTime != lastModified
}
object FileCheck extends ((String, Timestamp) => FileCheck) {
  def apply(f: FileObject): FileCheck = {
    val name = f.getName.getURI
    val ts = if (f.exists()) new Timestamp(f.getContent.getLastModifiedTime)
    else new Timestamp(-1L)
    FileCheck(name, ts)
  }
}

// core/it:test-only *Search* -- -z prestine
class GraphService(dir: File) extends SLF4JLogging {
  import org.ensime.indexer.graph.GraphService._

  implicit object TimeStampSPrimitive extends SPrimitive[Timestamp] {
    import SPrimitive.LongSPrimitive
    def toValue(v: Timestamp): java.lang.Long = LongSPrimitive.toValue(v.getTime)
    def fromValue(v: AnyRef): Timestamp = new Timestamp(LongSPrimitive.fromValue(v))
    def getType: (OType, Boolean) = OType.LONG -> true
  }

  implicit object AccessSPrimitive extends SPrimitive[Access] {
    import org.objectweb.asm.Opcodes._
    import SPrimitive.IntSPrimitive

    def toValue(v: Access): java.lang.Integer =
      if (v == null) null
      else {
        val code = v match {
          case Public => ACC_PUBLIC
          case Private => ACC_PRIVATE
          case Protected => ACC_PROTECTED
          case Default => 0
        }
        IntSPrimitive.toValue(code)
      }

    def fromValue(v: AnyRef): Access = Access(IntSPrimitive.fromValue(v))

    def getType: (OType, Boolean) = OType.INTEGER -> true
  }

  implicit val FqnSymbolS: BigDataFormat[FqnSymbol] = cachedImplicit
  implicit val MemberS: BigDataFormat[Member] = cachedImplicit
  implicit val FileCheckS: BigDataFormat[FileCheck] = cachedImplicit
  implicit val ClassDefS: BigDataFormat[ClassDef] = cachedImplicit
  implicit val MethodS: BigDataFormat[Method] = cachedImplicit
  implicit val FieldS: BigDataFormat[Field] = cachedImplicit
  implicit val DefinedInS: BigDataFormat[DefinedIn.type] = cachedImplicit
  implicit val OwningClassS: BigDataFormat[OwningClass.type] = cachedImplicit

  import shapeless._
  implicit val UniqueFileCheckV = LensId("filename", lens[FileCheck] >> 'filename)
  implicit val FieldV = LensId("fqn", lens[Field] >> 'fqn)
  implicit val MethodV = LensId("fqn", lens[Method] >> 'fqn)
  implicit val ClassDefV = LensId("fqn", lens[ClassDef] >> 'fqn)

  private implicit val FqnSymbolLens = new Lens[FqnSymbol, String] {
    override def get(sym: FqnSymbol): String = sym.fqn
    override def set(sym: FqnSymbol)(fqn: String) = ???
  }

  private implicit val FqnIndexLens = new Lens[FqnIndex, String] {
    override def get(index: FqnIndex): String = index.fqn
    override def set(index: FqnIndex)(fqn: String): FqnIndex = ???
  }

  implicit val UniqueFqnIndexV = LensId("fqn", FqnIndexLens)
  implicit val UniqueFqnSymbolV = LensId("fqn", FqnSymbolLens)

  // all methods return Future, which means we can do isolation by
  // doing all work on a single worker Thread. We can't optimise until
  // we better understand the concurrency of our third party
  // libraries.
  private val pools = 1
  private implicit val ec = ExecutionContext.fromExecutor(
    Executors.newSingleThreadExecutor()
  // WARNING: Faster, but needs further thought
  //Executors.newFixedThreadPool(pools)
  // http://orientdb.com/docs/2.1/Performance-Tuning.html
  )

  private implicit lazy val db = {
    // http://orientdb.com/docs/2.1/Performance-Tuning.html

    // this means disabling transactions!
    // slows down mutations, but no commit overhead (the real killer)
    OGlobalConfiguration.USE_WAL.setValue(false)
    //OGlobalConfiguration.TX_USE_LOG.setValue(false)

    // no discernable difference
    //OGlobalConfiguration.ENVIRONMENT_CONCURRENT.setValue(false)
    //OGlobalConfiguration.DISK_WRITE_CACHE_PART.setValue(50)
    //OGlobalConfiguration.WAL_SYNC_ON_PAGE_FLUSH.setValue(false)
    //OGlobalConfiguration.DISK_CACHE_SIZE.setValue(7200)

    val url = "plocal:" + dir.getAbsolutePath
    val db = new OrientGraphFactory(url).setupPool(pools, pools)
    // The placement of this fix needs futher thought, if placed before actual GraphFactory creation it wil work for me locally,
    // but causes NoClassDefFound during orient initialization on testing machine@fommmil.com
    Orient.setRegisterDatabaseByPath(true)
    val g = db.getNoTx

    // is this just needed on schema creation or always?
    // https://github.com/orientechnologies/orientdb/issues/5322
    g.setUseLightweightEdges(true)
    g.setUseLog(false)

    g.shutdown()
    // small speedup, but increases chance of concurrency issues
    db.declareIntent(new OIntentMassiveInsert())

    db
  }

  def shutdown(): Future[Unit] = Future {
    db.close()
  }

  if (!dir.exists) {
    log.info("creating the graph database...")
    dir.mkdirs()

    val g = db.getNoTx
    val fqnSymbolClass = g.createVertexType("FqnSymbol")
    val memberSymbolClass = g.createVertexType("Member", fqnSymbolClass)
    fqnSymbolClass.createProperty("fqn", OType.STRING).setMandatory(true)
    fqnSymbolClass.createProperty("line", OType.INTEGER).setMandatory(false)
    fqnSymbolClass.createProperty("source", OType.STRING).setMandatory(false)

    g.createVertexFrom[ClassDef](superClass = Some(fqnSymbolClass))
    g.createVertexFrom[Method](superClass = Some(memberSymbolClass))
    g.createVertexFrom[Field](superClass = Some(memberSymbolClass))

    g.createEdge[DefinedIn.type]
      .createEdge[OwningClass.type]
      .createEdge[UsedIn.type]
      .createEdge[IsParent.type]
    g.createIndexOn[Vertex, FqnSymbol, String](IndexT.Unique)
    g.createIndexOn[Vertex, FileCheck, String](IndexT.Unique)

    g.shutdown()

    log.info("... created the graph database")
  }

  def knownFiles(): Future[Seq[FileCheck]] = withGraphAsync { implicit g =>
    RichGraph.allV[FileCheck]
  }

  def outOfDate(f: FileObject)(implicit vfs: EnsimeVFS): Future[Boolean] = withGraphAsync { implicit g =>
    RichGraph.readUniqueV[FileCheck, String](f.getName.getURI) match {
      case None => true
      case Some(v) => v.toDomain.changed
    }
  }

  def persist(check: FileCheck, symbols: Seq[(RawSymbol, SourceInfo)]): Future[Int] = withGraphAsync { implicit g =>
    if (symbols.isEmpty) 0
    else {
      val fileV = RichGraph.upsertV[FileCheck, String](check) // bad atomic behaviour

      symbols.foreach {
        case (s: RawClassfile, (file, path, source)) =>
          val classDef = ClassDef(s.name.fqnString, file, path, source, s.source.line, s.access)
          val classV = RichGraph.upsertV[ClassDef, String](classDef)
          RichGraph.insertE(classV, fileV, DefinedIn)
          // nulls are used, because these fields are not really optional and are guaranteed to be
          // set in the graph after the indexing is completed.
          val superClass = s.superClass.map(name => ClassDef(name.fqnString, null, null, None, None, null))
          val interfaces = s.interfaces.map(name => ClassDef(name.fqnString, null, null, None, None, null))
          (superClass.toList ::: interfaces).foreach { cdef =>
            val parentV = RichGraph.upsertV[ClassDef, String](cdef)
            RichGraph.insertE(classV, parentV, IsParent)
          }
        case (s: RawField, (_, _, source)) =>
          val field = Field(s.name.fqnString, Some(s.clazz.internalString), None, source, s.access)
          RichGraph.upsertV[Field, String](field)
        case (s: RawMethod, (_, _, source)) =>
          val method = Method(s.name.fqnString, s.line, source, s.access)
          val methodV = RichGraph.upsertV[Method, String](method)
        case (s: RawType, (_, _, source)) =>
          val field = Field(s.fqn, None, None, source, s.access)
          val fieldV = RichGraph.upsertV[Field, String](field)
      }
      symbols.size
    }
  }

  /**
   * Removes given `files` from the graph.
   */
  def removeFiles(files: List[FileObject]): Future[Int] = withGraphAsync { implicit g =>
    RichGraph.removeV(files.map(FileCheck(_)))
  }

  /**
   * Finds the FqnSymbol uniquely identified by `fqn`.
   */
  def find(fqn: String): Future[Option[FqnSymbol]] = withGraphAsync { implicit g =>
    RichGraph.readUniqueV[FqnSymbol, String](fqn).map(_.toDomain)
  }

  /**
   * Finds all FqnSymbol's identified by unique `fqns`.
   */
  def find(fqns: List[FqnIndex]): Future[List[FqnSymbol]] = withGraphAsync { implicit g =>
    fqns.flatMap(fqn =>
      RichGraph.readUniqueV[FqnSymbol, String](fqn.fqn).map(_.toDomain))
  }

  // NOTE: only commits this thread's work
  def commit(): Future[Unit] = withGraphAsync { implicit graph =>
    graph.commit() // transactions disabled, is this a no-op?
    graph.declareIntent(null)
  }

  def getClassHierarchy(fqn: String, hierarchyType: HierarchyType): Future[Option[Hierarchy]] = withGraphAsync { implicit g =>
    RichGraph.classHierarchy[String](fqn, hierarchyType)
  }
}

object GraphService {
  private[indexer] case object DefinedIn extends EdgeT[ClassDef, FileCheck]
  private[indexer] case object OwningClass extends EdgeT[Member, ClassDef]
  private[indexer] case object UsedIn extends EdgeT[Member, FqnSymbol]
  private[indexer] case object IsParent extends EdgeT[ClassDef, ClassDef]
}
