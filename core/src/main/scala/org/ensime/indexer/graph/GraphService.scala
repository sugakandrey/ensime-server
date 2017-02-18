// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.indexer.graph

import java.sql.Timestamp
import java.util.concurrent.{ Executors, TimeUnit }

import scala.Predef.{ any2stringadd => _, _ }
import scala.concurrent._
import scala.collection.mutable
import akka.event.slf4j.SLF4JLogging
import com.orientechnologies.orient.core.Orient
import com.orientechnologies.orient.core.config.OGlobalConfiguration
import com.orientechnologies.orient.core.db.ODatabaseRecordThreadLocal
import com.orientechnologies.orient.core.intent.OIntentMassiveInsert
import com.orientechnologies.orient.core.metadata.schema.OType
import com.tinkerpop.blueprints.Vertex
import com.tinkerpop.blueprints.impls.orient.OrientGraphFactory
import org.apache.commons.vfs2.FileObject
import org.ensime.api.DeclaredAs
import org.ensime.indexer.IndexService.FqnIndex
import org.ensime.indexer.SearchService._
import org.ensime.indexer._
import org.ensime.indexer.orientdb.api._
import org.ensime.indexer.orientdb.syntax._
import org.ensime.indexer.orientdb.schema.api._
import org.ensime.util.file._
import org.ensime.vfs._
import shapeless.cachedImplicit

sealed trait FqnSymbol {
  def fqn: String
  def line: Option[Int]
  def source: Option[String]
  def declAs: DeclaredAs
  def access: Access
  def scalaName: Option[String]

  def sourceFileObject(implicit vfs: EnsimeVFS): Option[FileObject] = source.map(vfs.vfile)
  def toSearchResult: String = s"$declAs ${scalaName.getOrElse(fqn)}"
}

object FqnSymbol {
  private[graph] def fromFullyQualifiedName(name: FullyQualifiedName): Option[FqnSymbol] = name match {
    case cn: ClassName if !cn.isPrimitive => Some(ClassDef(name.fqnString, null, null, None, None, null, None, None))
    case fn: FieldName => Some(Field(name.fqnString, None, None, None, null, None))
    case mn: MethodName => Some(Method(name.fqnString, None, None, null, None))
    case _ => None
  }
}

sealed trait Hierarchy
object Hierarchy {
  sealed trait Direction
  case object Supertypes extends Direction
  case object Subtypes extends Direction
}

final case class TypeHierarchy(aClass: ClassDef, typeRefs: Seq[Hierarchy]) extends Hierarchy

final case class ClassDef(
    fqn: String,
    file: String, // the underlying file
    path: String, // the VFS handle (e.g. classes in jars)
    source: Option[String],
    line: Option[Int],
    access: Access,
    scalaName: Option[String],
    scalapDeclaredAs: Option[DeclaredAs]
) extends FqnSymbol with Hierarchy {
  override def declAs: DeclaredAs = scalapDeclaredAs.getOrElse(DeclaredAs.Class)
}

sealed trait Member extends FqnSymbol

final case class Field(
    fqn: String,
    internal: Option[String],
    line: Option[Int],
    source: Option[String],
    access: Access,
    scalaName: Option[String]
) extends Member {
  override def declAs: DeclaredAs = DeclaredAs.Field
}

final case class Method(
    fqn: String,
    line: Option[Int],
    source: Option[String],
    access: Access,
    scalaName: Option[String]
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

  def fromPath(path: String)(implicit vfs: EnsimeVFS): FileCheck = apply(vfs.vfile(path))
}

// core/it:test-only *Search* -- -z prestine
class GraphService(dir: File) extends SLF4JLogging {
  import org.ensime.indexer.graph.GraphService._

  // all methods return Future, which means we can do isolation by
  // doing all work on a single worker Thread. We can't optimise until
  // we better understand the concurrency of our third party
  // libraries.
  private val pools = 1
  // WARNING: Faster, but needs further thought
  //Executors.newFixedThreadPool(pools)
  // http://orientdb.com/docs/2.1/Performance-Tuning.html
  private val executor = Executors.newSingleThreadExecutor()
  private implicit val ec = ExecutionContext.fromExecutor(executor)

  private implicit lazy val db: OrientGraphFactory = {
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

    //This is a hack, that resolves some classloading issues in OrientDB.
    //https://github.com/orientechnologies/orientdb/issues/5146
    if (ODatabaseRecordThreadLocal.INSTANCE == null) {
      sys.error("Calling this manually apparently prevent an initialization issue.")
    }
    Orient.setRegisterDatabaseByPath(true)

    val url = "plocal:" + dir.getAbsolutePath
    val db = new OrientGraphFactory(url).setupPool(pools, pools)
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
    blocking {
      try {
        executor.shutdownNow()
        executor.awaitTermination(30, TimeUnit.SECONDS)
        ()
      } finally db.close()
    }
  }(ExecutionContext.Implicits.global)

  if (!dir.exists) {
    log.info("creating the graph database...")
    dir.mkdirs()

    val g = db.getNoTx
    val fqnSymbolClass = g.createVertexType("FqnSymbol")
    fqnSymbolClass.createProperty("fqn", OType.STRING).setMandatory(true)
    fqnSymbolClass.createProperty("line", OType.INTEGER).setMandatory(false)
    fqnSymbolClass.createProperty("source", OType.STRING).setMandatory(false)
    val memberSymbolClass = g.createVertexType("Member", fqnSymbolClass)

    g.createVertexFrom[ClassDef](superClass = Some(fqnSymbolClass))
    g.createVertexFrom[Method](superClass = Some(memberSymbolClass))
    g.createVertexFrom[Field](superClass = Some(memberSymbolClass))
    g.createVertexFrom[FileCheck]()

    g.createEdge[DefinedIn.type]
      .createEdge[EnclosingClass.type]
      .createEdge[UsedIn.type]
      .createEdge[IsParent.type]
    g.createIndexOn[Vertex, FqnSymbol, String](Unique)
    g.createIndexOn[Vertex, FileCheck, String](Unique)

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

  def persist(symbols: Seq[SourceSymbolInfo]): Future[Int] = withGraphAsync { implicit g =>
    val checks = mutable.Map.empty[String, VertexT[FileCheck]]
    val classes = mutable.Map.empty[String, VertexT[ClassDef]]

    symbols.foreach { s =>
      val scalaName = s.scalapSymbol.map(_.scalaName)
      val typeSignature = s.scalapSymbol.map(_.typeSignature)
      val declAs = s.scalapSymbol.map(_.declaredAs)

      val vertex = s match {
        case EmptySourceSymbolInfo(fileCheck) =>
          if (!checks.contains(fileCheck.filename)) {
            RichGraph.upsertV[FileCheck, String](fileCheck)
          }
          None

        case ClassSymbolInfo(fileCheck, path, source, refs, bs, scalap) =>
          val classDef = ClassDef(bs.fqn, fileCheck.filename, path, source, bs.source.line, bs.access, scalaName, declAs)

          val fileV = checks.getOrElse(fileCheck.filename, RichGraph.upsertV[FileCheck, String](fileCheck))
          val classV = RichGraph.upsertV[ClassDef, String](classDef)
          classes += (bs.fqn -> classV)
          RichGraph.insertE(classV, fileV, DefinedIn)
          val superClass = bs.superClass.map(
            name => ClassDef(name.fqnString, null, null, None, None, null, None, None)
          )
          val interfaces = bs.interfaces.map(
            name => ClassDef(name.fqnString, null, null, None, None, null, None, None)
          )
          (superClass.toList ::: interfaces).foreach { cdef =>
            val parentV = RichGraph.insertIfNotExists[ClassDef, String](cdef)
            RichGraph.insertE(classV, parentV, IsParent)
          }
          Some(classV)

        case MethodSymbolInfo(_, source, refs, bs, scalap) =>
          val owner = classes(bs.name.owner.fqnString)
          val method = Method(s.fqn, bs.line, source, bs.access, (scalaName ++ typeSignature).reduceOption(_ + _))
          val methodV: VertexT[FqnSymbol] = RichGraph.upsertV[Method, String](method)
          RichGraph.insertE(methodV, owner, EnclosingClass)
          Some(methodV)

        case FieldSymbolInfo(_, source, refs, bs, scalap) =>
          val owner = classes(bs.name.owner.fqnString)
          val field = Field(bs.name.fqnString, Some(s.fqn), None, source, bs.access, scalaName)
          val fieldV: VertexT[FqnSymbol] = RichGraph.upsertV[Field, String](field)
          RichGraph.insertE(fieldV, owner, EnclosingClass)
          Some(fieldV)

        case TypeAliasSymbolInfo(_, source, t) =>
          val owner = classes(t.owner.fqnString)
          val field = Field(s.fqn, None, None, source, t.access, Some(t.scalaName + t.typeSignature))
          val fieldV: VertexT[FqnSymbol] = RichGraph.upsertV[Field, String](field)
          RichGraph.insertE(fieldV, owner, EnclosingClass)
          Some(fieldV)
      }
      s.internalRefs.foreach { ref =>
        val sym = FqnSymbol.fromFullyQualifiedName(ref)
        val usage: Option[VertexT[FqnSymbol]] = sym.map {
          case cd: ClassDef => RichGraph.insertIfNotExists[ClassDef, String](cd)
          case m: Method => RichGraph.insertIfNotExists[Method, String](m)
          case f: Field => RichGraph.insertIfNotExists[Field, String](f)
        }
        for {
          u <- usage
          v <- vertex
        } yield RichGraph.insertE(u, v, UsedIn)
      }
    }
    symbols.collect {
      case c: ClassSymbolInfo => c.bytecodeSymbol.innerClasses.foreach { inner =>
        for {
          innerClassV: VertexT[FqnSymbol] <- classes.get(inner.fqnString)
          outerClassV <- classes.get(c.fqn)
        } yield {
          RichGraph.insertE(innerClassV, outerClassV, EnclosingClass)
          classes.get(s"${c.fqn}$$").foreach(RichGraph.insertE(innerClassV, _, EnclosingClass))
        }
      }
    }

    symbols.size
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

  def getClassHierarchy(fqn: String, hierarchyType: Hierarchy.Direction): Future[Option[Hierarchy]] = withGraphAsync { implicit g =>
    RichGraph.classHierarchy[String](fqn, hierarchyType)
  }

  def findUsages(fqn: String): Future[Iterable[FqnSymbol]] = withGraphAsync { implicit g =>
    RichGraph.findUsages[String](fqn).map(_.toDomain)
  }
}

object GraphService {
  private[indexer] case object DefinedIn extends EdgeT[ClassDef, FileCheck]
  private[indexer] case object EnclosingClass extends EdgeT[FqnSymbol, ClassDef]
  private[indexer] case object UsedIn extends EdgeT[FqnSymbol, FqnSymbol]
  private[indexer] case object IsParent extends EdgeT[ClassDef, ClassDef]

  // the domain-specific formats for schema generation
  import org.ensime.indexer.orientdb.schema.impl._
  import org.ensime.util.stringymap.api._
  import org.ensime.util.stringymap.impl._

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

    def fromValue(v: AnyRef): Either[String, Access] = IntSPrimitive.fromValue(v).right.map(Access(_))
  }

  implicit object DeclaredAsSPrimitive extends SPrimitive[DeclaredAs] {
    import org.ensime.util.enums._
    import SPrimitive.StringSPrimitive
    private val lookup: Map[String, DeclaredAs] = implicitly[AdtToMap[DeclaredAs]].lookup
    def toValue(v: DeclaredAs): java.lang.String = if (v == null) null else StringSPrimitive.toValue(v.toString)
    def fromValue(v: AnyRef): Either[String, DeclaredAs] = StringSPrimitive.fromValue(v).right.map(lookup)
  }

  implicit val FileCheckBdf: BigDataFormat[FileCheck] = cachedImplicit
  implicit val FileCheckS: SchemaFormat[FileCheck] = cachedImplicit

  implicit val ClassDefBdf: BigDataFormat[ClassDef] = cachedImplicit
  implicit val ClassDefS: SchemaFormat[ClassDef] = cachedImplicit

  implicit val MethodBdf: BigDataFormat[Method] = cachedImplicit
  implicit val MethodS: SchemaFormat[Method] = cachedImplicit

  implicit val FieldBdf: BigDataFormat[Field] = cachedImplicit
  implicit val FieldS: SchemaFormat[Field] = cachedImplicit

  implicit val FqnSymbolBdf: BigDataFormat[FqnSymbol] = cachedImplicit

  implicit val DefinedInS: BigDataFormat[DefinedIn.type] = cachedImplicit
  implicit val EnclosingClassS: BigDataFormat[EnclosingClass.type] = cachedImplicit
  implicit val UsedInS: BigDataFormat[UsedIn.type] = cachedImplicit
  implicit val IsParentS: BigDataFormat[IsParent.type] = cachedImplicit

  implicit val UniqueFileCheckV: OrientIdFormat[FileCheck, String] =
    new OrientIdFormat[FileCheck, String] {
      override def key = "filename"
      override def value(t: FileCheck): String = t.filename
    }

  implicit val FqnIndexV: OrientIdFormat[FqnIndex, String] =
    new OrientIdFormat[FqnIndex, String] {
      override def key = "fqn"
      override def value(t: FqnIndex): String = t.fqn
    }

  class UniqueFqnSymbol[T <: FqnSymbol] extends OrientIdFormat[T, String] {
    override def key = "fqn"
    override def value(t: T): String = t.fqn
  }

  implicit val UniqueClassDefV: UniqueFqnSymbol[ClassDef] = new UniqueFqnSymbol[ClassDef]
  implicit val UniqueMethodV: UniqueFqnSymbol[Method] = new UniqueFqnSymbol[Method]
  implicit val UniqueFieldV: UniqueFqnSymbol[Field] = new UniqueFqnSymbol[Field]
  implicit val UniqueFqnSymbolV: UniqueFqnSymbol[FqnSymbol] = new UniqueFqnSymbol[FqnSymbol]

}
