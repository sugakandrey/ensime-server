// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.indexer.graph

import java.sql.Timestamp
import java.util.concurrent.Executors

import scala.Predef.{ any2stringadd => _, _ }
import scala.concurrent._

import akka.event.slf4j.SLF4JLogging
import com.orientechnologies.orient.core.config.OGlobalConfiguration
import com.orientechnologies.orient.core.intent.OIntentMassiveInsert
import com.orientechnologies.orient.core.metadata.schema.OType
import com.tinkerpop.blueprints.Vertex
import com.tinkerpop.blueprints.impls.orient.OrientGraphFactory
import org.apache.commons.vfs2.FileObject
import org.ensime.api.DeclaredAs
import org.ensime.indexer.IndexService.FqnIndex
import org.ensime.indexer.orientdb.api._
import org.ensime.indexer.orientdb.syntax._
import org.ensime.indexer.stringymap.api._
import org.ensime.indexer.stringymap.impl._
import org.ensime.util.file._
import org.ensime.vfs._
import shapeless.cachedImplicit

// FIXME: these domain objects are remnants of the DatabaseService and should be remodelled
final case class FileCheck(filename: String, timestamp: Timestamp) {
  def file(implicit vfs: EnsimeVFS) = vfs.vfile(filename)
  def lastModified = timestamp.getTime
  def changed(implicit vfs: EnsimeVFS) = file.getContent.getLastModifiedTime != lastModified
}
object FileCheck extends ((String, Timestamp) => FileCheck) {
  def apply(f: FileObject): FileCheck = {
    val name = f.getName.getURI
    val ts = new Timestamp(f.getContent.getLastModifiedTime)
    FileCheck(name, ts)
  }
}
final case class FqnSymbol(
    id: Option[Int],
    file: String, // the underlying file
    path: String, // the VFS handle (e.g. classes in jars)
    fqn: String,
    descriptor: Option[String], // for methods
    internal: Option[String], // for fields
    source: Option[String], // VFS
    line: Option[Int],
    offset: Option[Int] = None // future features:
//    type: ??? --- better than descriptor/internal
) {
  // this is just as a helper until we can use more sensible
  // domain objects with slick
  def sourceFileObject(implicit vfs: EnsimeVFS) = source.map(vfs.vfile)

  // legacy: note that we can't distinguish class/trait
  def declAs: DeclaredAs =
    if (descriptor.isDefined) DeclaredAs.Method
    else if (internal.isDefined) DeclaredAs.Field
    else DeclaredAs.Class
}

// core/it:test-only *Search* -- -z prestine
class GraphService(dir: File) extends SLF4JLogging {

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

    val g = db.getNoTx()

    // is this just needed on schema creation or always?
    // https://github.com/orientechnologies/orientdb/issues/5322
    g.setUseLightweightEdges(true)
    g.setUseLog(false)

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

    val g = db.getNoTx()
    val schema = g.getRawGraph().getMetadata().getSchema()

    // TODO: typesafe syntax to support schema creation and
    //       indexed/unique vertices and edges
    g.createVertexType("FqnSymbol")
    val fqnSymbol = schema.getClass("FqnSymbol")
    g.createVertexType("FileCheck")
    val fileCheck = schema.getClass("FileCheck")

    fqnSymbol.createProperty("file", OType.STRING)
    fqnSymbol.createProperty("timestamp", OType.LONG)
    g.createKeyIndex("fqn", classOf[Vertex],
      "type" -> "UNIQUE",
      "class" -> "FqnSymbol")
    // TODO unsure if this is used
    // http://stackoverflow.com/questions/33722046
    fqnSymbol.createProperty("FileCheck", OType.LINK, fileCheck)

    fileCheck.createProperty("file", OType.STRING)
    fileCheck.createProperty("path", OType.STRING)
    fileCheck.createProperty("fqn", OType.STRING)
    fileCheck.createProperty("descriptor", OType.STRING)
    fileCheck.createProperty("internal", OType.STRING)
    fileCheck.createProperty("source", OType.STRING)
    fileCheck.createProperty("line", OType.INTEGER)
    fileCheck.createProperty("offset", OType.INTEGER)
    g.createKeyIndex("filename", classOf[Vertex],
      "type" -> "UNIQUE",
      "class" -> "FileCheck")

    g.createEdgeType("DefinedIn")

    log.info("... created the graph database")
  }

  implicit object TimeStampSPrimitive extends SPrimitive[Timestamp] {
    import SPrimitive.LongSPrimitive
    def toValue(v: Timestamp): java.lang.Long = LongSPrimitive.toValue(v.getTime)
    def fromValue(v: AnyRef): Timestamp = new Timestamp(LongSPrimitive.fromValue(v))
  }

  case object DefinedIn extends EdgeT[FqnSymbol, FileCheck]

  implicit val FileCheckS: BigDataFormat[FileCheck] = cachedImplicit
  implicit val FqnSymbolS: BigDataFormat[FqnSymbol] = cachedImplicit
  implicit val DefinedInS: BigDataFormat[DefinedIn.type] = cachedImplicit

  def knownFiles(): Future[Seq[FileCheck]] = withGraphAsync { implicit g =>
    RichGraph.allV[FileCheck]
  }

  import shapeless._
  implicit val UniqueFileCheckV = LensId("filename", lens[FileCheck] >> 'filename)
  implicit val UniqueFqnSymbolV = LensId("fqn", lens[FqnSymbol] >> 'fqn)

  def outOfDate(f: FileObject)(implicit vfs: EnsimeVFS): Future[Boolean] = withGraphAsync { implicit g =>
    RichGraph.readUniqueV[FileCheck, String](f.getName.getURI) match {
      case None => true
      case Some(v) => v.toDomain.changed
    }
  }

  def persist(check: FileCheck, symbols: Seq[FqnSymbol]): Future[Option[Int]] = withGraphAsync { implicit g =>

    val fileV = RichGraph.upsertV[FileCheck, String](check) // bad atomic behaviour

    // TODO
    // - delete incoming FileCheck links
    // - delete outgoing FqnSymbol links (don't delete vertices)

    symbols.foreach { sym =>
      val symV = RichGraph.upsertV[FqnSymbol, String](sym)
      RichGraph.insertE(symV, fileV, DefinedIn)
    }

    Some(symbols.size)
  }

  def removeFiles(files: List[FileObject]): Future[Int] = Future { ??? }

  def find(fqn: String): Future[Option[FqnSymbol]] = Future { ??? }
  def find(fqns: List[FqnIndex]): Future[List[FqnSymbol]] = Future { ??? }

  // NOTE: only commits this thread's work
  def commit(): Future[Unit] = withGraphAsync { implicit graph =>
    graph.commit() // transactions disabled, is this a no-op?
    graph.declareIntent(null)
  }
}
