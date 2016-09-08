// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.indexer

import java.util.concurrent.Semaphore

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{ Failure, Properties, Success }
import scala.collection.{ mutable => m, Map, Set }
import akka.actor._
import akka.event.slf4j.SLF4JLogging
import org.apache.commons.vfs2._
import org.ensime.api._
import org.ensime.indexer.graph._
import org.ensime.util.file._
import org.ensime.util.fileobject._
import org.ensime.vfs._

/**
 * Provides methods to perform ENSIME-specific indexing tasks,
 * receives events that require an index update, and provides
 * searches against the index.
 *
 * We have an H2 database for storing relational information
 * and Lucene for advanced indexing.
 */
class SearchService(
  config: EnsimeConfig,
  resolver: SourceResolver
)(
  implicit
  actorSystem: ActorSystem,
  vfs: EnsimeVFS
) extends ClassfileIndexer
    with FileChangeListener
    with SLF4JLogging {
  import SearchService._

  private[indexer] val allTargets = config.allTargets.map(vfs.vfile)

  private[indexer] def isUserFile(file: FileName): Boolean = allTargets.exists(file isAncestor _.getName)

  private val QUERY_TIMEOUT = 30 seconds

  private[indexer] val ensimeVfs = vfs

  /**
   * Changelog:
   *
   * 2.3.1g - bump schema for Lucene analyser
   *
   * 2.3g - persist reverse lookups info
   *
   * 2.2g - persist scalap information (scala names, type sigs, etc)
   *
   * 2.1g - remodel OrientDB schema with new domain objects
   *
   * 2.0 - upgrade Lucene, format not backwards compatible.
   *
   * 1.4 - remove redundant descriptors, doh!
   *
   * 1.3g - use a graph database
   *
   * 1.3 - methods include descriptors in (now unique) FQNs
   *
   * 1.2 - added foreign key to FqnSymbols.file with cascade delete
   *
   * 1.0 - reverted index due to negative impact to startup time. The
   *       workaround to large scale deletions is to just nuke the
   *       .ensime_cache.
   *
   * 1.1 - added index to FileCheck.file to speed up delete.
   *
   * 1.0 - initial schema
   */
  private val version = "2.3.1"

  private val index = new IndexService((config.cacheDir / ("index-" + version)).toPath)
  private val db = new GraphService(config.cacheDir / ("graph-" + version))

  import ExecutionContext.Implicits.global

  // each jar / directory must acquire a permit, released when the
  // data is persisted. This is to keep the heap usage down and is a
  // poor man's backpressure.
  val semaphore = new Semaphore(Properties.propOrElse("ensime.index.parallel", "10").toInt, true)

  def noReverseLookups: Boolean = Properties.propOrFalse("ensime.index.no.reverse.lookups")

  private[indexer] def getTopLevelClassFile(f: FileObject): FileObject = {
    import scala.reflect.NameTransformer
    val filename = f.getName
    val className = filename.getBaseName
    val baseClassName =
      if (className.contains("$")) NameTransformer.encode(NameTransformer.decode(className).split("\\$")(0)) + ".class"
      else className
    vfs.vfile(filename.getURI.substring(0, filename.getURI.length - className.length) + baseClassName)
  }

  private def scanGrouped(
    f: FileObject,
    initial: m.MultiMap[FileName, FileObject] = new m.HashMap[FileName, m.Set[FileObject]] with m.MultiMap[FileName, FileObject]
  ): Map[FileName, Set[FileObject]] = f.findFiles(ClassfileSelector) match {
    case null => initial
    case res =>
      for (fo <- res) {
        val key = getTopLevelClassFile(fo)
        if (key.exists()) {
          initial.addBinding(key.getName, fo)
        }
      }
      initial
  }

  /**
   * Indexes everything, making best endeavours to avoid scanning what
   * is unnecessary (e.g. we already know that a jar or classfile has
   * been indexed).
   *
   * @return the number of rows (removed, indexed) from the database.
   */
  def refresh(): Future[(Int, Int)] = {
    // it is much faster during startup to obtain the full list of
    // known files from the DB then and check against the disk, than
    // check each file against DatabaseService.outOfDate
    def findStaleFileChecks(checks: Seq[FileCheck]): List[FileCheck] = {
      log.debug("findStaleFileChecks")
      for {
        check <- checks
        name = check.file.getName.getURI
        if !check.file.exists || check.changed
      } yield check
    }.toList

    // delete the stale data before adding anything new
    // returns number of rows deleted
    def deleteReferences(checks: List[FileCheck]): Future[Int] = {
      log.debug(s"removing ${checks.size} stale files from the index")
      deleteInBatches(checks.map(_.file))
    }

    // a snapshot of everything that we want to index
    def findBases(): (Set[FileObject], Map[FileName, Set[FileObject]]) = {
      val grouped = new m.HashMap[FileName, m.Set[FileObject]] with m.MultiMap[FileName, FileObject]
      val jars = config.modules.flatMap {
        case (name, m) =>
          (m.testTargets ++ m.targets).flatMap {
            case d if !d.exists() => Nil
            case d if d.isJar => List(vfs.vfile(d))
            case d => scanGrouped(vfs.vfile(d), grouped); Nil
          } ::: (m.compileJars ++ m.testJars).map(vfs.vfile)
      }.toSet ++ config.javaLibs.map(vfs.vfile)
      (jars, grouped)
    }

    def indexBase(
      baseName: FileName,
      fileCheck: Option[FileCheck],
      grouped: Map[FileName, Set[FileObject]]
    ): Future[Int] = {
      val base = vfs.vfile(baseName.getURI)
      val outOfDate = fileCheck.forall(_.changed)
      if (!outOfDate || !base.exists()) Future.successful(0)
      else {
        val boost = isUserFile(baseName)
        val indexed = extractSymbolsFromClassOrJar(base, grouped).flatMap(persist(_, commitIndex = false, boost = boost))
        indexed.onComplete { _ =>
          if (base.getName.getExtension == "jar") {
            log.debug(s"finished indexing $base")
          }
          semaphore.release()
        }
        indexed
      }
    }

    // index all the given bases and return number of rows written
    def indexBases(files: (Set[FileObject], Map[FileName, Set[FileObject]]), checks: Seq[FileCheck]): Future[Int] = {
      val (jars, classFiles) = files
      log.debug("Indexing bases...")
      val checksLookup: Map[String, FileCheck] = checks.map(check => (check.filename -> check)).toMap
      val jarsWithChecks = jars.map { jar =>
        val name = jar.getName
        (name, checksLookup.get(name.getURI))
      }
      val basesWithChecks: Seq[(FileName, Option[FileCheck])] = classFiles.map {
        case (outerClassFile, _) =>
          (outerClassFile, checksLookup.get(outerClassFile.getURI))
      }(collection.breakOut)

      Future.sequence(
        (jarsWithChecks ++ basesWithChecks).collect { case (file, check) => indexBase(file, check, classFiles) }
      ).map(_.sum)
    }

    def commitIndex(): Future[Unit] = {
      log.debug("committing index to disk...")
      val i = Future { blocking { index.commit() } }
      val g = db.commit()
      for {
        _ <- i
        _ <- g
      } yield {
        log.debug("...done committing index")
      }
    }

    // chain together all the future tasks
    for {
      checks <- db.knownFiles()
      stale = findStaleFileChecks(checks)
      deletes <- deleteReferences(stale)
      bases = findBases()
      added <- indexBases(bases, checks)
      _ <- commitIndex()
    } yield (deletes, added)
  }

  def refreshResolver(): Unit = resolver.update()

  def persist(symbols: List[SourceSymbolInfo], commitIndex: Boolean, boost: Boolean): Future[Int] = {
    val iwork = Future { blocking { index.persist(symbols, commitIndex, boost) } }
    val dwork = db.persist(symbols)
    iwork.flatMap { _ => dwork }
  }

  // this method leak semaphore on every call, which must be released
  // when the List[FqnSymbol] has been processed (even if it is empty)
  def extractSymbolsFromClassOrJar(
    file: FileObject,
    grouped: Map[FileName, Set[FileObject]]
  ): Future[List[SourceSymbolInfo]] = {
    def global: ExecutionContext = null // detach the global implicit
    val ec = actorSystem.dispatchers.lookup("akka.search-service-dispatcher")

    Future {
      blocking {
        semaphore.acquire()

        file match {
          case classfile if classfile.getName.getExtension == "class" =>
            // too noisy to log
            val files = grouped(classfile.getName)
            try extractSymbols(classfile, files, classfile)
            finally { files.foreach(_.close()); classfile.close() }
          case jar =>
            log.debug(s"indexing $jar")
            val check = FileCheck(jar)
            val vJar = vfs.vjar(jar)
            try { (scanGrouped(vJar) flatMap { case (root, files) => extractSymbols(jar, files, vfs.vfile(root.getURI)) }).toList }
            finally { vfs.nuke(vJar) }
        }
      }
    }(ec)
  }

  private val blacklist = Set("sun/", "sunw/", "com/sun/")
  private val ignore = Set("$$", "$worker$")
  private def extractSymbols(
    container: FileObject,
    files: collection.Set[FileObject],
    rootClassFile: FileObject
  ): List[SourceSymbolInfo] = {
    def getInternalRefs(isUserFile: Boolean, s: RawSymbol): Set[FullyQualifiedName] = if (isUserFile && !noReverseLookups) s.internalRefs else Set.empty

    val depickler = new ClassfileDepickler(rootClassFile)
    val name = container.getName.getURI
    val scalapClasses = depickler.getClasses

    files.flatMap { f =>
      f.pathWithinArchive match {
        case Some(relative) if blacklist.exists(relative.startsWith) => Nil
        case _ =>
          val path = f.getName.getURI
          val file = if (path.startsWith("jar") || path.startsWith("zip")) {
            FileCheck(container)
          } else FileCheck(f)
          val clazz = indexClassfile(f)
          val userFile = isUserFile(f.getName)
          val source = resolver.resolve(clazz.name.pack, clazz.source)
          val sourceUri = source.map(_.getName.getURI)
          val scalapClassInfo = scalapClasses.get(clazz.name.fqnString)

          scalapClassInfo match {
            case _ if clazz.access != Public => Nil
            case None if clazz.isScala => List(SourceSymbolInfo(file, path, None, Set.empty, None, None))
            case Some(scalapSymbol) =>
              val classInfo = SourceSymbolInfo(file, path, sourceUri, getInternalRefs(userFile, clazz), Some(clazz), Some(scalapSymbol))
              val fields = clazz.fields.map(f =>
                SourceSymbolInfo(file, path, sourceUri, getInternalRefs(userFile, f), Some(f), scalapSymbol.fields.get(f.fqn)))
              val methods = clazz.methods.groupBy(_.name.name).flatMap {
                case (methodName, overloads) =>
                  val scalapMethods = scalapSymbol.methods.get(methodName)
                  overloads.iterator.zipWithIndex.map {
                    case (m, i) =>
                      val scalap = scalapMethods.fold(Option.empty[RawScalapMethod])(seq =>
                        if (seq.length <= i) None else Some(seq(i)))
                      SourceSymbolInfo(file, path, sourceUri, getInternalRefs(userFile, m), Some(m), scalap)
                  }
              }
              val aliases = scalapSymbol.typeAliases.valuesIterator.map(alias =>
                SourceSymbolInfo(file, path, sourceUri, Set.empty, None, Some(alias))).toList
              classInfo :: fields ::: methods.toList ::: aliases
            case None =>
              (clazz :: clazz.methods.toList ::: clazz.fields)
                .map(s => SourceSymbolInfo(file, path, sourceUri, getInternalRefs(userFile, s), Some(s)))
          }
      }
    }.filterNot(sym => ignore.exists(sym.fqn.contains))
  }.toList

  /** free-form search for classes */
  def searchClasses(query: String, max: Int): List[FqnSymbol] = {
    val fqns = index.searchClasses(query, max)
    Await.result(db.find(fqns), QUERY_TIMEOUT) take max
  }

  /** free-form search for classes and methods */
  def searchClassesMethods(terms: List[String], max: Int): List[FqnSymbol] = {
    val fqns = index.searchClassesMethods(terms, max)
    Await.result(db.find(fqns), QUERY_TIMEOUT) take max
  }

  /** only for exact fqns */
  def findUnique(fqn: String): Option[FqnSymbol] = Await.result(db.find(fqn), QUERY_TIMEOUT)

  /** returns hierarchy of a type identified by fqn */
  def getTypeHierarchy(fqn: String, hierarchyType: Hierarchy.Direction): Future[Option[Hierarchy]] = db.getClassHierarchy(fqn, hierarchyType)

  /** returns FqnSymbol which reference given fqn */
  def findUsages(fqn: String): Future[Iterable[FqnSymbol]] = db.findUsages(fqn)

  /* DELETE then INSERT in H2 is ridiculously slow, so we put all modifications
   * into a blocking queue and dedicate a thread to block on draining the queue.
   * This has the effect that we always react to a single change on disc but we
   * will work through backlogs in bulk.
   *
   * We always do a DELETE, even if the entries are new, but only INSERT if
   * the list of symbols is non-empty.
   */

  val backlogActor = actorSystem.actorOf(Props(new IndexingQueueActor(this)), "ClassfileIndexer")

  // deletion in both Lucene and H2 is really slow, batching helps
  def deleteInBatches(
    files: List[FileObject],
    batchSize: Int = 1000
  ): Future[Int] = {
    val removing = files.grouped(batchSize).map(delete)
    Future.sequence(removing).map(_.sum)
  }

  // returns number of rows removed
  def delete(files: List[FileObject]): Future[Int] = {
    // this doesn't speed up Lucene deletes, but it means that we
    // don't wait for Lucene before starting the H2 deletions.
    val iwork = Future { blocking { index.remove(files) } }
    val dwork = db.removeFiles(files)
    iwork.flatMap(_ => dwork)
  }

  def fileChanged(f: FileObject): Unit = backlogActor ! IndexFile(f)
  def fileRemoved(f: FileObject): Unit = fileChanged(f)
  def fileAdded(f: FileObject): Unit = fileChanged(f)

  def shutdown(): Future[Unit] = {
    db.shutdown()
  }
}

object SearchService {
  case class SourceSymbolInfo(
      file: FileCheck,
      path: String,
      source: Option[String],
      usageInfo: Set[FullyQualifiedName],
      bytecodeSymbol: Option[RawSymbol],
      scalapSymbol: Option[RawScalapSymbol] = None
  ) {
    def fqn: String = (bytecodeSymbol, scalapSymbol) match {
      case (Some(bytecode), _) => bytecode.fqn
      case (None, Some(t: RawType)) => t.javaName.fqnString
      case _ => ""
    }
  }
}

final case class IndexFile(f: FileObject)

class IndexingQueueActor(searchService: SearchService) extends Actor with ActorLogging {
  import context.system

  import scala.concurrent.duration._

  case object Process

  // De-dupes files that have been updated since we were last told to
  // index them. No need to aggregate values: the latest wins. Key is
  // the URI because FileObject doesn't implement equals
  private val todo = new m.HashMap[FileName, m.Set[FileObject]] with m.MultiMap[FileName, FileObject]

  private val vfs = searchService.ensimeVfs

  // debounce and give us a chance to batch (which is *much* faster)
  private var worker: Cancellable = _

  private val advice = "If the problem persists, you may need to restart ensime."

  private def debounce(): Unit = {
    Option(worker).foreach(_.cancel())
    import context.dispatcher
    worker = system.scheduler.scheduleOnce(5 seconds, self, Process)
  }

  override def receive: Receive = {
    case IndexFile(f) =>
      val topLevelClassFile = f match {
        case jar if jar.getName.getExtension == "jar" => jar
        case classFile => searchService.getTopLevelClassFile(classFile)
      }
      todo.addBinding(topLevelClassFile.getName, topLevelClassFile)
      debounce()

    case Process if todo.isEmpty => // nothing to do

    case Process =>
      val batch = todo.take(250)
      batch.keys.foreach(todo.remove)
      if (todo.nonEmpty)
        debounce()

      import ExecutionContext.Implicits.global

      log.debug(s"Indexing ${batch.size} groups of files")

      def retry(): Unit = {
        batch.valuesIterator.foreach(_.foreach(self ! IndexFile(_)))
      }

      batch.grouped(10).foreach(chunk => Future.sequence(chunk.map {
        case (outerClassFile, _) =>
          val filename = outerClassFile.getPath
          // I don't trust VFS's f.exists()
          if (!File(filename).exists()) {
            Future {
              searchService.semaphore.acquire() // nasty, but otherwise we leak
              outerClassFile -> Nil
            }
          } else searchService.extractSymbolsFromClassOrJar(vfs.vfile(outerClassFile.getURI), batch).map(outerClassFile -> )
      }).onComplete {
        case Failure(t) =>
          searchService.semaphore.release()
          log.error(t, s"failed to index batch of ${batch.size} files. $advice")
          retry()
        case Success(indexed) =>
          searchService.delete(indexed.flatMap(f => batch(f._1))(collection.breakOut)).onComplete {
            case Failure(t) =>
              searchService.semaphore.release()
              log.error(t, s"failed to remove stale entries in ${batch.size} files. $advice")
              retry()
            case Success(_) => indexed.foreach {
              case (file, syms) =>
                val boost = searchService.isUserFile(file)
                val persisting = searchService.persist(syms, commitIndex = true, boost = boost)

                persisting.onComplete {
                  case _ => searchService.semaphore.release()
                }

                persisting.onComplete {
                  case Failure(t) =>
                    log.error(t, s"failed to persist entries in $file. $advice")
                    retry()
                  case Success(_) =>
                }
            }
          }

      })
  }

}
