// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.indexer.lucene

import java.nio.file.{ Files, Path }
import java.util.concurrent.{ Executors, TimeUnit }

import akka.event.slf4j.SLF4JLogging
import org.apache.lucene.analysis.Analyzer
import org.apache.lucene.analysis.core._
import org.apache.lucene.analysis.miscellaneous._
import org.apache.lucene.document._
import org.apache.lucene.index._
import org.apache.lucene.search._
import org.apache.lucene.store.FSDirectory
import org.apache.lucene.util._

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.concurrent.{ ExecutionContext, Future, blocking }

object SimpleLucene {
  // from DirectDocValuesFormat.MAX_SORTED_SET_ORDS = 2147483391 but
  // it's kind of an insane number, so let's pick something halfway
  // sensible.
  val MaxResults = 10000

  // our fallback analyzer
  class LowercaseAnalyzer extends Analyzer {
    override protected def createComponents(fieldName: String) = {
      val source = new KeywordTokenizer()
      val filtered = new LowerCaseFilter(source)
      new Analyzer.TokenStreamComponents(source, filtered)
    }
  }
}

/**
 * Lightweight convenience wrapper over Lucene that does some sanity
 * checking, sets up per-field `Analyzer`s and gives access to
 * CRUD-like operations. Callers are expected to perform their own
 * marshalling and unmarshalling for Lucene's `Query` and `Document`
 * types.
 *
 * This class is thread safe. Only one instance is allowed **on the
 * operating system** (not just the JVM) for the same path. Lucene
 * manages a file lock to mitigate the risk of this happening.
 *
 * Technical note: Lucene is an excellent INDEX store, but is not
 * a database. Prefer using the DatabaseProvider where possible
 * and only fall back to using the index when SQL doesn't cut it.
 * Excellent examples of using an index are for creating multiple
 * representations of the same column, or for allowing allow/deny
 * filtering rules based on tags.
 */
class SimpleLucene(path: Path, analyzers: Map[String, Analyzer]) extends SLF4JLogging {
  import SimpleLucene._

  Files.createDirectories(path)

  private val executorService = Executors.newFixedThreadPool(8)
  private implicit val executionContext = ExecutionContext.fromExecutorService(executorService)

  // http://blog.thetaphi.de/2012/07/use-lucenes-mmapdirectory-on-64bit.html
  private val directory = FSDirectory.open(path)

  private val analyzer = new PerFieldAnalyzerWrapper(
    new LowercaseAnalyzer,
    analyzers.asJava
  )

  private val config = new IndexWriterConfig(analyzer)

  private val writer = new IndexWriter(directory, config)
  writer.commit() // puts a new directory into a valid state

  // nature of the beast is that this becomes stale
  private var lastReader = DirectoryReader.open(directory)
  private def reader() = {
    // non-atomic, but worth it to avoid blocking
    val latest = DirectoryReader.openIfChanged(lastReader)
    if (latest == null) lastReader
    else {
      lastReader = latest
      latest
    }
  }

  def search(query: Query, limit: Int): Future[List[Document]] = {
    val searcher = new IndexSearcher(reader(), executorService)

    val collector = TopScoreDocCollector.create(limit)

    Future {
      blocking {
        searcher.search(query, collector)

        val results = mutable.ListBuffer.empty[Document]
        for (hit <- collector.topDocs().scoreDocs.take(limit)) {
          val result = searcher.doc(hit.doc)
          results += result
          if (log.isTraceEnabled) {
            val explanation = searcher.explain(query, hit.doc)
            log.trace("" + result + " scored " + explanation)
          }
        }

        results.toList
      }
    }
  }

  /**
   * Lucene does not offer an out-of-the-box UPDATE, so we have to
   * manually DELETE then CREATE, which is problematic because Lucene
   * DELETE can be extremely slow (plus this is not atomic).
   *
   * It is a **lot** more efficient to do this in batch: expect
   * 10,000 deletes to take about 1 second and inserts about 100ms.
   * Each call to this method constitutes a batch UPDATE operation.
   */
  def update(delete: Seq[Query], create: Seq[Document], commit: Boolean = true): Future[Unit] =
    Future {
      blocking {
        if (delete.nonEmpty) {
          writer.deleteDocuments(delete.toArray: _*)
          if (commit) writer.commit()
        }

        if (create.nonEmpty) {
          create foreach { doc =>
            writer addDocument doc
          }
          if (commit) writer.commit()
        }
      }
    }

  def create(docs: Seq[Document], commit: Boolean = true): Future[Unit] = update(Nil, docs, commit)
  def delete(queries: Seq[Query], commit: Boolean = true): Future[Unit] = update(queries, Nil, commit)

  // for manual committing after multiple insertions
  def commit(): Future[Unit] = Future(blocking(writer.commit()))

  def shutdown(): Future[Unit] = {
    executionContext.shutdown()
    executorService.shutdown()

    Future {
      blocking {
        writer.close()

        executionContext.awaitTermination(30, TimeUnit.SECONDS)
        executorService.awaitTermination(30, TimeUnit.SECONDS)

        ()
      }
    }(ExecutionContext.Implicits.global)
  }
}
