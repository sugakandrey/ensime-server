// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.indexer

import java.io.FileNotFoundException
import java.nio.file.Path

import scala.collection.JavaConverters._

import akka.event.slf4j.SLF4JLogging
import org.apache.commons.vfs2.FileObject
import org.apache.lucene.analysis.core.KeywordAnalyzer
import org.apache.lucene.document.{ Document, TextField }
import org.apache.lucene.document.Field.Store
import org.apache.lucene.index.Term
import org.apache.lucene.search._
import org.apache.lucene.search.BooleanClause.Occur
import org.apache.lucene.search.Query
import org.ensime.indexer.SearchService._
import org.ensime.indexer.graph._
import org.ensime.indexer.lucene._
import org.ensime.util.list._
import shapeless.Typeable

object IndexService extends SLF4JLogging {

  class FqnAnalyzer extends DynamicSynonymAnalyzer {
    private def cases(s: String) = List(s, s.toLowerCase)
    override def synonyms(term: String): Set[String] = {
      val (path, name) = term.replaceAll("\\(.*", "").split('.').toList.initLast
      def camel(s: String) = s.filter(_.isUpper)
      def spacey(s: String) = List(s, s.replace('.', ' '))

      val shortPkg = path.map(_.charAt(0)).mkString("", ".", "." + name)
      val innerNames = name.split('$').flatMap(cases)

      Set(term, name, camel(name)) ++ innerNames ++ spacey(shortPkg)
    }.filter(_.size > 1).flatMap(cases) - term
  }

  sealed trait FqnIndex extends Entity {
    def file: Option[FileCheck] // not retrieved
    def fqn: String
    def id = fqn
  }
  final case class ClassIndex(fqn: String, file: Option[FileCheck]) extends FqnIndex
  final case class MethodIndex(fqn: String, file: Option[FileCheck]) extends FqnIndex
  final case class FieldIndex(fqn: String, file: Option[FileCheck]) extends FqnIndex
  abstract class AFqnIndexS[T <: FqnIndex](
      tpe: Typeable[T],
      cons: (String, Option[FileCheck]) => T
  ) extends EntityS(tpe) {
    def addFields(doc: Document, i: T): Unit = {
      doc.add(new TextField("file", i.file.get.filename, Store.NO))
      doc.add(new TextField("fqn", i.fqn, Store.YES))
    }
    def toEntity(d: Document): T = cons(d.get("fqn"), None)
  }
  implicit object ClassIndexS extends AFqnIndexS(Typeable[ClassIndex], ClassIndex)
  implicit object MethodIndexS extends AFqnIndexS(Typeable[MethodIndex], MethodIndex)
  implicit object FieldIndexS extends AFqnIndexS(Typeable[FieldIndex], FieldIndex)
  implicit object FqnIndexS extends DocumentRecovery[FqnIndex] {
    def toEntity(d: Document) = d.get("TYPE") match {
      case "ClassIndex" => ClassIndexS.toEntity(d)
      case "MethodIndex" => MethodIndexS.toEntity(d)
      case "FieldIndex" => FieldIndexS.toEntity(d)
      case o => throw new IllegalStateException(o)
    }
  }
  private val ClassIndexT = new TermQuery(new Term("TYPE", classOf[ClassIndex].getSimpleName))
  private val MethodIndexT = new TermQuery(new Term("TYPE", classOf[MethodIndex].getSimpleName))
  private val FieldIndexT = new TermQuery(new Term("TYPE", classOf[FieldIndex].getSimpleName))

  /**
   * Like `PrefixQuery` but gives a higher value to exact matches.
   * [Stack Overflow](http://stackoverflow.com/questions/17723025)
   */
  def boostedPrefixQuery(t: Term): Query = new BooleanQuery.Builder().
    add(new PrefixQuery(t), Occur.SHOULD).
    add(new TermQuery(t), Occur.SHOULD).
    build()
}

class IndexService(path: Path) {
  import org.ensime.indexer.IndexService._

  private val analyzers = Map(
    "fqn" -> new FqnAnalyzer,
    "file" -> new KeywordAnalyzer
  )

  private[indexer] val lucene = new SimpleLucene(path, analyzers)

  private def calculatePenalty(fqn: String): Float = {
    val nonTrailing$s = fqn.count(_ == '$') - (if (fqn.endsWith("$")) 1 else 0)
    1 - .25f * nonTrailing$s
  }

  def persist(symbols: List[SourceSymbolInfo], commit: Boolean, boost: Boolean): Unit = {
    val fqns: List[Document] = symbols.collect {
      case ClassSymbolInfo(f, _, _, _, classSymbol, scalap) if !classSymbol.isScala || scalap.isDefined =>
        val fqn = classSymbol.name.fqnString
        val penalty = calculatePenalty(fqn)
        val document = ClassIndex(fqn, Some(f)).toDocument
        document.boostText("fqn", penalty)
        document
      case MethodSymbolInfo(f, _, _, methodSymbol, _) => MethodIndex(methodSymbol.name.fqnString, Some(f)).toDocument
      case FieldSymbolInfo(f, _, _, fieldSymbol, _) => FieldIndex(fieldSymbol.name.fqnString, Some(f)).toDocument
      case TypeAliasSymbolInfo(f, _, t) => FieldIndex(t.javaName.fqnString, Some(f)).toDocument
    }(collection.breakOut)

    if (boost) {
      fqns foreach { fqn =>
        val currentBoost = fqn.boost("fqn")
        fqn.boostText("fqn", currentBoost + .25f)
      }
    }

    lucene.create(fqns, commit)
  }

  def commit(): Unit = {
    try lucene.commit()
    catch {
      case e: FileNotFoundException =>
        // one of those useless exceptions that is either harmless
        // (testing, commits are happening after we're interested) or
        // utterly fatal (the user deleted the files on disk)
        log.error("the Lucene database was deleted: " + e.getMessage)
    }
  }

  def remove(fs: List[FileObject]): Unit = {
    val terms = fs.map { f => new TermQuery(new Term("file", f.getName.getURI)) }
    lucene.delete(terms, commit = false) // don't commit yet
  }

  def searchClasses(query: String, max: Int): List[ClassIndex] = {
    val q = new BooleanQuery.Builder().
      add(boostedPrefixQuery(new Term("fqn", query)), Occur.MUST).
      add(ClassIndexT, Occur.MUST).
      build()
    lucene.search(q, max).map(_.toEntity[ClassIndex]).distinct
  }

  def searchClassesMethods(terms: List[String], max: Int): List[FqnIndex] = {
    val query = new DisjunctionMaxQuery(
      terms.map(buildTermClassMethodQuery).asJava, 0.3f
    )
    lucene.search(query, max).map(_.toEntity[ClassIndex]).distinct
  }

  def buildTermClassMethodQuery(query: String): Query = {
    new BooleanQuery.Builder().
      add(boostedPrefixQuery(new Term("fqn", query)), Occur.MUST).
      add(
        new BooleanQuery.Builder().
          add(ClassIndexT, Occur.SHOULD).
          add(FieldIndexT, Occur.MUST_NOT).
          add(MethodIndexT, Occur.SHOULD).
          build(),
        Occur.MUST
      ).
        build()
  }
}
