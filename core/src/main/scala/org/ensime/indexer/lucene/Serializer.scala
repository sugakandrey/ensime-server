// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.indexer.lucene

import org.apache.lucene.document.{ Document, StringField }
import org.apache.lucene.document.Field.Store
import org.apache.lucene.index.Term
import org.apache.lucene.search.{ BooleanQuery, Query, TermQuery }
import org.apache.lucene.search.BooleanClause.Occur._
import shapeless.Typeable

// in hindsight, this would have been more cleanly designed as TypeClass
abstract class Serializer[T](tpe: Typeable[T])
    extends DocumentProvider[T] with DocumentRecovery[T] with QueryProvider[T] {
  private val TypeField = new StringField("TYPE", tpe.describe, Store.YES)
  private val TypeTerm = new TermQuery(new Term(TypeField.name, TypeField.stringValue))

  def id(t: T): String

  final def toDocument(t: T): Document = {
    val doc = new Document
    doc.add(TypeField)
    doc.add(new StringField("ID", id(t), Store.NO))
    addFields(doc, t)
    doc
  }
  def addFields(doc: Document, t: T): Unit

  final def createQuery(e: T): Query = {
    new BooleanQuery.Builder().
      add(TypeTerm, MUST).
      add(new TermQuery(new Term("ID", id(e))), MUST).
      build()
  }
}

abstract class EntityS[T <: Entity](tpe: Typeable[T]) extends Serializer(tpe) {
  def id(t: T) = t.id
}
