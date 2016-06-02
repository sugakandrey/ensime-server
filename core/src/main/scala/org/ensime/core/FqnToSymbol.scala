// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import scala.tools.nsc.interactive.Global

import org.ensime.indexer._
import org.ensime.util.list._
import org.slf4j.Logger

/**
 * The inverse of SymbolToFqn
 */
trait FqnToSymbol { self: Global with SymbolToFqn =>
  def logger: Logger

  private val primitiveSymbolByName: Map[String, Symbol] = Map(
    "boolean" -> definitions.BooleanClass,
    "byte" -> definitions.ByteClass,
    "char" -> definitions.CharClass,
    "short" -> definitions.ShortClass,
    "int" -> definitions.IntClass,
    "long" -> definitions.LongClass,
    "float" -> definitions.FloatClass,
    "double" -> definitions.DoubleClass,
    "void" -> definitions.UnitClass
  )

  private def segToSym(seg: List[Name], root: Symbol): Symbol = seg.foldLeft(root) {
    (sym, name) => sym.info.member(name)
  }

  def toSymbol(scalaName: String, rootSymbol: Symbol = RootClass): Symbol = {
    if (rootSymbol == RootClass) primitiveSymbolByName.get(scalaName)
    else None
  } getOrElse {
    val term = segToSym(nme.segments(scalaName, assumeTerm = true), rootSymbol)
    if (term != NoSymbol) term
    else segToSym(nme.segments(scalaName, assumeTerm = false), rootSymbol)
  }

  def toSymbol(fqn: FullyQualifiedName): Symbol = fqn match {
    case p: PackageName =>
      nme.segments(p.fqnString, assumeTerm = true).
        foldLeft(RootClass: Symbol) {
          (owner, name) => owner.info.member(name)
        }

    case ClassName(p, name) =>
      val (outer, inner) = name.split("\\$").toList.initLast

      // $ is used to select Term names, no $ at end is a Type name
      val container = outer.foldLeft(toSymbol(p)) {
        (owner, name) => owner.info.member(newTermName(name))
      }

      val select = if (name.endsWith("$")) newTermName(inner) else newTypeName(inner)
      container.info.member(select)

    case f @ FieldName(c, name) =>
      val field = toSymbol(c).info.member(newTermName(name))
      if (field == NoSymbol && !field.name.endsWith("$")) {
        // HACK: java static fields look like methods on companions to scala
        toSymbol(f.copy(owner = companion(c)))
      } else field

    case m @ MethodName(c, name, desc) =>
      val container = toSymbol(c)
      val candidates = container.info.members.filter { sym =>
        // scala doesn't provide a Java descriptor
        sym.isMethod && sym.name.encoded == name && toFqn(sym) == fqn
      }

      if (candidates.nonEmpty) candidates.head
      else if (!c.name.endsWith("$")) {
        // HACK: java static methods look like methods on companions to scala
        toSymbol(m.copy(owner = companion(c)))
      } else NoSymbol
  }

  private def companion(c: ClassName): ClassName =
    if (c.name.endsWith("$")) c else c.copy(name = c.name + "$")

}
