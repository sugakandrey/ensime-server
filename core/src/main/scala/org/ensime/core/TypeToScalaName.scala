// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import scala.tools.nsc.interactive.Global

/**
 * Type safety around Scala type names, scalac uses raw Strings.
 */
final class ScalaName(val underlying: String) extends AnyVal {
  def +(other: String): ScalaName =
    if (other == "") this
    else new ScalaName(underlying + other)
}

trait TypeToScalaName { self: Global with Helpers =>
  import definitions._

  def scalaName(tpe: Type, full: Boolean): ScalaName = tpe match {
    case _: MethodType | _: PolyType =>
      val tparams = tpe.paramss.map { sect =>
        sect.map { p => scalaName(p.tpe, full).underlying }.mkString("(", ", ", ")")
      }.mkString(" => ")
      new ScalaName(tparams) + " => " + scalaName(tpe.finalResultType, full).underlying

    case args: ArgsTypeRef if args.typeSymbol.fullName.startsWith("scala.Function") =>
      val parts = tpe.typeArgs.map(scalaName(_, full).underlying)
      new ScalaName(parts.init.mkString("(", ", ", ")")) + " => " + parts.last

    case args: ArgsTypeRef if args.typeSymbol.fullName.startsWith("scala.Tuple") =>
      val parts = tpe.typeArgs.map(scalaName(_, full).underlying)
      new ScalaName(parts.mkString("(", ", ", ")"))

    case args: ArgsTypeRef if args.typeSymbol.decodedName.forall(!_.isLetterOrDigit) =>
      val parts = tpe.typeArgs.map(scalaName(_, full).underlying)
      val name =
        if (full) tpe.typeSymbol.fullNameString
        else tpe.typeSymbol.nameString

      new ScalaName(parts.init.mkString(" ")) + s" $name ${parts.last}"

    case TypeRef(_, RepeatedParamClass | JavaRepeatedParamClass, typeArgs) =>
      // Safe to assume that args is of length 1 as repeated params means Seq[args]
      val parts = typeArgs.map(scalaName(_, full).underlying)
      new ScalaName(parts.mkString + "*")

    case _ =>
      val name = tpe match {
        case c: ConstantType =>
          scalaName(c.underlying, full).underlying + "(" + c.value.escapedStringValue + ")"
        case r: RefinedType =>
          r.parents.map(scalaName(_, full).underlying).mkString(" with ")
        case _ =>
          if (full) tpe.typeSymbol.fullNameString
          else tpe.typeSymbol.nameString
      }
      new ScalaName(name) + {
        if (tpe.typeArgs.isEmpty) ""
        else tpe.typeArgs.map(scalaName(_, full).underlying).mkString("[", ", ", "]")
      }
  }

  def fullName(tpe: Type): ScalaName = scalaName(tpe, full = true)
  def shortName(tpe: Type): ScalaName = scalaName(tpe, full = false)

  def shortName(sym: Symbol): ScalaName = new ScalaName(sym.nameString)

}
