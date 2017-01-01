// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import scala.tools.nsc.interactive.Global

import org.ensime.api._
import org.slf4j.Logger

trait Helpers { self: Global =>
  def logger: Logger

  import rootMirror.EmptyPackage

  def applySynonyms(sym: Symbol): List[Symbol] = {
    val members = if (sym.isModule || sym.isModuleClass || sym.isPackageObject) {
      sym.tpe.members
    } else if (sym.isClass || sym.isPackageClass || sym.isPackageObjectClass) {
      sym.companionModule.tpe.members
    } else { List.empty }
    members.toList.filter { _.name.toString == "apply" }
  }

  def constructorSynonyms(sym: Symbol): List[Symbol] = {
    val members = if (sym.isClass || sym.isPackageClass || sym.isPackageObjectClass) {
      sym.tpe.members
    } else if (sym.isModule || sym.isModuleClass || sym.isPackageObject) {
      sym.companionClass.tpe.members
    } else { List.empty }
    members.toList.filter { _.isConstructor }
  }

  def isArrowType(tpe: Type): Boolean = {
    tpe match {
      case args: ArgsTypeRef if args.typeSymbol.fullName.startsWith("scala.Function") => true
      case TypeRef(_, definitions.ByNameParamClass, _) => true
      case _: MethodType => true
      case _: PolyType => true
      case _ =>
        // this still doesn't catch functions defined as val, e.g.
        // Int => String class scala.reflect.internal.Types$ClassArgsTypeRef
        false
    }
  }

  def isNoParamArrowType(tpe: Type): Boolean = {
    tpe match {
      case t: MethodType => t.paramss.flatten.isEmpty
      case t: PolyType => t.paramss.flatten.isEmpty
      case t: Type => false
    }
  }

  def typeOrArrowTypeResult(tpe: Type): Type = {
    tpe match {
      case t: MethodType => t.finalResultType
      case t: PolyType => t.finalResultType
      case t: Type => t
    }
  }

  /* Give the outerClass of a symbol representing a nested type */
  def outerClass(typeSym: Symbol): Option[Symbol] = {
    try {
      if (typeSym.isNestedClass) {
        Some(typeSym.outerClass)
      } else None
    } catch {
      case e: java.lang.Error => None
    }
  }

  def companionTypeOf(tpe: Type): Option[Type] = {
    val sym = tpe.typeSymbol
    if (sym != NoSymbol) {
      if (sym.isModule || sym.isModuleClass) {
        val comp = sym.companionClass
        if (comp != NoSymbol && comp.tpe != tpe) {
          Some(comp.tpe)
        } else None
      } else if (sym.isTrait || sym.isClass || sym.isPackageClass) {
        val comp = sym.companionModule
        if (comp != NoSymbol && comp.tpe != tpe) {
          Some(comp.tpe)
        } else None
      } else None
    } else None
  }

  /*
   * Get the valid member symbols of the package denoted by aSym.
   */
  def packageMembers(parent: Symbol): Iterable[Symbol] = {

    def isRoot(s: Symbol) = s.isRoot || s.isRootPackage

    def filterAndSort(symbols: Iterable[Symbol]) = {
      val validSyms = symbols.filter { s =>
        s != EmptyPackage && !isRoot(s) &&
          // This check is necessary to prevent infinite looping..
          ((isRoot(s.owner) && isRoot(parent)) || (s.owner.fullName == parent.fullName))
      }

      // the nameString operation is depressingly expensive - mapping to tuples first reduces the overhead.
      val vsPairsAsList: List[(String, Symbol)] = validSyms.map(vs => (vs.nameString, vs))(scala.collection.breakOut)
      vsPairsAsList.sortBy(_._1).map(_._2)
    }

    if (isRoot(parent)) {
      filterAndSort(parent.info.members ++ EmptyPackage.info.members)
    } else {
      filterAndSort(parent.info.members)
    }
  }

  import scala.tools.nsc.symtab.Flags._

  def declaredAs(sym: Symbol): DeclaredAs = {
    if (sym.isMethod)
      DeclaredAs.Method
    else if (sym.isTrait && sym.hasFlag(JAVA))
      DeclaredAs.Interface
    else if (sym.isTrait)
      DeclaredAs.Trait
    else if (sym.isInterface)
      DeclaredAs.Interface
    else if (sym.isModule)
      DeclaredAs.Object
    else if (sym.isModuleClass)
      DeclaredAs.Object
    else if (sym.isClass)
      DeclaredAs.Class
    else if (sym.isPackageClass)
      DeclaredAs.Class

    // check this last so objects are not
    // classified as fields
    else if (sym.isValue || sym.isVariable)
      DeclaredAs.Field
    else
      DeclaredAs.Nil
  }
}
