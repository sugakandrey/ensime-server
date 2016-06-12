package org.ensime.core

import org.ensime.indexer.{ ClassName, PackageName }
import org.slf4j.{ Logger, LoggerFactory }

import scala.annotation.tailrec
import scala.tools.scalap.scalax.rules.scalasig._

object ScalaSigApi {
  def log: Logger = LoggerFactory.getLogger(this.getClass)

  implicit class RichSymbol(val sym: Symbol) {
    /**
     * @return true if this [[Symbol]] is top level class or object, false otherwise
     */
    def isTopLevel: Boolean = sym.parent match {
      case Some(ext: ExternalSymbol) => true
      case Some(_) => false
      case None => ???
    }

    /**
     * @return top level parent of this [[Symbol]]
     */
    def topLevelParent: Symbol = sym.parent match {
      case Some(ext: ExternalSymbol) => sym
      case Some(p) => p.topLevelParent
      case _ => throw new AssertionError("Empty parent on non External Symbol")
    }

    def ownerChain: List[Symbol] = {
      @tailrec
      def loop(sym: Symbol, acc: List[Symbol] = Nil): List[Symbol] =
        sym.parent match {
          case Some(ext: ExternalSymbol) => sym :: acc
          case Some(s) => loop(s, sym :: acc)
          case None => throw new AssertionError("Empty parent on non External Symbol")
        }
      loop(sym)
    }

    /**
     * @return Dot separated, full name of enclosing package
     */
    def enclosingPackage: String = sym.topLevelParent.parent.fold("")(_.path)
  }

  implicit class RichType(val t: Type) {
    def getPackage: String = t match {
      case ThisType(sym @ ExternalSymbol(_, _, _)) => sym.path
      case ThisType(sym) => sym.enclosingPackage
      case TypeRefType(prefix, _, _) => prefix.getPackage
      case SingleType(typeRef, _) => typeRef.getPackage
      case _ => log.debug(s"Called packageName on type: ${t.getClass} $t"); ???
    }

    @tailrec
    final def nested(acc: List[String] = Nil): List[String] = t match {
      case ThisType(ExternalSymbol(_, _, _)) => acc
      case ThisType(sym @ ClassSymbol(_, _)) => sym.name :: acc
      case TypeRefType(prefix, sym, _) => prefix.nested(sym.name :: acc)
      case SingleType(typeRef, sym) => typeRef.nested(sym.name :: acc)
      case _ => log.debug(s"Got $t as a type in methodType.nested()"); ???
    }
  }

  implicit class RichTypeRefType(val t: TypeRefType) {
    def toClassName: ClassName = {
      val TypeRefType(prefix, sym, _) = t
      val aPackage = prefix.getPackage
      val packageName = PackageName(aPackage.split("\\.").toList)

      val className = sym match {
        case es @ ExternalSymbol(name, _, _) => es.path.substring(aPackage.length + 1).replaceAll("\\.", "\\$")
        case _ => s"${prefix.nested().mkString("$")}$$${sym.name}"
      }
      ClassName(packageName, className)
    }
  }

}
