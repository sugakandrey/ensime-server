package org.ensime.core

import scala.annotation.tailrec
import scala.tools.scalap.scalax.rules.scalasig._

object ScalaSigApi {

  implicit class RichSymbol(val sym: Symbol) {
    /**
     * @return true if this [[Symbol]] is top level class or object, false otherwise
     */
    def isTopLevel: Boolean = sym.parent match {
      case Some(ext: ExternalSymbol) => true
      case Some(_) => false
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
    def dealias: Type = ???
    def erasure: Type = ???
    def typeSymbol: Symbol = ???
  }
}
