package org.ensime.core

import org.slf4j.{ Logger, LoggerFactory }

import scala.annotation.tailrec
import scala.tools.scalap.scalax.rules.scalasig._

object ScalaSigApi {
  def log: Logger = LoggerFactory.getLogger(this.getClass)

  implicit class RichSymbol(val sym: Symbol) {

    /**
     * @return true if this symbol is top level class or object, false otherwise
     */
    def isTopLevel: Boolean = sym.parent match {
      case Some(ext: ExternalSymbol) => true
      case Some(_) => false
      case None => ???
    }

    /**
     * @return top level parent of this symbol
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
     * @return Dot separated, name of enclosing package
     */
    def enclosingPackage: String = sym.topLevelParent.parent.fold("")(_.path)
  }
}
