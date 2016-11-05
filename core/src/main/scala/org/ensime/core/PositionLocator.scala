// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import scala.tools.nsc.interactive.Global

trait PositionLocator {
  self: Global =>

  private class ProperlyIncludingLocator(pos: Position) extends Locator(pos) {
    override def traverse(t: Tree): Unit = t match {
      case tt: TypeTree if tt.original != null && (tt.pos includes tt.original.pos) =>
        traverse(tt.original)
      case _ =>
        if (t.pos properlyIncludes pos) {
          if (isEligible(t)) last = t
          super.traverse(t)
        } else t match {
          case mdef: MemberDef =>
            val annTrees = mdef.mods.annotations match {
              case Nil if mdef.symbol != null =>
                mdef.symbol.annotations.map(_.original)
              case anns => anns
            }
            traverseTrees(annTrees)
          case _ =>
        }
    }
  }

  def enclosingTree(p: Position, root: Tree): Tree = new ProperlyIncludingLocator(p).locateIn(root)

}
