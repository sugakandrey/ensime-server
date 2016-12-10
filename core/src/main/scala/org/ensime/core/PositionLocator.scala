// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import scala.reflect.io.AbstractFile
import scala.tools.refactoring.common.{ CompilerAccess, EnrichedTrees }

class PositionLocator(val global: RichPresentationCompiler) extends CompilerAccess with EnrichedTrees {
  import global._

  def compilationUnitOfFile(f: AbstractFile): Option[CompilationUnit] = unitOfFile.get(f)

  private class ProperlyIncludingLocator(pos: Position) extends Traverser {
    private var last: Position = NoPosition

    protected def isEligible(t: Tree) = !t.pos.isTransparent

    override def traverse(t: Tree): Unit = t match {
      case tt: TypeTree if tt.original != null && (tt.pos includes tt.original.pos) =>
        traverse(tt.original)
      case _ =>
        if (t.pos properlyIncludes pos) {
          if (isEligible(t)) {
            val namePosition = t.namePosition()
            last = if (namePosition properlyIncludes pos) namePosition else t.pos
          }
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

    def locateIn(root: Tree): Position = {
      traverse(root)
      last
    }
  }

  def enclosingTreePosition(p: Position): Position = new ProperlyIncludingLocator(p).locateIn(parseTree(p.source))
}
