// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import scala.tools.nsc.interactive.Global

trait PositionLocator {
  self: Global =>

  private class ProperlyIncludingLocator(pos: Position) extends Locator(pos) {
    override protected def isEligible(t: Tree): Boolean = super.isEligible(t) && (t.pos properlyIncludes pos)
  }

  def enclosingTree(p: Position, root: Tree): Tree = new ProperlyIncludingLocator(p).locateIn(root)

}
