// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import Predef.{ any2stringadd => _ }

import scala.reflect.internal.util.Position

/**
 * Simulate methods that were added in later versions of the scalac
 * API, or to generate fake methods that we can use in both versions.
 */
trait PresentationCompilerBackCompat

trait PositionBackCompat {
  implicit class RichPosition(pos: Position) {
    // annoyingly, {start, end}OrPoint is deprecated
    def startOrCursor: Int = if (pos.isRange) pos.start else pos.point
    def endOrCursor: Int = if (pos.isRange) pos.end else pos.point
  }
}
