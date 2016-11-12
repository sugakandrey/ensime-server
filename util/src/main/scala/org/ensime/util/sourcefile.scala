// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.util

import org.ensime.api._
import org.ensime.util.ensimefile._

/**
 * Functionality for the SourceFileInfo family.
 */
package object sourcefile {

  implicit class RichSourceFileInfo(val v: SourceFileInfo) extends AnyVal {
    def exists() = v match {
      case SourceFileInfo(f, _, _) if f.exists() => true
      case SourceFileInfo(_, Some(c), _) => true
      case SourceFileInfo(_, _, Some(f)) if f.exists() => true
      case _ => false
    }
  }

}
