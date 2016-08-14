// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.util

import java.io._
import java.net._

import org.apache.commons.vfs2.FileObject
import org.ensime.api.deprecating

/**
 * Adds functionality to the Apache VFS FileObject.
 */
package object fileobject {

  @deprecating("https://github.com/ensime/ensime-server/issues/1437")
  implicit class RichFileObject(val fo: FileObject) extends AnyVal {
    // None if the fo is not an entry in an archive
    def pathWithinArchive: Option[String] = {
      val uri = fo.getName.getURI
      if (uri.startsWith("jar") || uri.startsWith("zip"))
        Some(fo.getName.getRoot.getRelativeName(fo.getName))
      else None
    }

    // assumes it is a local file
    def asLocalFile: File = {
      require(fo.getName.getURI.startsWith("file"))
      new File(new URI(fo.getName.getURI))
    }
  }

}
