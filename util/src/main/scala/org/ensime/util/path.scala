// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.util

import java.nio.charset.Charset
import java.nio.file.{ Files, Path }

/**
 * Idiomatic scala methods for Path. One must not assume that Path is
 * a File, see `ensimefile` for more appropriate functionality.
 *
 * (Maybe this should be private within the ensimefile classes)
 */
package object path {

  implicit class RichPath(val path: Path) extends AnyVal {
    def exists(): Boolean = Files.exists(path)
    def readBytes(): Array[Byte] = Files.readAllBytes(path)
    def readString()(implicit cs: Charset): String = new String(readBytes(), cs)

    def /(child: String): Path = path.resolve(child)
    def canon: Path = ???
    def isFile: Boolean = path.toFile.isFile

    /*
       def getAbsolutePath(path: Path): Path = if (!path.isJarUrl) path.toAbsolutePath
  else {
    val split = path.toString.split("!").take(2)
    val slashOrNot = if (path.toString.startsWith("/")) "/" else ""
    Paths.get(Paths.get(split.head.split("jar:file:")(1)).getParent.toString + slashOrNot + split(1))
  }
  def baseName(path: String) = splitPath(path, front = false)

  private def splitPath(path0: String, front: Boolean): String = {
    val isDir = path0.charAt(path0.length - 1) == '/'
    val path = if (isDir) path0.substring(0, path0.length - 1) else path0
    val idx = path.lastIndexOf('/')

    if (idx < 0)
      if (front) "/"
      else path
    else if (front) path.substring(0, idx + 1)
    else path.substring(idx + 1)
  }
  def getInputStream(path: Path) = if (!path.isJarUrl) new FileInputStream(path.toFile) else {
    val inputFile = path.toString
    val inputURL = new URL(inputFile)
    val conn = inputURL.openConnection().asInstanceOf[JarURLConnection]
    conn.getInputStream
  }
     */

  }
}
