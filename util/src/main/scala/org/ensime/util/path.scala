// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.util

import java.io.IOException
import java.nio.charset.Charset
import java.nio.file.{ Files, Path }
import java.nio.file.attribute.BasicFileAttributes

/**
 * Idiomatic scala methods for Path. One must not assume that Path is
 * a File, see `ensimefile` for more appropriate functionality.
 *
 * (Maybe this should be private within the ensimefile classes)
 */
package object path {

  import java.nio.file.StandardOpenOption

  implicit class RichPath(val path: Path) extends AnyVal {
    def exists(): Boolean = Files.exists(path)
    def attrs(): BasicFileAttributes = Files.readAttributes(path, classOf[BasicFileAttributes])
    def readBytes(): Array[Byte] = Files.readAllBytes(path)
    def readString()(implicit cs: Charset): String = new String(readBytes(), cs)

    def write(bytes: Array[Byte]): Unit = Files.write(path, bytes, StandardOpenOption.CREATE)

    def /(child: String): Path = path.resolve(child)
    def canon: Path = try path.normalize.toRealPath() catch { case e: IOException => path.normalize() }
    def isFile: Boolean = path.toFile.isFile

  }
}
