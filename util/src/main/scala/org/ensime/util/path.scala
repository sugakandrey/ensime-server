// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.util

import java.io.IOException
import java.nio.charset.Charset
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import java.util.{ Collections, EnumSet }

import scala.collection.JavaConverters._
import scala.util.Try

import org.slf4j.LoggerFactory

/**
 * Idiomatic scala methods for Path.
 *
 * This is considered a low-level API and may be made private, prefer
 * the more typesafe EnsimeFile hierarchy.
 */
package object path {

  def withTempDirPath[T](a: Path => T): T = {
    val dir = Files.createTempDirectory("ensime-").canon
    try a(dir)
    finally Try(dir.deleteDirRecursively())
  }

  def withTempFilePath[T](a: Path => T): T = {
    val file = Files.createTempFile("ensime-", ".tmp").canon
    try a(file)
    finally Try(file.delete())
  }

  implicit class RichPath(val path: Path) extends AnyVal {
    def exists(): Boolean = Files.exists(path)
    def attrs(): BasicFileAttributes = Files.readAttributes(path, classOf[BasicFileAttributes])
    def readBytes(): Array[Byte] = Files.readAllBytes(path)
    def readString()(implicit cs: Charset): String = new String(readBytes(), cs)
    def readLines(): List[String] = Files.readAllLines(path).asScala.toList

    def write(bytes: Array[Byte]): Unit = Files.write(path, bytes, StandardOpenOption.CREATE)

    def /(child: String): Path = path.resolve(child)
    def canon(): Path = {
      val normalised = path.normalize()
      val target = if (Files.isSymbolicLink(normalised)) Files.readSymbolicLink(normalised) else normalised
      try target.toRealPath() catch { case e: IOException => target }
    }
    def isFile(): Boolean = Files.isRegularFile(path)
    def isDirectory(): Boolean = Files.isDirectory(path)

    def delete(): Unit = Files.delete(path)
    def mkdirs(): Unit = Files.createDirectories(path)
    def copyDirTo(to: Path): Unit = {
      require(path.isDirectory && to.isDirectory)
      Files.walkFileTree(path, EnumSet.of(FileVisitOption.FOLLOW_LINKS), Integer.MAX_VALUE, new CopyDirVisitor(path, to))
    }
    def deleteDirRecursively(): Unit = {
      require(path.isDirectory)
      Files.walkFileTree(path, Collections.emptySet[FileVisitOption], Integer.MAX_VALUE, new DeleteDirVisitor(path))
    }
  }

}

package path {

  private class CopyDirVisitor(from: Path, to: Path) extends SimpleFileVisitor[Path] {
    override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult = {
      val target = to.resolve(from.relativize(dir))
      if (!Files.exists(target)) {
        Files.createDirectory(target)
      }
      FileVisitResult.CONTINUE
    }

    override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
      Files.copy(file, to.resolve(from.relativize(file)), StandardCopyOption.COPY_ATTRIBUTES)
      FileVisitResult.CONTINUE
    }
  }

  private class DeleteDirVisitor(base: Path) extends SimpleFileVisitor[Path] {
    private val log = LoggerFactory.getLogger(getClass)
    override def postVisitDirectory(dir: Path, e: IOException): FileVisitResult = {
      try Files.delete(dir)
      catch {
        case e: DirectoryNotEmptyException =>
          log.warn(s"failed to delete $dir in $base because it was not empty", e)
      }
      FileVisitResult.CONTINUE
    }

    override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
      try Files.delete(file)
      catch {
        case e: FileSystemException =>
          // I bet we're on Windows! Remember, if any other process
          // has a handle to the same file we can't delete it. It's
          // not the end of the world.
          log.warn(s"failed to delete $file in $base", e)
      }
      FileVisitResult.CONTINUE
    }
  }

}
