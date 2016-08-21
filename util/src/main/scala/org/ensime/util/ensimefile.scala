// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.util

import java.io.File
import java.net._
import java.nio.charset.Charset
import java.nio.file._
import java.util.HashMap

import org.ensime.api._
import org.ensime.util.path._

/**
 * Adds functionality to the EnsimeFile sealed family, without
 * polluting the API with implementation detail.
 */
package ensimefile {

  trait RichEnsimeFile {
    def isJava: Boolean
    def isScala: Boolean
    def exists(): Boolean

    /** Direct access contents: not efficient for streaming. */
    def readStringDirect()(implicit cs: Charset): String

    def uri(): URI
  }

}

package object ensimefile {

  object Implicits {
    implicit val DefaultCharset: Charset = Charset.defaultCharset()
  }

  private val ArchiveRegex = "(?:(?:jar:)?file:)?([^!]++)!(.++)".r
  private val FileRegex = "(?:(?:jar:)?file:)?(.++)".r
  def EnsimeFile(path: String): EnsimeFile = path match {
    case ArchiveRegex(file, entry) => ArchiveFile(Paths.get(cleanBadWindows(file)), entry)
    case FileRegex(file) => RawFile(Paths.get(cleanBadWindows(file)))
  }
  def EnsimeFile(path: File): EnsimeFile = RawFile(path.toPath)
  def EnsimeFile(url: URL): EnsimeFile = EnsimeFile(URLDecoder.decode(url.toExternalForm(), "UTF-8"))

  // URIs on Windows can look like /C:/path/to/file, which are malformed
  private val BadWindowsRegex = "/+([^:]+:[^:]+)".r
  private def cleanBadWindows(file: String): String = file match {
    case BadWindowsRegex(clean) => clean
    case other => other
  }

  implicit class RichRawFile(val raw: RawFile) extends RichEnsimeFile {
    // PathMatcher is too complex, use http://stackoverflow.com/questions/20531247
    override def isJava: Boolean = raw.file.toString.toLowerCase.endsWith(".java")
    override def isScala: Boolean = raw.file.toString.toLowerCase.endsWith(".scala")
    override def exists(): Boolean = raw.file.exists()
    override def readStringDirect()(implicit cs: Charset): String = raw.file.readString()
    override def uri: URI = raw.file.toUri()
  }

  // most methods require obtaining the Path of the entry, within the
  // context of the archive file, and ensuring that we close the
  // resource afterwards (which is slow for random access)
  implicit class RichArchiveFile(val archive: ArchiveFile) extends RichEnsimeFile {
    override def isJava: Boolean = archive.entry.toLowerCase.endsWith(".java")
    override def isScala: Boolean = archive.entry.toLowerCase.endsWith(".scala")
    override def exists(): Boolean = archive.jar.exists() && withEntry(_.exists())
    override def readStringDirect()(implicit cs: Charset): String = withEntry(_.readString())
    override def uri: URI = URI.create(s"jar:${archive.jar.toUri}!${archive.entry}") // path is null (opaque)

    private def fileSystem(): FileSystem = FileSystems.newFileSystem(
      URI.create(s"jar:${archive.jar.toUri}"),
      new HashMap[String, String]
    )
    private def withFs[T](action: FileSystem => T): T = {
      val fs = fileSystem()
      try action(fs)
      finally fs.close()
    }
    private def withEntry[T](action: Path => T): T = withFs { fs =>
      action(fs.getPath(archive.entry))
    }

    def fullPath: String = s"${archive.jar}!${archive.entry}"

    /*
    import java.nio.file.attribute._
    import java.nio.file.FileVisitResult
    val printer = new SimpleFileVisitor[Path] {
      override def visitFile(file: Path, attr: BasicFileAttributes): FileVisitResult = {
        FileVisitResult.CONTINUE
      }
    }
    Files.walkFileTree(fs.getPath("/"), printer)
   */

  }

  // boilerplate-tastic... Coproduct would be helpful here
  implicit def richEnsimeFile(ensime: EnsimeFile): RichEnsimeFile = ensime match {
    case raw: RawFile => new RichRawFile(raw)
    case archive: ArchiveFile => new RichArchiveFile(archive)
  }
}
