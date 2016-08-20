// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.indexer

import akka.event.slf4j.SLF4JLogging
import java.io.File
import org.apache.commons.vfs2._

import org.ensime.api._
import org.ensime.vfs._

import org.ensime.util.file._
import org.ensime.util.list._
import org.ensime.util.map._
import scala.annotation.tailrec

// mutable: lookup of user's source files are atomically updated
class SourceResolver(
    config: EnsimeConfig
)(
    implicit
    vfs: EnsimeVFS
) extends FileChangeListener with SLF4JLogging {

  def fileAdded(f: FileObject) = if (relevant(f)) update()
  def fileRemoved(f: FileObject) = if (relevant(f)) update()
  def fileChanged(f: FileObject) = {}

  def relevant(f: FileObject): Boolean = f.getName.isFile && {
    val file = new File(f.getName.getURI)
    (file.isScala || file.isJava) && !file.getPath.contains(".ensime_cache")
  }

  def resolve(clazz: PackageName, source: RawSource): Option[FileObject] = {
    @tailrec
    def loop(clazzes: List[PackageName]): Option[FileObject] = {
      clazzes match {
        case Nil => None
        case h :: t => resolveClazz(h, source) match {
          case None => loop(t)
          case s @ Some(_) => s
        }
      }
    }

    val size = clazz.path.size
    val combinations = clazz.path.tails.flatMap(_.inits).filterNot(_.isEmpty).toList
    // Quite offen people put stuff into the root package,
    // so we add empty package after parent packages, just
    // before we try other possible packages
    val combinationsWithEmpty =
      (combinations.take(size) ::: List.empty[String] :: combinations.drop(size))
        .map(PackageName.apply)
    loop(combinationsWithEmpty)
  }

  // we only support the case where RawSource has a Some(filename)
  private def resolveClazz(clazz: PackageName, source: RawSource): Option[FileObject] =
    source.filename match {
      case None => None
      case Some(filename) => all.get(clazz).flatMap {
        _.find(_.getName.getBaseName == filename)
      }
    }

  def update(): Unit = {
    log.debug("updating sources")
    all = recalculate
  }

  private def scan(f: FileObject) = f.findFiles(SourceSelector) match {
    case null => Nil
    case res => res.toList
  }

  private val depSources = {
    val srcJars = config.referenceSourceJars.toSet ++ {
      for {
        (_, module) <- config.modules
        srcArchive <- module.referenceSourceJars
      } yield srcArchive
    }
    for {
      srcJarFile <- srcJars.toList
      // interestingly, this is able to handle zip files
      srcJar = vfs.vjar(srcJarFile)
      srcEntry <- scan(srcJar)
      inferred = infer(srcJar, srcEntry)
      // continue to hold a reference to source jars
      // so that we can access their contents elsewhere.
      // this does mean we have a file handler, sorry.
      //_ = vfs.nuke(srcJar)
    } yield (inferred, srcEntry)
  }.toMultiMapSet

  private def userSources = {
    for {
      (_, module) <- config.modules.toList
      root <- module.sourceRoots
      dir = vfs.vfile(root)
      file <- scan(dir)
    } yield (infer(dir, file), file)
  }.toMultiMapSet

  private def recalculate = depSources merge userSources

  private var all = recalculate

  private def infer(base: FileObject, file: FileObject): PackageName = {
    // getRelativeName feels the wrong way round, but this is correct
    val relative = base.getName.getRelativeName(file.getName)
    // vfs separator char is always /
    PackageName((relative split "/").toList.init)
  }

}
