// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// Licence: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.indexer

import akka.actor.Actor
import akka.event.slf4j.SLF4JLogging
import org.apache.commons.vfs2._

import org.ensime.api._
import org.ensime.vfs._

import org.ensime.util.file._
import java.util.UUID

trait FileChangeListener {
  def fileAdded(f: FileObject): Unit
  def fileRemoved(f: FileObject): Unit
  def fileChanged(f: FileObject): Unit
  def baseReCreated(f: FileObject): Unit = {}
  def baseRemoved(f: FileObject): Unit = {}
  def baseRegistered(): Unit = {}
}

trait Watcher {
  def shutdown(): Unit
}

/**
 * Watches the user's target output directories for classfiles that
 * need to be indexed or updated (i.e. picks up changes when the
 * compiler produces any output). Can also support jars instead of
 * target directories.
 *
 * If we were Java 7+ we'd be using
 * http://docs.oracle.com/javase/7/docs/api/java/nio/file/WatchService.html
 */
class ClassfileWatcher(
    config: EnsimeConfig,
    listeners: Seq[FileChangeListener]
)(
    implicit
    vfs: EnsimeVFS
) extends Actor with SLF4JLogging {

  private val impls =
    if (config.disableClassMonitoring) Nil
    else {
      val jarJava7WatcherBuilder = new JarJava7WatcherBuilder()
      val classJava7WatcherBuilder = new ClassJava7WatcherBuilder()
      config.targetClasspath.map { target =>
        if (target.isJar) {
          if (log.isTraceEnabled())
            log.trace(s"creating a Java 7 jar watcher for ${target}")
          jarJava7WatcherBuilder.build(target, listeners)
        } else {
          if (log.isTraceEnabled())
            log.trace(s"creating a Java 7 class watcher for ${target}")
          classJava7WatcherBuilder.build(target, listeners)
        }
      }
    }

  override def receive: Receive = {
    case _ =>
  }

  override def postStop(): Unit = {
    impls.foreach(_.shutdown())
  }
}

class SourceWatcher(
    config: EnsimeConfig,
    listeners: Seq[FileChangeListener]
)(
    implicit
    vfs: EnsimeVFS
) extends Watcher with SLF4JLogging {
  private val impls =
    if (config.disableSourceMonitoring) Nil
    else {
      val sourceJava7WatcherBuilder = new SourceJava7WatcherBuilder()
      for {
        module <- config.modules.values
        root <- module.sourceRoots
      } yield {
        if (log.isTraceEnabled())
          log.trace(s"creating a Java 7 source watcher for $root")
        sourceJava7WatcherBuilder.build(root, listeners)
      }
    }

  override def shutdown(): Unit = impls.foreach(_.shutdown)

}

trait Java7WatcherBuilder extends SLF4JLogging {
  import org.ensime.filewatcher.WatcherListener
  val serviceBuilder = new Java7WatchServiceBuilder()
  def build(
    watched: File,
    listeners: Seq[FileChangeListener]
  )(
    implicit
    vfs: EnsimeVFS
  ): Watcher = {
    val watcherId = UUID.randomUUID()
    serviceBuilder.build(watcherId, watched,
      listeners.map { l => toWatcherListener(l, watched, watcherId, vfs) })
  }
  def toWatcherListener(
    l: FileChangeListener,
    baseFile: File,
    uuid: UUID,
    vfs: EnsimeVFS
  ): WatcherListener
}

class JarJava7WatcherBuilder() extends Java7WatcherBuilder {
  import org.ensime.filewatcher.WatcherListener
  override def toWatcherListener(
    l: FileChangeListener,
    baseFile: File,
    uuid: UUID,
    vfs: EnsimeVFS
  ) = {
    new WatcherListener() {
      override val base = baseFile
      override val recursive = false
      override val extensions = JarSelector.include
      override val watcherId = uuid
      override def fileCreated(f: File) =
        l.fileAdded(vfs.vfile(f))
      override def fileDeleted(f: File) = {}
      override def fileModified(f: File) =
        l.fileChanged(vfs.vfile(f))
      override def baseRegistered(): Unit =
        l.baseRegistered()
      override def baseRemoved(): Unit =
        l.fileRemoved(vfs.vfile(baseFile))
      override def missingBaseRegistered(): Unit =
        l.fileAdded(vfs.vfile(baseFile))
      override def baseSubdirRegistered(f: File): Unit = {}
      override def proxyRegistered(f: File): Unit = {}
      override def existingFile(f: File): Unit = {}
    }
  }

}

private class SourceJava7WatcherBuilder() extends Java7WatcherBuilder {
  import org.ensime.filewatcher.WatcherListener
  override def toWatcherListener(
    l: FileChangeListener,
    baseFile: File,
    uuid: UUID,
    vfs: EnsimeVFS
  ) = {
    new WatcherListener() {
      override val base = baseFile
      override val recursive = true
      override val extensions = SourceSelector.include
      override val watcherId = uuid
      @volatile private var notifyExisting = false
      override def fileCreated(f: File) =
        l.fileAdded(vfs.vfile(f))
      override def fileDeleted(f: File) =
        l.fileRemoved(vfs.vfile(f))
      override def fileModified(f: File) =
        l.fileChanged(vfs.vfile(f))
      override def baseRegistered(): Unit = {
        notifyExisting = true
        l.baseRegistered()
      }
      override def baseRemoved(): Unit =
        l.baseRemoved(vfs.vfile(baseFile))
      override def missingBaseRegistered(): Unit =
        l.baseReCreated(vfs.vfile(baseFile))
      override def baseSubdirRegistered(f: File): Unit = {}
      override def proxyRegistered(f: File): Unit =
        notifyExisting = true
      override def existingFile(f: File): Unit =
        if (notifyExisting)
          l.fileAdded(vfs.vfile(f))
    }
  }
}

private class ClassJava7WatcherBuilder() extends Java7WatcherBuilder {
  import org.ensime.filewatcher.WatcherListener
  override def toWatcherListener(
    l: FileChangeListener,
    baseFile: File,
    uuid: UUID,
    vfs: EnsimeVFS
  ) = {
    new WatcherListener() {
      override val base = baseFile
      override val recursive = true
      override val extensions = ClassfileSelector.include
      override val watcherId = uuid
      @volatile private var notifyExisting = false;

      override def fileCreated(f: File) =
        l.fileAdded(vfs.vfile(f))
      override def fileDeleted(f: File) =
        l.fileRemoved(vfs.vfile(f))
      override def fileModified(f: File) =
        l.fileChanged(vfs.vfile(f))
      override def baseRegistered(): Unit = {
        notifyExisting = true
        l.baseRegistered()
      }
      override def baseRemoved(): Unit =
        l.baseRemoved(vfs.vfile(baseFile))
      override def missingBaseRegistered(): Unit =
        l.baseReCreated(vfs.vfile(baseFile))
      override def baseSubdirRegistered(f: File): Unit = {}
      override def proxyRegistered(f: File): Unit =
        notifyExisting = true
      override def existingFile(f: File): Unit =
        if (notifyExisting)
          l.fileAdded(vfs.vfile(f))
    }
  }
}

class Java7WatchServiceBuilder extends SLF4JLogging {
  import org.ensime.filewatcher.FileWatchService
  import org.ensime.filewatcher.WatcherListener

  val fileWatchService: FileWatchService = new FileWatchService
  def build(
    watcherId: UUID,
    base: File,
    listeners: Seq[WatcherListener]
  )(implicit vfs: EnsimeVFS) = {
    if (log.isTraceEnabled())
      log.trace("watching {}", base)

    trait EnsimeWatcher extends Watcher {
      val w = fileWatchService.spawnWatcher(watcherId, base, listeners.toSet)
      override def shutdown(): Unit = {
        if (log.isDebugEnabled())
          log.debug("shutdown watcher {}", w.watcherId)
        w.shutdown()
      }
    }
    val ensimeWatcher = new EnsimeWatcher {}
    ensimeWatcher
  }
}
