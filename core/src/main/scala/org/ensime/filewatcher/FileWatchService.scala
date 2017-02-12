// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.filewatcher

import java.io._
import java.nio.file.{ FileSystems, Path, WatchKey }
import java.nio.file.StandardWatchEventKinds._
import java.nio.file.WatchService
import java.util.UUID
import java.util.concurrent.ConcurrentHashMap

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.concurrent.Map
import scala.collection.immutable.Set
import scala.language.implicitConversions
import scala.util.control.NonFatal
import scala.util.{ Failure, Properties, Success, Try }

import org.slf4j.LoggerFactory

abstract class Watcher(val watcherId: UUID, val file: File, val listeners: Set[WatcherListener]) {
  val fileWatchService: FileWatchService
  def watch(): Unit =
    fileWatchService.watch(file, listeners, false)

  def shutdown(): Unit = {
    fileWatchService.WatchKeyManager.removeObservers(watcherId)
    fileWatchService.monitorThread.foreach { thread =>
      thread.interrupt()
    }
  }
}

trait WatcherListener {
  val base: File
  val recursive: Boolean
  val extensions: scala.collection.Set[String]
  val watcherId: UUID

  def fileCreated(f: File): Unit = {}
  def fileDeleted(f: File): Unit = {}
  def fileModified(f: File): Unit = {}

  def baseRegistered(): Unit = {}
  def baseRemoved(): Unit = {}
  def baseSubdirRemoved(f: File): Unit = {}
  def missingBaseRegistered(): Unit = {}
  def baseSubdirRegistered(f: File): Unit = {}
  def proxyRegistered(f: File): Unit = {}

  def existingFile(f: File): Unit = {}

  def isWatched(f: File) =
    (extensions.exists(e => {
      f.getName.endsWith(e)
    })) && f.getPath.startsWith(base.getPath)

  def isBaseAncestor(f: File) =
    base.getAbsolutePath.startsWith(f.getAbsolutePath)
}

// tested in FileWatcherSpec
class FileWatchService { self =>
  private val log = LoggerFactory.getLogger(getClass)

  /**
   * The low priority thread used for checking the files being monitored.
   */
  @volatile private[filewatcher] var monitorThread: Option[Thread] = None

  /**
   * A flag used to determine if the monitor thread should be running.
   */
  @volatile private var shouldRun: Boolean = true

  /**
   * Construct a new Java7 WatchService
   */
  var watchService: WatchService = null

  implicit def keyToFile(k: WatchKey): File = k.watchable().asInstanceOf[Path].toFile
  implicit def keyToCanonicalPath(k: WatchKey): String = k.watchable().asInstanceOf[Path].toFile.getCanonicalPath()

  private def init(): Unit = {
    log.debug("init watcher")
    watchService = Try {
      FileSystems.getDefault().newWatchService()
    } match {
      case Success(w) => w
      case Failure(e) => throw new Exception("failed to create WatchService {}", e)
    }
    start()
  }

  /**
   * Start a background monitoring thread
   */
  private def start() = {
    log.debug("start a background monitoring thread")
    monitorThread match {
      case Some(t) => log.warn(s"monitoring thread is already started")
      case None => {
        val thread = new Thread(
          new Runnable {
            override def run(): Unit =
              try monitor()
              catch {
                case i: InterruptedException => // silently ignore
                case NonFatal(e) =>
                  log.warn(s"caught an exception while monitoring", e)
              }
          }
        )
        thread.setName("FileWatchService-monitor")
        thread.start()
        monitorThread = Some(thread)
      }
    }
  }

  def watch(file: File, listeners: Set[WatcherListener], wasMissing: Boolean, retry: Int = 2): Unit = {
    try {
      if (file.isDirectory) {
        registerDir(file, listeners, wasMissing, retry)
      } else if (file.isFile) {
        val fileBase = new File(file.getParent)
        registerDir(fileBase, listeners, wasMissing, retry)
      } else {
        if (file.getParentFile.exists) {
          registerDir(file.getParentFile, listeners, wasMissing, retry)
        } else {
          watch(file.getParentFile, listeners, wasMissing, retry)
        }
      }
    } catch {
      case e: Throwable =>
        log.error(s"failed to watch ${file}")
    }

  }

  def notifyExisting(dir: File, listeners: Set[WatcherListener], key: WatchKey) =
    for {
      f <- dir.listFiles
      if f.isFile
      l <- listeners
      if l.isWatched(f)
    } { l.existingFile(f) }

  def watchExistingSubdirs(dir: File, listeners: Set[WatcherListener]) =
    if (listeners.exists(_.recursive))
      for {
        d <- dir.listFiles
        if d.isDirectory
        l <- listeners
      } { watch(d, listeners, false) }

  def registerDir(dir: File, listeners: Set[WatcherListener], wasMissing: Boolean, retry: Int = 2): Unit = {
    if (wasMissing && listeners.exists { l => l.base == dir }) {
      if (log.isTraceEnabled)
        log.trace(s"delay ${dir} base registration")
      Thread.sleep(100)
    }
    val observers = (listeners map { maybeBuildWatchKeyObserver(dir, _) }).flatten
    if (log.isTraceEnabled)
      log.trace(s"register ${dir} with WatchService")
    if (!observers.isEmpty) {
      val key: WatchKey = try {
        dir.toPath.register(
          watchService,
          ENTRY_CREATE,
          ENTRY_MODIFY,
          ENTRY_DELETE
        )
      } catch {
        case e: Throwable => {
          if (retry < 0) {
            log.warn("can not register. retrying..." + dir + " " + e)
            Thread.sleep(50)
            watch(dir, listeners, wasMissing, retry - 1)
          }
          throw new Exception(e)
        }
      }

      notifyExisting(dir, listeners, key)
      if (observers.exists {
        case _: BaseObserver => true
        case _: BaseSubdirObserver => true
        case _: BaseFileObserver => true
        case _ => false
      })
        watchExistingSubdirs(dir, listeners)

      observers foreach {
        case o: BaseObserver =>
          if (wasMissing)
            o.watcherListener.missingBaseRegistered()
          else
            o.watcherListener.baseRegistered()
        case o: BaseFileObserver =>
          if (wasMissing)
            o.watcherListener.missingBaseRegistered()
          else
            o.watcherListener.baseRegistered()
        case o: BaseSubdirObserver =>
          o.watcherListener.baseSubdirRegistered(dir)

        case o: ProxyObserver => o.watcherListener.proxyRegistered(dir)
      }

      observers foreach (WatchKeyManager.addObserver(key, _))
      if (WatchKeyManager.hasProxy(key))
        dir.listFiles.filter(f => (f.isDirectory || f.isFile))
          .foreach(WatchKeyManager.maybeAdvanceProxy(key, _))

    } else
      log.warn("No listeners for {}. Skip registration.")
  }

  /**
   *  Wait for Java7 WatchService event and notify the listeners.
   */
  private def monitor() = {
    log.debug("start monitoring WatchService events")
    while (continueMonitoring) {
      Try { watchService.take() } match {
        case Success(key) => {
          if (WatchKeyManager.contains(key)) {
            processEvents(key)
            val isWindows = Properties.osName.startsWith("Windows")
            // can not recover reliably from deleted base without delay
            if (isWindows) Thread.sleep(1000)
            else Thread.sleep(20)
            if (!key.reset) {
              if (log.isTraceEnabled)
                log.trace("may be recover from deletion {}", keyToFile(key))
              maybeRecoverFromDeletion(key)
            }
          } else if (log.isTraceEnabled)
            log.trace(s"key {} is not managed by watcher yet", keyToFile(key))
        }
        case Failure(e) => {
          log.error("unexpected WatchService take error. {}", e)
          shouldRun = false
        }
      }
    }
    closeWatchService()

    def processEvents(key: WatchKey) = {

      for (event <- key.pollEvents.asScala) {
        val kind = event.kind
        val file = key.watchable.asInstanceOf[Path]
          .resolve(event.context.asInstanceOf[Path]).toFile

        if (kind == ENTRY_CREATE
          && file.isDirectory
          && WatchKeyManager.hasRecursive(key))
          watch(file, WatchKeyManager.recListeners(key), false)

        if (kind == ENTRY_CREATE)
          WatchKeyManager.maybeAdvanceProxy(key, file)

        val ls = WatchKeyManager.nonProxyListeners(key)

        if (kind == ENTRY_CREATE)
          ls filter { _.isWatched(file) } foreach (_.fileCreated(file))

        if (kind == ENTRY_MODIFY)
          ls filter { _.isWatched(file) } foreach (_.fileModified(file))

        if (kind == ENTRY_DELETE) {
          ls filter { _.isWatched(file) } foreach (_.fileDeleted(file))
          for {
            o <- WatchKeyManager.baseFileObservers(key)
            if o.watcherListener.isWatched(file)
          } {
            WatchKeyManager.removeObserver(key, o)
            o.watcherListener.baseRemoved()
            watch(file, Set(o.watcherListener), true)
          }
        }
        if (kind == OVERFLOW)
          log.warn(s"overflow event for ${file}")
      }
    }

    def maybeRecoverFromDeletion(key: WatchKey, retry: Int = 0): Unit = {
      if (WatchKeyManager.hasBase(key)
        || WatchKeyManager.hasBaseFile(key)
        || WatchKeyManager.hasProxy(key)) {
        if (log.isTraceEnabled)
          log.trace("recover from deletion {}", keyToFile(key))

        if (!key.mkdirs && !key.exists) {
          if (retry <= 3) {
            Thread.sleep(20)
            log.error("retry re-create {} with parents", keyToFile(key))
            maybeRecoverFromDeletion(key, retry + 1)
          }
          log.error("Unable to re-create {} with parents", keyToFile(key))
        } else {
          val listeners = WatchKeyManager.listeners(key)
          val baseListeners = WatchKeyManager.baseListeners(key)
          val baseFileListeners = WatchKeyManager.baseFileListeners(key)
          listeners foreach (_.baseRemoved())
          baseFileListeners foreach (o => o.fileDeleted(o.base))
          WatchKeyManager.removeKey(key)
          watch(key, listeners, true)
        }
      } else if (WatchKeyManager.hasSubDir(key)) {
        WatchKeyManager.keyFromFile(key.getParentFile) match {
          case Some(p) => maybeRecoverFromDeletion(p)
          case None => log.warn(s"can not find a parent key")
        }
      }
    }

    def continueMonitoring() =
      (monitorThread match {
        case Some(t) => if (t.isInterrupted) {
          log.info("monitoring thread was interrupted")
          false
        } else true
        case None => {
          log.info("monitoring should run in a background thread")
          false
        }
      }) && shouldRun
  }

  def closeWatchService() = {
    try {
      log.info("close  WatchService")
      shouldRun = false
      watchService.close();
    } catch {
      case e: Throwable =>
        log.error("failed to close WatchService {}", e);
    }
  }

  def spawnWatcher(file: File, listeners: Set[WatcherListener]): Watcher = {
    spawnWatcher(UUID.randomUUID(), file, listeners)
  }

  def spawnWatcher(uuid: UUID, file: File, listeners: Set[WatcherListener]) = {
    if (log.isTraceEnabled)
      log.trace(s"spawn ${uuid} watcher for ${file} base")
    val w = new Watcher(uuid, file, listeners) {
      val fileWatchService = self;
    }
    w.watch()
    w
  }

  def maybeBuildWatchKeyObserver(f: File, l: WatcherListener): Option[WatchKeyObserver] = {
    if (!f.isDirectory) {
      log.warn("building a WatchKeyObserver for a non-existent {} doesn't make sense.", f)
      return None
    }
    if (l.base == f)
      Some(new BaseObserver(l))
    else if (l.base.isFile && l.base.getParentFile == f)
      Some(new BaseFileObserver(l))
    else if (l.recursive && f.getAbsolutePath.startsWith(l.base.getAbsolutePath))
      Some(new BaseSubdirObserver(l))
    else if (l.base.getAbsolutePath.startsWith(f.getAbsolutePath))
      Some(new ProxyObserver(l))
    else {
      log.warn(s"don't know what observer to create dir: ${f} for ${l.base.getAbsolutePath} base")
      None
    }
  }

  init()

  case class BaseObserver(val watcherListener: WatcherListener) extends WatchKeyObserver {
    override lazy val recursive = watcherListener.recursive
    override val observerType = "BaseObserver"
  }
  case class BaseFileObserver(val watcherListener: WatcherListener) extends WatchKeyObserver {
    val treatExistingAsNew = true
    val recursive = false
    override val observerType = "BaseFileObserver"
  }
  case class ProxyObserver(val watcherListener: WatcherListener) extends WatchKeyObserver {
    val recursive = false
    override val observerType = "ProxyObserver"
  }
  case class BaseSubdirObserver(val watcherListener: WatcherListener) extends WatchKeyObserver {
    override lazy val recursive = watcherListener.recursive
    override val observerType = "BaseSubdirObserver"
  }

  trait WatchKeyObserver {
    val watcherListener: WatcherListener
    val recursive: Boolean
    val observerType: String
  }

  object WatchKeyManager {
    val keymap: Map[WatchKey, Set[WatchKeyObserver]] = new ConcurrentHashMap().asScala

    def contains(key: WatchKey) = {
      keymap.contains(key)
    }

    @tailrec
    def addObserver(key: WatchKey, o: WatchKeyObserver): Unit = {
      val l = Set[WatchKeyObserver]()
      val oldListeners = keymap.putIfAbsent(key, l).getOrElse(l)
      val newListeners = oldListeners + o
      val status = keymap.replace(key, oldListeners, newListeners)
      if (!status) {
        log.warn(s"retry adding ${o.observerType} to ${keyToFile(key)}")
        addObserver(key, o)
      }
    }

    @tailrec
    def removeObserver(key: WatchKey, o: WatchKeyObserver, retry: Int = 2): Unit = {
      keymap.get(key) match {
        case Some(oldObservers) => {
          val newObservers = oldObservers - o
          if (newObservers.isEmpty) {
            keymap.remove(key)
            key.cancel()
          } else if (!keymap.replace(key, oldObservers, newObservers))
            if (retry > 0)
              removeObserver(key, o)
            else
              log.warn("unable to remove an observer from {}", keyToFile(key))
        }
        case None => log.warn(s"watcher doesn't monitor ${keyToFile(key)}")
      }
    }

    def maybeAdvanceProxy(key: WatchKey, createdFile: File) =
      proxies(key) foreach (o =>
        if (o.watcherListener.isBaseAncestor(createdFile))
          if (createdFile.isDirectory || createdFile.isFile) {
          removeObserver(key, o)
          watch(createdFile, Set(o.watcherListener), true)
        } else
          log.warn("unable to advance a proxy {}", o))

    def removeObservers(id: UUID) =
      keymap.keys foreach (
        key => {
          val observers = keymap.get(key).getOrElse { Set() }
          val unneeded = observers filter { _.watcherListener.watcherId == id }
          val retained = observers filter { _.watcherListener.watcherId != id }

          if (observers.size == 0 || unneeded.size == observers.size) {
            key.cancel()
            keymap.remove(key)
          } else if (observers.size != retained.size)
            if (!keymap.replace(key, observers, retained))
              log.error(s"failed to remove ${unneeded.size} listeners from  ${keyToFile(key)}")
        }
      )

    def baseFileObservers(key: WatchKey) =
      keymap getOrElse (key, Set()) filter {
        case _: BaseFileObserver => true
        case _ => false
      }

    def recListeners(key: WatchKey) =
      listeners(key) filter { _.recursive }

    def baseListeners(key: WatchKey) =
      keymap getOrElse (key, Set()) filter {
        case _: BaseObserver => true
        case _ => false
      } map { _.watcherListener }

    def baseFileListeners(key: WatchKey) =
      keymap getOrElse (key, Set()) filter {
        case _: BaseFileObserver => true
        case _ => false
      } map { _.watcherListener }

    def proxyListeners(key: WatchKey) =
      keymap getOrElse (key, Set()) filter {
        case _: ProxyObserver => true
        case _ => false
      } map { _.watcherListener }

    def nonProxyListeners(key: WatchKey) =
      keymap getOrElse (key, Set()) filter {
        case _: ProxyObserver => false
        case _ => true
      } map { _.watcherListener }

    def proxies(key: WatchKey) =
      keymap getOrElse (key, Set()) filter {
        case _: ProxyObserver => true
        case _ => false
      }

    def listeners(key: WatchKey) =
      keymap getOrElse (key, Set()) map { _.watcherListener }

    def removeKey(key: WatchKey): Unit = {
      key.cancel()
      keymap.remove(key)
    }

    def hasRecursive(key: WatchKey) =
      keymap.get(key) match {
        case Some(os) => os.exists { _.recursive }
        case None => false
      }

    def hasBase(key: WatchKey) =
      keymap.get(key) match {
        case Some(os) => os.exists {
          case _: BaseObserver => true
          case _ => false
        }
        case None => false
      }

    def hasSubDir(key: WatchKey) =
      keymap.get(key) match {
        case Some(os) => os.exists {
          case _: BaseSubdirObserver => true
          case _ => false
        }
        case None => false
      }

    def hasBaseFile(key: WatchKey) =
      keymap.get(key) match {
        case Some(os) => os.exists {
          case _: BaseFileObserver => true
          case _ => false
        }
        case None => false
      }

    def hasProxy(key: WatchKey) =
      keymap.get(key) match {
        case Some(os) => os.exists {
          case _: ProxyObserver => true
          case _ => false
        }
        case None => false
      }

    def hasBaseSubdir(key: WatchKey) =
      keymap.get(key) match {
        case Some(os) => os.exists {
          case _: BaseSubdirObserver => true
          case _ => false
        }
        case None => false
      }

    def totalKeyNum() =
      keymap.keys.foldLeft(0) { (a, _) => a + 1 }

    def keyFromFile(f: File): Option[WatchKey] =
      keymap.keys.find { k => keyToFile(k).getAbsolutePath == f.getAbsolutePath }
  }
}
