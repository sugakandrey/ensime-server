// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.indexer

import java.nio.charset.Charset

import scala.concurrent.duration._

import akka.testkit._
import com.google.common.io.Files
import org.apache.commons.vfs2._
import org.ensime.fixture._
import org.ensime.util._
import org.ensime.util.file._
import org.ensime.util.fileobject._
import org.ensime.vfs._
import org.scalatest.tagobjects.Retryable

sealed trait FileWatcherMessage
final case class Added(f: FileObject) extends FileWatcherMessage
final case class Removed(f: FileObject) extends FileWatcherMessage
final case class Changed(f: FileObject) extends FileWatcherMessage
final case class BaseAdded(f: FileObject) extends FileWatcherMessage
final case class BaseRemoved(f: FileObject) extends FileWatcherMessage
final case class BaseRegistered() extends FileWatcherMessage

/**
 * These tests are insanely flakey so everything is retryable. The
 * fundamental problem is that file watching is impossible without
 * true OS and FS support, which is lacking on all major platforms.
 *
 * NOTE: if you're making local edits to this file, try adding "with
 *       ParallelTestExecution". It's not used by default, only to
 *       reduce the load on the pathetic CI machines.
 */
class FileWatcherSpec extends EnsimeSpec
    with IsolatedTestKitFixture with IsolatedEnsimeVFSFixture {

  implicit val DefaultCharset: Charset = Charset.defaultCharset()

  // variant that watches a jar file
  def createJarWatcher(jar: File)(implicit vfs: EnsimeVFS, tk: TestKit): Watcher =
    (new JarJava7WatcherBuilder()).build(jar, listeners)

  // variant that recursively watches a directory of classes
  def createClassWatcher(base: File)(implicit vfs: EnsimeVFS, tk: TestKit): Watcher =
    (new ClassJava7WatcherBuilder()).build(base, listeners)

  /**
   * The Linux ext2+ filesystems have a timestamp precision of 1
   * second, which means its impossible to tell if a newly created
   * file has been modified, or deleted and re-added, if it happens
   * sub-second (without looking at the contents).
   */
  def waitForLinus(): Unit = {
    Thread.sleep(1000)
  }

  val maxWait = 20 seconds

  "FileWatcher" should "detect added files" taggedAs (Retryable) in
    withVFS { implicit vfs =>
      withTestKit { implicit tk =>
        withTempDir { dir =>
          withClassWatcher(dir) { watcher =>
            waitForBaseRegistered(tk)

            val foo = (dir / "foo.class")
            val bar = (dir / "b/bar.class")

            foo.createWithParents() shouldBe true
            bar.createWithParents() shouldBe true

            val fishForFooBar: Fish = {
              case Added(f) => {
                f.asLocalFile.getAbsolutePath == foo.getAbsolutePath ||
                  f.asLocalFile.getAbsolutePath == bar.getAbsolutePath
              }
              case _ => false
            }
            tk.fishForMessage(maxWait)(fishForFooBar)
            tk.fishForMessage(maxWait)(fishForFooBar)
          }
        }
      }
    }

  it should "detect added / changed files" taggedAs (Retryable) in
    withVFS { implicit vfs =>
      withTestKit { implicit tk =>
        withTempDir { dir =>
          withClassWatcher(dir) { watcher =>
            waitForBaseRegistered(tk)

            val foo = (dir / "foo.class")
            val bar = (dir / "b/bar.class")

            foo.createWithParents() shouldBe true
            bar.createWithParents() shouldBe true
            tk.expectMsgType[Added]
            tk.expectMsgType[Added]
            ignoreAdded(tk)
            ignoreAdded(tk)
            waitForLinus()

            foo.writeString("foo")
            bar.writeString("bar")
            tk.expectMsgType[Changed]
            tk.expectMsgType[Changed]
          }
        }
      }
    }

  it should "detect added / removed files" taggedAs (Retryable) in
    withVFS { implicit vfs =>
      withTestKit { implicit tk =>
        withTempDir { dir =>
          withClassWatcher(dir) { watcher =>
            waitForBaseRegistered(tk)
            tk.ignoreMsg {
              case msg: Changed => true // ignore on Windows
            }

            val foo = (dir / "foo.class")
            val bar = (dir / "b/bar.class")

            foo.createWithParents() shouldBe true
            bar.createWithParents() shouldBe true
            tk.expectMsgType[Added]
            tk.expectMsgType[Added]
            ignoreAdded(tk)
            ignoreAdded(tk)

            waitForLinus()

            foo.delete()
            bar.delete()
            tk.expectMsgType[Removed]
            tk.expectMsgType[Removed]
          }
        }
      }
    }

  it should "detect removed base directory" taggedAs (Retryable) in
    withVFS { implicit vfs =>
      withTestKit { implicit tk =>
        withTempDir { dir =>
          withClassWatcher(dir) { watcher =>
            waitForBaseRegistered(tk)
            waitForLinus()

            dir.delete()

            val createOrDelete: Fish = {
              case r: BaseRemoved => true
              case a: BaseAdded => true
              case _ => false
            }

            tk.fishForMessage()(createOrDelete)
            tk.fishForMessage()(createOrDelete)
          }
        }
      }
    }

  it should "detect removed parent base directory" taggedAs (Retryable) in
    withVFS { implicit vfs =>
      withTestKit { implicit tk =>
        val parent = Files.createTempDir().canon
        val dir = parent / "base"
        dir.mkdirs()
        try {
          withClassWatcher(dir) { watcher =>
            // would be better if this was atomic (not possible from JVM?)
            waitForBaseRegistered(tk)

            parent.tree.reverse.foreach(_.delete())
            val createOrDelete: Fish = {
              case r: BaseRemoved => true
              case a: BaseAdded => true
              case _ => false
            }
            tk.fishForMessage()(createOrDelete)
            tk.fishForMessage()(createOrDelete)
          }
        } finally parent.tree.reverse.foreach(_.delete())
      }
    }

  it should "survive deletion of the watched directory" taggedAs (Retryable) in
    withVFS { implicit vfs =>
      withTestKit { implicit tk =>
        withTempDir { dir =>
          withClassWatcher(dir) { watcher =>
            waitForBaseRegistered(tk)
            val foo = (dir / "foo.class")
            val bar = (dir / "b/bar.class")

            foo.createWithParents() shouldBe true
            bar.createWithParents() shouldBe true
            tk.expectMsgType[Added]
            tk.expectMsgType[Added]
            ignoreAdded(tk)
            ignoreAdded(tk)
            waitForLinus()
            dir.tree.reverse.foreach(_.delete())

            val createOrDelete: Fish = {
              case r: BaseRemoved => true
              case a: BaseAdded => true
              case r: Removed => false
              case r: Added => false // java7 watcher can detect it twice as existing and created
              case r: Changed => false // ignore on Windows
            }

            tk.fishForMessage()(createOrDelete)
            tk.fishForMessage()(createOrDelete)

            foo.createWithParents() shouldBe true
            bar.createWithParents() shouldBe true
            val nonDeterministicAdd: Fish = {
              case a: Added => true
              case c: Changed => true
              case r: Removed => false
              case r: BaseRemoved => false
              case r: BaseAdded => false
            }
            tk.fishForMessage()(nonDeterministicAdd)
            tk.fishForMessage()(nonDeterministicAdd)
          }
        }
      }
    }

  it should "be able to start up from a non-existent directory" taggedAs (Retryable) in
    withVFS { implicit vfs =>
      withTestKit { implicit tk =>
        val dir = Files.createTempDir().canon / "root"
        try {
          withClassWatcher(dir) { watcher =>
            tk.ignoreMsg {
              case msg: BaseAdded => true
            }
            val foo = (dir / "foo.class")
            val bar = (dir / "b/bar.class")

            waitForLinus()

            foo.createWithParents() shouldBe true
            bar.createWithParents() shouldBe true

            val fishForFooBar: Fish = {
              case Added(f) => {
                f.asLocalFile.getAbsolutePath == foo.getAbsolutePath ||
                  f.asLocalFile.getAbsolutePath == bar.getAbsolutePath
              }
              case _ => false
            }
            tk.fishForMessage(maxWait)(fishForFooBar)
            tk.fishForMessage(maxWait)(fishForFooBar)

          }
        } finally dir.tree.reverse.foreach(_.delete())
      }
    }

  it should "survive removed parent base directory and recreated base" taggedAs (Retryable) in
    withVFS { implicit vfs =>
      withTestKit { implicit tk =>

        val parent = Files.createTempDir().canon
        val dir = parent / "base"
        dir.mkdirs()
        try {
          withClassWatcher(dir) { watcher =>
            waitForBaseRegistered(tk)

            val foo = (dir / "foo.class")
            val bar = (dir / "b/bar.class")

            foo.createWithParents() shouldBe true
            bar.createWithParents() shouldBe true
            tk.expectMsgType[Added]
            tk.expectMsgType[Added]
            ignoreAdded(tk)
            ignoreAdded(tk)
            waitForLinus()

            parent.tree.reverse.foreach(_.delete())

            val createOrDelete: Fish = {
              case r: BaseRemoved => true
              case a: BaseAdded => true
              case r: Removed => false
              case a: Added => false
              case r: Changed => false // ignore on Windows
            }
            tk.fishForMessage()(createOrDelete)
            tk.fishForMessage()(createOrDelete)

            foo.createWithParents() shouldBe true
            bar.createWithParents() shouldBe true

            // non-deterministically receive zero, one or two more Removed
            // and either Added or Changed for foo / bar.
            val nonDeterministicAdd: Fish = {
              case a: Added => true
              case c: Changed => true
              case r: Removed => false
              case r: BaseRemoved => false //ignore on Windows
              case r: BaseAdded => false // ignore on Windows
            }
            tk.fishForMessage()(nonDeterministicAdd)
            tk.fishForMessage()(nonDeterministicAdd)
          }
        } finally dir.tree.reverse.foreach(_.delete())
      }
    }

  //////////////////////////////////////////////////////////////////////////////
  it should "detect changes to a file base" taggedAs (Retryable) in
    withVFS { implicit vfs =>
      withTestKit { implicit tk =>
        withTempDir { dir =>

          val jar = (dir / "jar.jar")
          jar.createWithParents() shouldBe true

          withJarWatcher(jar) { watcher =>
            waitForBaseRegistered(tk)
            waitForLinus()

            jar.writeString("binks")
            tk.expectMsgType[Changed]
          }
        }
      }
    }

  it should "detect removal of a file base" taggedAs (Retryable) in
    withVFS { implicit vfs =>
      withTestKit { implicit tk =>
        withTempDir { dir =>
          val jar = (dir / "jar.jar")
          jar.createWithParents() shouldBe true

          withJarWatcher(jar) { watcher =>
            waitForBaseRegistered(tk)
            waitForLinus()
            tk.ignoreMsg {
              case msg: Changed => true // ignore on Windows
            }

            jar.delete()
            tk.expectMsgType[Removed]
          }
        }
      }
    }

  it should "be able to start up from a non-existent base file" taggedAs (Retryable) in
    withVFS { implicit vfs =>
      withTestKit { implicit tk =>
        withTempDir { dir =>
          val jar = (dir / "jar.jar")
          withJarWatcher(jar) { watcher =>
            waitForLinus()
            tk.ignoreMsg {
              case msg: BaseAdded => true
            }

            jar.createWithParents() shouldBe true

            tk.expectMsgType[Added]
          }
        }
      }
    }

  it should "survive removal of a file base" taggedAs (Retryable) in
    withVFS { implicit vfs =>
      withTestKit { implicit tk =>
        withTempDir { dir =>
          val jar = (dir / "jar.jar")
          jar.createWithParents() shouldBe true

          withJarWatcher(jar) { watcher =>
            waitForBaseRegistered(tk)
            tk.ignoreMsg {
              case msg: Changed => true // ignore on Windows
            }

            waitForLinus()

            jar.delete() // best thing for him, frankly
            tk.expectMsgType[Removed]

            waitForLinus()
            jar.writeString("binks")
            tk.expectMsgType[Added]
          }
        }
      }
    }

  //////////////////////////////////////////////////////////////////////////////
  type -->[A, B] = PartialFunction[A, B]
  type Fish = PartialFunction[Any, Boolean]

  def waitForBaseRegistered(tk: TestKit) = {
    val baseCreated: Fish = {
      case BaseRegistered() => true //f == dir
      // case e => { logEvent("Bad ", dir, e); false }
      case e => false
    }
    tk.fishForMessage(5 seconds)(baseCreated)
  }

  def ignoreAdded(tk: TestKit) = {
    // Ignore extra Added message because
    // java7 watcher can detect a file in a directory as existing
    // or/and as created.
    try {
      val baseCreated: Fish = {
        case Added(f) => true
        case e => false
      }
      tk.fishForMessage(1 second)(baseCreated)
    } catch {
      case e: Throwable => true
    }
  }
  def withClassWatcher[T](base: File)(code: Watcher => T)(implicit vfs: EnsimeVFS, tk: TestKit) = {
    val w = createClassWatcher(base)
    try code(w)
    finally w.shutdown()
  }

  def withJarWatcher[T](jar: File)(code: Watcher => T)(implicit vfs: EnsimeVFS, tk: TestKit) = {
    val w = createJarWatcher(jar)
    try code(w)
    finally w.shutdown()
  }

  def listeners(implicit vfs: EnsimeVFS, tk: TestKit) = List(
    new FileChangeListener {
      def fileAdded(f: FileObject): Unit = { tk.testActor ! Added(f) }
      def fileRemoved(f: FileObject): Unit = { tk.testActor ! Removed(f) }
      def fileChanged(f: FileObject): Unit = { tk.testActor ! Changed(f) }
      override def baseReCreated(f: FileObject): Unit = { tk.testActor ! BaseAdded(f) }
      override def baseRemoved(f: FileObject): Unit = { tk.testActor ! BaseRemoved(f) }
      override def baseRegistered(): Unit = { tk.testActor ! BaseRegistered() }
    }
  )
}
