// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.fixture

import java.io.{ File => JFile }
import java.nio.charset.Charset
import java.nio.file.Files

import org.ensime.api._
import org.ensime.config._
import org.ensime.util.file._
import org.ensime.util.path._
import org.scalatest._
import org.slf4j.LoggerFactory

/**
 * Provides a fixture for tests to have access to a cloned project,
 * based on an example project that will be untouched.
 */
trait EnsimeConfigFixture {
  /** The definition of the original project to clone for testing. */
  def original: EnsimeConfig

  /** See .drone.yml to set up a pre-warmed index to speed up testing. Will be used if available. */
  def usePreWarmedIndex: Boolean = true

  def copyTargets: Boolean = true

  def withEnsimeConfig(testCode: EnsimeConfig => Any): Any

  // convenience method
  def main(lang: String)(implicit config: EnsimeConfig): File =
    config.subprojects.head.sourceRoots.filter { dir =>
      val sep = JFile.separator
      dir.getPath.endsWith(s"${sep}main$sep$lang")
    }.head
  def scalaMain(implicit config: EnsimeConfig): File = main("scala")
  def javaMain(implicit config: EnsimeConfig): File = main("java")

  def mainTarget(implicit config: EnsimeConfig): File =
    config.subprojects.head.targets.head
}

object EnsimeConfigFixture {
  private[this] val log = LoggerFactory.getLogger(getClass)

  lazy val dotEnsime = File("../.ensime")
  if (!dotEnsime.exists) {
    System.err.println(
      "The .ensime file must exist to run the integration tests." +
        " Type 'sbt ensimeConfig' to create it"
    )
    System.err.flush()
    sys.exit(1)
  }

  private implicit def charset = Charset.defaultCharset()
  lazy val EnsimeTestProject = EnsimeConfigProtocol.parse(dotEnsime.readString())

  lazy val EnsimeCacheProject = {
    val config = File("../testing/cache/.ensime").canon
    if (!config.exists()) {
      println(s"consider pre-warming testing/cache (see .drone.yml) to speed up your tests")
      None
    } else {
      Some(EnsimeConfigProtocol.parse(config.readString()))
    }
  }

  lazy val EmptyTestProject: EnsimeConfig = EnsimeTestProject.copy(
    subprojects = EnsimeTestProject.subprojects.filter(_.name == "testing_empty")
  )
  lazy val SimpleTestProject: EnsimeConfig = EnsimeTestProject.copy(
    subprojects = EnsimeTestProject.subprojects.filter(_.name == "testing_simple")
  )
  lazy val SimpleJarTestProject: EnsimeConfig = EnsimeTestProject.copy(
    subprojects = EnsimeTestProject.subprojects.filter(_.name == "testing_simpleJar")
  )
  lazy val ImplicitsTestProject: EnsimeConfig = EnsimeTestProject.copy(
    subprojects = EnsimeTestProject.subprojects.filter(_.name == "testing_implicits")
  )
  lazy val TimingTestProject: EnsimeConfig = EnsimeTestProject.copy(
    subprojects = EnsimeTestProject.subprojects.filter(_.name == "testing_timing")
  )
  lazy val MacrosTestProject: EnsimeConfig = EnsimeTestProject.copy(
    subprojects = EnsimeTestProject.subprojects.filter(_.name == "testing_macros")
  )
  lazy val ShapelessTestProject: EnsimeConfig = EnsimeTestProject.copy(
    subprojects = EnsimeTestProject.subprojects.filter(_.name == "testing_shapeless")
  )
  lazy val FqnsTestProject: EnsimeConfig = EnsimeTestProject.copy(
    subprojects = EnsimeTestProject.subprojects.filter(_.name == "testing_fqns")
  )
  lazy val DebugTestProject: EnsimeConfig = EnsimeTestProject.copy(
    subprojects = EnsimeTestProject.subprojects.filter(_.name == "testing_debug")
  )
  lazy val DocsTestProject: EnsimeConfig = EnsimeTestProject.copy(
    subprojects = EnsimeTestProject.subprojects.filter(_.name == "testing_docs")
  )
  lazy val JavaTestProject: EnsimeConfig = EnsimeTestProject.copy(
    subprojects = EnsimeTestProject.subprojects.filter(_.name == "testing_java")
  )

  // generates an empty single module project in a temporary directory
  // and returns the config, containing many of the same settings
  // as the ensime-server project itself (source/dependency jars),
  // with options to copy ENSIME's own sources/classes into the structure.
  def cloneForTesting(
    source: EnsimeConfig,
    target: File,
    copyTargets: Boolean,
    preWarm: Boolean
  ): EnsimeConfig = {

    def rename(from: File): File = {
      val toPath = from.getAbsolutePath.replace(
        source.root.getAbsolutePath,
        target.getAbsolutePath
      )
      require(toPath != from.getAbsolutePath, s"${source.root.getAbsolutePath} ${target.getAbsolutePath} in ${from.getAbsolutePath}")
      File(toPath)
    }

    def renameAndCopy(from: File): File = {
      val to = rename(from)

      if (from.isDirectory) {
        Files.createDirectories(to.toPath)
        from.toPath.copyDirTo(to.toPath)
      } else {
        val parent = to.getParentFile
        if (!parent.exists())
          Files.createDirectories(parent.toPath)

        Files.copy(from.toPath, to.toPath)
      }
      to
    }

    def renameAndCopyTarget(from: File): File =
      if (copyTargets) renameAndCopy(from)
      else rename(from)

    def cloneModule(m: EnsimeModule): EnsimeModule = m.copy(
      targets = m.targets.map(renameAndCopyTarget),
      testTargets = m.testTargets.map(renameAndCopyTarget),
      sourceRoots = m.sourceRoots.map(renameAndCopy)
    )

    val cacheDir = target / ".ensime_cache"
    cacheDir.mkdirs()
    val config = EnsimeConfigProtocol.validated(source.copy(
      rootDir = rename(source.rootDir),
      cacheDir = cacheDir,
      subprojects = source.subprojects.map(cloneModule)
    ))

    // HACK: we must force OS line endings on sources or the tests
    // (which have fixed points within the file) will fail on Windows
    config.scalaSourceFiles.foreach { file =>
      file.writeLines(file.readLines())
    }

    if (preWarm && (config.compileClasspath ++ config.javaLibs).nonEmpty)
      EnsimeCacheProject.foreach { cacheProject =>
        log.info(s"copying ${cacheProject.cacheDir}")
        cacheProject.cacheDir.toPath.copyDirTo(config.cacheDir.toPath)
      }

    config
  }
}

/**
 * Provides the basic building blocks to build custom fixtures around
 * a project that is cloned for every test in a suite.
 *
 * Implementations tend to run very slowly, so consider using
 * `SharedConfigFixture` if possible, or reducing your configuration
 * parameters to the bare minimal (e.g. remove JRE and dependencies to
 * index if not needed).
 */
trait IsolatedEnsimeConfigFixture extends Suite
    with EnsimeConfigFixture {
  //running in parallel actually slows things down
  //with ParallelTestExecution {
  import EnsimeConfigFixture._

  override def withEnsimeConfig(testCode: EnsimeConfig => Any): Any = withTempDir { dir =>
    testCode(cloneForTesting(original, dir, copyTargets, usePreWarmedIndex))
  }
}

/**
 * Provides the basic building blocks to build custom fixtures around
 * a project that is cloned once for the test suite.
 */
trait SharedEnsimeConfigFixture extends Suite
    with EnsimeConfigFixture with BeforeAndAfterAll {
  import EnsimeConfigFixture._

  private val tmpDir = Files.createTempDirectory("ensime").toFile

  private[fixture] var _config: EnsimeConfig = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    _config = cloneForTesting(original, tmpDir, copyTargets, usePreWarmedIndex)
  }

  override def afterAll(): Unit = {
    super.afterAll()
    tmpDir.toPath.deleteDirRecursively()
  }

  override def withEnsimeConfig(testCode: EnsimeConfig => Any): Any = testCode(_config)

}
