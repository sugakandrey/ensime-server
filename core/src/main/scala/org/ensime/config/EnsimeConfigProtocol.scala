// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.config

import java.io.File
import akka.event.slf4j.Logger
import scala.util.Properties
import shapeless._

import org.ensime.sexp._
import org.ensime.sexp.formats._
import org.ensime.core.Canonised

import org.ensime.util.file._

import org.ensime.api._

object EnsimeConfigProtocol {
  object Protocol extends DefaultSexpProtocol
    with OptionAltFormat
    with CamelCaseToDashes
  import org.ensime.config.EnsimeConfigProtocol.Protocol._

  private def log = Logger(this.getClass.getName)

  private implicit val moduleFormat: SexpFormat[EnsimeModule] = cachedImplicit
  private implicit val configFormat: SexpFormat[EnsimeConfig] = cachedImplicit

  def parse(config: String): EnsimeConfig = {
    val raw = config.parseSexp.convertTo[EnsimeConfig]
    validated(raw).copy(javaLibs = inferJavaLibs(raw.javaHome))
  }

  // there are lots of JRE libs, but most people only care about
  // rt.jar --- this could be parameterised.
  private def inferJavaLibs(javaHome: File): List[File] =
    // WORKAROUND https://github.com/ensime/ensime-server/issues/886
    // speeds up the emacs integration tests significantly,
    if (Properties.envOrNone("ENSIME_SKIP_JRE_INDEX").isDefined) Nil
    else javaHome.tree.filter(_.getName == "rt.jar").toList

  def validated(c: EnsimeConfig): EnsimeConfig = c.copy(
    subprojects = c.subprojects.map(validated)
  )

  /*
   We use the canonical form of files/directories to keep OS X happy
   when loading S-Expressions. But the canon may fail to resolve if
   the file/directory does not exist, so we force create all required
   directories and then re-canon them, which is - admittedly - a weird
   side-effect.
   */
  private[config] def validated(m: EnsimeModule): EnsimeModule = {
    (m.targets ++ m.testTargets ++ m.sourceRoots).foreach { dir =>
      if (!dir.exists() && !dir.isJar) {
        log.warn(s"$dir does not exist, creating")
        dir.mkdirs()
      }
    }
    Canonised(m)
  }
}
