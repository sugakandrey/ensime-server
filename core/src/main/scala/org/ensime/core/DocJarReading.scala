// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import java.io.File
import java.util.jar.JarFile

import org.ensime.api.EnsimeConfig
import org.ensime.util.io._

trait DocJarReading {

  def docJarContent(filename: String, entry: String): Option[Array[Byte]]

  def docJars(): Set[File]
}

object DocJarReading {

  def forConfig(config: EnsimeConfig) = new DocJarReading {

    override def docJarContent(filename: String, entry: String): Option[Array[Byte]] = for {
      file <- config.allDocJars.find(_.getName == filename)
      jar = new JarFile(file)
      entry <- Option(jar.getJarEntry(entry))
      stream = jar.getInputStream(entry)
    } yield stream.toByteArray

    override def docJars(): Set[File] = config.allDocJars
  }
}
