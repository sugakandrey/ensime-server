// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.indexer

import scala.tools.scalap.scalax.rules.scalasig._

import com.google.common.io.ByteStreams
import org.apache.commons.vfs2.FileObject
import org.ensime.core.ScalapSymbolToFqn

class ClassfileDepickler(file: FileObject) extends ScalapSymbolToFqn {

  val scalasig: Option[ScalaSig] = depickle

  /** Uses scalap to produce a scala reflective view of the classfile */
  private def depickle: Option[ScalaSig] = {
    val in = file.getContent.getInputStream
    try {
      val bytes = ByteStreams.toByteArray(in)
      val byteCode = ByteCode(bytes)
      val classFile = ClassFileParser.parse(byteCode)
      ScalaSigParser.parse(classFile)
    } catch {
      // ClassFileParser fails to parse some JDK class files
      case e: Exception => None
    } finally in.close()
  }

  private val ignore = Set("<local child>", "<refinement>", "anon")
  def getClasses: Map[String, RawScalapClass] = scalasig.fold(Map.empty[String, RawScalapClass]) { sig =>
    sig.symbols.collect {
      case s: ClassSymbol if !(ignore.exists(s.name.contains) || s.isSynthetic) =>
        val aClass = rawScalaClass(s)
        aClass.javaName.fqnString -> aClass
    }.toMap
  }
}
