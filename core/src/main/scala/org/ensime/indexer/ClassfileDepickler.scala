// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.indexer

import scala.collection.breakOut
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

  def getTypeAliases: Seq[RawType] = withScalaSig { sig =>
    sig.symbols.collect {
      case s: AliasSymbol => RawType(symbolName(s), access(s))
    }(breakOut)
  }

  def getClasses: Seq[ClassName] = withScalaSig { sig =>
    sig.symbols.collect {
      case s: ClassSymbol if !s.name.contains("<local child>") => className(s)
    }(breakOut)
  }

  def getFields: Seq[FieldName] = withScalaSig { sig =>
    sig.symbols.collect {
      case s: MethodSymbol if !s.isMethod && s.isLocal => fieldName(s)
    }(breakOut)
  }

  def getMethods: Seq[MethodName] = withScalaSig { sig =>
    sig.symbols.collect {
      case ms: MethodSymbol if ms.isMethod && ms.parent.forall(!_.name.contains("<refinement>")) => methodName(ms)
    }(breakOut)
  }

  private def withScalaSig[A](code: ScalaSig => Seq[A]): Seq[A] = scalasig.fold(Seq.empty[A])(sig => code(sig))

  private def access(sym: Symbol): Access = {
    if (sym.isPrivate) Private
    else if (sym.isProtected) Protected
    else Public
  }

  private def symbolName(a: Symbol): String = {
    a.parent match {
      case Some(s: SymbolInfoSymbol) => symbolName(s) + "$" + a.name
      case Some(s: Symbol) => s.toString + "." + a.name
      case None => a.name
    }
  }
}
