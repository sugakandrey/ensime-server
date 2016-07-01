// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core.javac

import com.sun.source.util.TreePath
import org.ensime.api.{ TypeInfo, ArrowTypeInfo, ParamSectionInfo }
import org.ensime.api.{ SourceFileInfo, DeclaredAs }
import org.ensime.model.BasicTypeInfo

import javax.lang.model.`type`.{ TypeMirror, ExecutableType }

import scala.collection.JavaConversions._

trait JavaTypeAtPoint { requires: JavaCompiler =>

  def askTypeAtPoint(file: SourceFileInfo, offset: Int): Option[TypeInfo] = {
    pathToPoint(file, offset) flatMap {
      case (c: Compilation, path: TreePath) =>
        Option(c.trees.getTypeMirror(path)).map(typeMirrorToTypeInfo)
    }
  }

  private def typeMirrorToTypeInfo(t: TypeMirror): TypeInfo = t match {
    case t: ExecutableType => executableTypeToTypeInfo(t)
    case t => BasicTypeInfo(t.toString, DeclaredAs.Class, t.toString)

  }

  private def name(t: ExecutableType)(formatType: TypeMirror => String): String = {

    val params = t.getParameterTypes
      .map(formatType)
      .mkString("(", ", ", ")")

    val returns = formatType(t.getReturnType)

    s"$returns $params"
  }

  private def fullName(t: ExecutableType): String = name(t)(_.toString())
  private def shortName(t: ExecutableType): String = name(t)(_.toString.split("\\.").last)

  private def executableTypeToTypeInfo(t: ExecutableType): TypeInfo = {
    ArrowTypeInfo(
      shortName(t), fullName(t),
      typeMirrorToTypeInfo(t.getReturnType),
      ParamSectionInfo(
        t.getParameterTypes.zipWithIndex.map {
          case (param, index) =>
            s"arg$index" -> typeMirrorToTypeInfo(param)
        },
        isImplicit = false
      ) :: Nil, Nil
    )
  }
}
