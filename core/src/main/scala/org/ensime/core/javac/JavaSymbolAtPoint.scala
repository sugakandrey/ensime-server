// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core.javac

import scala.collection.JavaConverters._

import com.sun.source.tree.{ IdentifierTree, MemberSelectTree }
import com.sun.source.util.TreePath
import javax.lang.model.`type`.{ ExecutableType, TypeMirror }
import org.ensime.api._
import org.ensime.model.BasicTypeInfo

trait JavaSymbolAtPoint { requires: JavaCompiler =>

  def askSymbolAtPoint(file: SourceFileInfo, offset: Int): Option[SymbolInfo] = {
    pathToPoint(file, offset) flatMap {
      case (c: Compilation, path: TreePath) =>
        for {
          identifierName <- path.getLeaf match {
            case t: IdentifierTree => Some(t.getName.toString)
            case t: MemberSelectTree => Some(t.getIdentifier.toString)
            case _ => None
          }
          typeInfo <- Option(c.trees.getTypeMirror(path))
            .map(typeMirrorToTypeInfo(identifierName, _))
        } yield {

          SymbolInfo(
            fqn(c, path)
              .map(toSymbolName(_))
              .getOrElse(identifierName),
            identifierName,
            findDeclPos(c, path),
            typeInfo
          )
        }
    }
  }

  private def typeMirrorToTypeInfo(identifierName: String, t: TypeMirror): TypeInfo = t match {
    case t: ExecutableType => executableTypeToTypeInfo(identifierName, t)
    case t => BasicTypeInfo(t.toString, DeclaredAs.Class, t.toString)
  }

  private def typeMirrorToTypeInfo(t: TypeMirror): TypeInfo =
    BasicTypeInfo(t.toString, DeclaredAs.Class, t.toString)

  private def name(identifierName: String, t: ExecutableType)(formatType: TypeMirror => String): String = {

    val params = t.getParameterTypes.asScala.zipWithIndex.map {
      case (p, i) =>
        val paramType = formatType(p)
        s"$paramType arg$i"
    }.mkString("(", ", ", ")")

    val returns = formatType(t.getReturnType)

    s"$returns $identifierName$params"
  }

  private def fullName(identifierName: String, t: ExecutableType): String = name(identifierName, t)(_.toString())
  private def shortName(identifierName: String, t: ExecutableType): String = name(identifierName, t)(_.toString.split("\\.").last)

  private def executableTypeToTypeInfo(identifierName: String, t: ExecutableType): TypeInfo = {

    val returnType = t.getReturnType

    ArrowTypeInfo(
      shortName(identifierName, t), fullName(identifierName, t),
      typeMirrorToTypeInfo(t.getReturnType),
      ParamSectionInfo(
        t.getParameterTypes.asScala.zipWithIndex.map {
          case (param, index) =>
            s"arg$index" -> typeMirrorToTypeInfo(param)
        },
        isImplicit = false
      ) :: Nil, Nil
    )
  }
}
