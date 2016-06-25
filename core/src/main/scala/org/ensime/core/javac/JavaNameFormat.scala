// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core.javac

import javax.lang.model.`type`.TypeMirror
import javax.lang.model.element.Element
import javax.lang.model.element.ExecutableElement
import scala.collection.JavaConversions._

case class JavaName(short: String, full: String)

trait TypeNameFormat[T <: TypeMirror] {

  def apply(t: T): JavaName

}

object DefaultTypeNameFormat extends TypeNameFormat[TypeMirror] {

  def apply(t: TypeMirror): JavaName = JavaName(t.toString, t.toString) // I think this also should be t.toString.split("\\.").last (short)
}

trait ElementNameFormat[T <: Element] {

  def apply(t: T): JavaName

}

object SymbolAtPointMethodNameFormat extends ElementNameFormat[ExecutableElement] {

  private def elementName(e: ExecutableElement)(formatType: TypeMirror => String): String = {

    val params = e.getParameters.map { p =>
      val paramType = formatType(p.asType())
      val paramName = p.getSimpleName
      s"$paramType $paramName"
    }.mkString("(", ", ", ")")

    val returns = formatType(e.getReturnType)

    //val modifiers = e.getModifiers.mkString(" ")

    val name = e.getSimpleName

    s"$returns $name$params"
  }

  def fullName(e: ExecutableElement): String = elementName(e)(_.toString())
  def shortName(e: ExecutableElement): String = elementName(e)(_.toString.split("\\.").last)

  def apply(e: ExecutableElement) = JavaName(shortName(e), fullName(e))
}

object TypeAtPointMethodNameFormat extends ElementNameFormat[ExecutableElement] {

  private def elementName(e: ExecutableElement)(formatType: TypeMirror => String): String = {

    val params = e.getParameters.map { p =>
      val paramType = formatType(p.asType())
      val paramName = p.getSimpleName
      s"$paramType $paramName"
    }.mkString("(", ", ", ")")

    val returns = formatType(e.getReturnType)

    s"$returns $params"
  }

  def fullName(e: ExecutableElement): String = elementName(e)(_.toString())
  def shortName(e: ExecutableElement): String = elementName(e)(_.toString.split("\\.").last)

  def apply(e: ExecutableElement) = JavaName(shortName(e), fullName(e))
}

object CompletionAtPointMethodNameFormat extends ElementNameFormat[ExecutableElement] {

  private def elementName(e: ExecutableElement)(formatType: TypeMirror => String): String = {

    val params = e.getParameters.map { p =>
      val paramType = formatType(p.asType())
      val paramName = p.getSimpleName
      s"$paramType $paramName"
    }.mkString("(", ", ", ")")

    val returns = formatType(e.getReturnType)

    s"$returns $params"
  }

  def fullName(e: ExecutableElement): String = elementName(e)(_.toString())
  def shortName(e: ExecutableElement): String = elementName(e)(_.toString.split("\\.").last)

  def apply(e: ExecutableElement) = JavaName(shortName(e), fullName(e))
}

object JavaNameFormat {

  implicit val symbolAtPointMethodNameFormat = SymbolAtPointMethodNameFormat
  implicit val typeAtPointMethodNameFormat = TypeAtPointMethodNameFormat
  implicit val completionAtPointMethodNameFormat = CompletionAtPointMethodNameFormat

  implicit val defaultTypeNameFormat = DefaultTypeNameFormat

}
