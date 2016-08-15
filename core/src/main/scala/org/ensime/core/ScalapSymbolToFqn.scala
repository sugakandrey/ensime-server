// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import java.io.{ ByteArrayOutputStream, PrintStream }
import java.nio.charset.StandardCharsets

import org.ensime.api.DeclaredAs
import org.ensime.indexer._

import scala.tools.scalap.scalax.rules.scalasig._
import scala.collection.breakOut
import scala.util.control.NonFatal

trait ScalapSymbolToFqn {
  import ScalaSigApi._

  private def withScalaSigPrinter(code: ScalaSigPrinter => Any): String = {
    val baos = new ByteArrayOutputStream()
    val ps = new PrintStream(baos)
    val printer = new ScalaSigPrinter(ps, true)
    try {
      code(printer)
      new String(baos.toByteArray, StandardCharsets.UTF_8)
    } catch {
      case NonFatal(e) => ""
    }
  }

  private def getAccess(sym: Symbol): Access =
    if (sym.isPrivate) Private
    else if (sym.isProtected) Protected
    else Public

  def rawType(s: AliasSymbol, parentPrefix: String): RawType = {
    val parentName = className(s.symbolInfo.owner)
    val isParentModule = parentName.fqnString.endsWith("$")
    val javaName = ClassName(parentName.pack, parentName.name + (if (isParentModule) "" else "$") + s.name)
    val scalaName = parentPrefix + (if (isParentModule) "." else "#") + s.name
    val access = getAccess(s)
    val typeSignature = withScalaSigPrinter { printer =>
      printer.printType(s.infoType, " = ")(printer.TypeFlags(true))
    }
    RawType(parentName, javaName, scalaName, access, typeSignature)
  }

  def rawScalaClass(sym: ClassSymbol): RawScalapClass = {
    val javaName = className(sym)
    val aPackage = sym.enclosingPackage
    val ownerChain = sym.ownerChain
    val name =
      ownerChain.init.map(s => s.name + (if (s.isModule) "." else "#")).mkString + ownerChain.last.name

    val access = getAccess(sym)

    val declaredAs =
      if (sym.isTrait) DeclaredAs.Trait
      else if (sym.isModule) DeclaredAs.Object
      else DeclaredAs.Class

    val typeSignature = withScalaSigPrinter { printer =>
      printer.printType(sym.infoType)(printer.TypeFlags(true))
    }

    val scalaName = aPackage + "." + name
    val parentPrefix = if (sym.isModule) scalaName + "." else scalaName + "#"
    val fields: Map[String, RawScalapField] = sym.children.collect {
      case ms: MethodSymbol if !ms.isMethod && ms.isLocal =>
        val field = rawScalaField(ms, parentPrefix)
        field.javaName.fqnString -> field
    }(breakOut)

    val methods: Map[String, IndexedSeq[RawScalapMethod]] = sym.children.collect {
      case ms: MethodSymbol if ms.isMethod =>
        rawScalaMethod(ms, parentPrefix)
    }(collection.breakOut).groupBy(_.simpleName)

    val aliases: Map[String, RawType] = sym.children.collect {
      case as: AliasSymbol =>
        val alias = rawType(as, scalaName)
        alias.javaName.fqnString -> alias
    }(breakOut)

    RawScalapClass(
      javaName,
      scalaName,
      typeSignature,
      access,
      declaredAs,
      fields,
      methods,
      aliases
    )
  }

  private def className(sym: Symbol): ClassName = {
    val nested = sym.ownerChain
    val pkg = PackageName(sym.enclosingPackage.split("\\.").toList)
    val name = nested.map(_.name).mkString("$")
    val postfix = if (nested.last.isModule) "$" else ""

    ClassName(pkg, name + postfix)
  }

  private def rawScalaField(ms: MethodSymbol, parentPrefix: String): RawScalapField = {
    val aClass = className(ms.symbolInfo.owner)
    val name = ms.name.trim
    val javaName = FieldName(aClass, name)
    val scalaName = parentPrefix + name
    val access = getAccess(ms)

    val typeInfo = withScalaSigPrinter { printer =>
      printer.printType(ms.infoType)(printer.TypeFlags(true))
    }

    RawScalapField(javaName, scalaName, typeInfo, access)
  }

  private def rawScalaMethod(ms: MethodSymbol, parentPrefix: String): RawScalapMethod = {
    val scalaName = parentPrefix + ms.name
    val access = getAccess(ms)
    val signature = withScalaSigPrinter { printer =>
      printer.printMethodType(ms.infoType, printResult = true)(printer.TypeFlags(true))
    }

    RawScalapMethod(ms.name, scalaName, signature, access)
  }

}
