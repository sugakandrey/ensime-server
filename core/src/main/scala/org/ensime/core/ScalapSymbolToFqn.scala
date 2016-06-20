package org.ensime.core

import java.io.{ ByteArrayOutputStream, PrintStream }
import java.nio.charset.StandardCharsets

import org.ensime.api.DeclaredAs
import org.ensime.indexer._

import scala.tools.scalap.scalax.rules.scalasig._

trait ScalapSymbolToFqn {
  import ScalaSigApi._

  private def withScalaSigPrinter(code: ScalaSigPrinter => Any): String = {
    val baos = new ByteArrayOutputStream()
    val ps = new PrintStream(baos)
    val printer = new ScalaSigPrinter(ps, true)
    code(printer)
    new String(baos.toByteArray, StandardCharsets.UTF_8)
  }

  private def getAccess(sym: Symbol): Access =
    if (sym.isPrivate) Private
    else if (sym.isProtected) Protected
    else Public

  def rawScalaClass(sym: ClassSymbol): RawScalaClass = {
    val javaName = className(sym)
    val aPackage = sym.enclosingPackage
    val scalaName = sym.ownerChain.sliding(2, 1).map {
      case x :: y :: Nil =>
        val sep = if (x.isModule) "." else "#"
        x.name + sep
      case last :: Nil => last.name
    }.mkString
    val access = getAccess(sym)

    val declaredAs =
      if (sym.isTrait) DeclaredAs.Trait
      else if (sym.isModule) DeclaredAs.Object
      else DeclaredAs.Class

    val typeSignature = withScalaSigPrinter { printer =>
      printer.printType(sym.infoType)(printer.TypeFlags(true))
    }

    val fields = sym.children.collect {
      case ms: MethodSymbol if !ms.isMethod && ms.isLocal =>
        rawScalaField(ms)
    }

    val methods = sym.children.collect {
      case ms: MethodSymbol if ms.isMethod =>
        rawScalaMethod(ms)
    }

    RawScalaClass(
      javaName,
      aPackage + scalaName,
      typeSignature,
      access,
      declaredAs,
      fields,
      methods
    )
  }

  private def className(sym: Symbol): ClassName = {
    val nested = sym.ownerChain
    val pkg = PackageName(sym.enclosingPackage.split("\\.").toList)
    val name = nested.map(_.name).mkString("$")
    val postfix = if (nested.last.isModule) "$" else ""

    ClassName(pkg, name + postfix)
  }

  private def rawScalaField(ms: MethodSymbol): RawScalaField = {
    val aClass = className(ms.symbolInfo.owner)
    val name = ms.name
    val javaName = FieldName(aClass, name)
    val scalaName = ms.path
    val access = getAccess(ms)

    val typeInfo = withScalaSigPrinter { printer =>
      printer.printType(ms.infoType)(printer.TypeFlags(true))
    }

    RawScalaField(javaName, scalaName, typeInfo, access)
  }

  private def rawScalaMethod(ms: MethodSymbol): RawScalaMethod = {
    val scalaName = ms.path
    val access = getAccess(ms)
    val signature = withScalaSigPrinter { printer =>
      printer.printMethodType(ms.infoType, printResult = true)(printer.TypeFlags(true))
    }

    RawScalaMethod(scalaName, signature, access)
  }

}
