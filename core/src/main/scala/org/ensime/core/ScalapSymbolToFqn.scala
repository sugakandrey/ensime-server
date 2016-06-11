package org.ensime.core

import org.ensime.indexer._

import scala.tools.scalap.scalax.rules.scalasig._

trait ScalapSymbolToFqn {
  import ScalaSigApi._
  import FqnUtils._

  protected def className(sym: Symbol): ClassName = {
    val nested = sym.ownerChain
    val pkg = PackageName(sym.enclosingPackage.split("\\.").toList)
    val name = nested.map(_.name).mkString("$")
    val postfix = if (nested.last.isModule) "$" else ""

    ClassName(pkg, name + postfix)
  }

  protected def fieldName(ms: MethodSymbol): FieldName = {
    val aClass = className(ms.symbolInfo.owner)
    val name = ms.name
    FieldName(aClass, name)
  }

  private def descriptorType(t: Type): DescriptorType = {
    val c = normaliseClass(className(t.dealias.erasure.typeSymbol))
    if (c.fqnString == "scala.Array") {
      ArrayDescriptor(descriptorType(t.asInstanceOf[TypeRefType].typeArgs.head))
    } else c
  }

  protected def methodName(ms: MethodSymbol): MethodName = {
    val owner = ms.ownerChain.dropWhile(_.isMethod).head
    val aClass = className(owner)
    val name = ms.name

    val descriptor = {
      val typeInfo = ms.infoType
      val params = typeInfo match {
        case MethodType(_, paramSymbols) =>
          paramSymbols.collect { case s: MethodSymbol => descriptorType(s.infoType) }.toList
        case NullaryMethodType(_) => Nil
      }
      val ret = typeInfo match {
        case NullaryMethodType(resultType) => descriptorType(resultType)
        case MethodType(resultType, _) => descriptorType(resultType)
      }
      Descriptor(params, ret)
    }

    MethodName(aClass, name, descriptor)
  }

  def toFqn(sym: Symbol): FullyQualifiedName = sym match {
    case cs: ClassSymbol => className(sym)
    case os: ObjectSymbol => className(os)
    case ms: MethodSymbol if ms.isMethod => methodName(ms)
    case fs: MethodSymbol => fieldName(fs)
  }

}
