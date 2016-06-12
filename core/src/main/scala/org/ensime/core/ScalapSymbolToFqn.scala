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

  private def descriptorType(t: TypeRefType): DescriptorType = {
    val c = normaliseClass(t.toClassName)
    if (c.fqnString == "scala.Array") {
      ArrayDescriptor(descriptorType(t.typeArgs.head.asInstanceOf[TypeRefType]))
    } else c
  }

  protected def methodName(ms: MethodSymbol): MethodName = {
    val owner = ms.ownerChain.dropWhile(_.isMethod).head
    val aClass = className(owner)
    val name = ms.name

    val descriptor = {
      val typeInfo = ms.infoType match {
        case PolyType(typeRef, _) => typeRef
        case mt @ MethodType(_, _) => mt
        case nmt @ NullaryMethodType(_) => nmt
        case whatever => log.debug(s"Got $whatever as method.infoType"); ???
      }

      val params = typeInfo match {
        case MethodType(_, paramSymbols) =>
          paramSymbols.collect { case s: MethodSymbol => descriptorType(s.infoType.asInstanceOf[TypeRefType]) }.toList
        case NullaryMethodType(_) => Nil
      }
      val ret = typeInfo match {
        case NullaryMethodType(resType @ TypeRefType(_, _, _)) => descriptorType(resType)
        case MethodType(refined @ RefinedType(_, _), _) => throw new UnsupportedOperationException("TODO: Refined return type")
        case MethodType(resType @ TypeRefType(_, _, _), _) => descriptorType(resType)
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
