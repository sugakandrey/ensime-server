// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import scala.tools.nsc.interactive.Global

import org.ensime.indexer._
import org.slf4j.Logger

/**
 * Resolves scalac `scala.reflect.internal.Symbols.Symbol` to Java
 * bytecode FQNs (including descriptors for methods).
 *
 * Note this does not work for
 * `scala.tools.scalap.scalax.rules.scalasig.Symbol` which must be
 * handled separately (going from a `Symbols.Symbol` to a
 * `scalasig.Symbol` would involve invoking the `Pickler` phase -
 * which we can't do - to generate a byte array that could be
 * reparsed by scalap).
 *
 * See also `DocFinding.javaFqnString` (which should be rewritten to
 * use this).
 */
trait SymbolToFqn { self: Global with PresentationCompilerBackCompat =>
  def logger: Logger

  import ClassName._
  private val ScalaPackageName = PackageName(List("scala"))
  private val normaliseClass: ClassName => ClassName = Map(
    ClassName(PackageName(List("scala", "runtime")), "BoxedUnit") -> PrimitiveVoid,
    ClassName(ScalaPackageName, "<byname>") -> ClassName(ScalaPackageName, "Function0"),
    ClassName(ScalaPackageName, "Boolean") -> PrimitiveBoolean,
    ClassName(ScalaPackageName, "Byte") -> PrimitiveByte,
    ClassName(ScalaPackageName, "Char") -> PrimitiveChar,
    ClassName(ScalaPackageName, "Short") -> PrimitiveShort,
    ClassName(ScalaPackageName, "Int") -> PrimitiveInt,
    ClassName(ScalaPackageName, "Long") -> PrimitiveLong,
    ClassName(ScalaPackageName, "Float") -> PrimitiveFloat,
    ClassName(ScalaPackageName, "Double") -> PrimitiveDouble,
    ClassName(ScalaPackageName, "Void") -> PrimitiveVoid
  ).withDefault(identity)

  private def packageName(sym: Symbol): PackageName = {
    PackageName(sym.ownerChain.takeWhile(!_.isRootSymbol).reverse.map(_.encodedName))
  }

  private def className(sym: Symbol): ClassName = try {
    val (packages, nested) = sym.ownerChain.takeWhile(!_.isRootSymbol).reverse.span(_.hasPackageFlag)
    val pkg = PackageName(packages.map(_.encodedName))
    val name = nested.map(_.encodedName).mkString("$")
    val postfix = if (nested.last.isModuleOrModuleClass) "$" else ""

    ClassName(pkg, name + postfix)
  } catch {
    case n: NoSuchElementException =>
      logger.error(s"sym = $sym ${sym.getClass}")
      throw n
  }

  private def descriptorType(t: Type): DescriptorType = {
    val c = normaliseClass(className(t.dealias.erasure.typeSymbol))
    if (c.fqnString == "scala.Array") {
      ArrayDescriptor(descriptorType(t.typeArgs.head))
    } else c
  }

  private def methodName(sym: MethodSymbol): MethodName = {
    val owner = sym.ownerChain.dropWhile(_.isMethod).head
    val clazz = className(owner)
    val name = sym.encodedName

    val descriptor: Descriptor = {
      val params = sym.paramLists.flatten.map {
        p => descriptorType(p.tpe)
      }
      val ret = descriptorType(sym.returnType)
      Descriptor(params, ret)
    }

    MethodName(clazz, name, descriptor)
  }

  private def fieldName(sym: TermSymbol): FieldName = {
    val clazz = className(sym.owner)
    val name = sym.encodedName
    FieldName(clazz, name)
  }

  def toFqn(sym: Symbol): FullyQualifiedName = sym match {
    case p if sym.hasPackageFlag => packageName(sym)
    case ts: TypeSymbol => normaliseClass(className(ts))
    case ms: ModuleSymbol => className(ms)
    case ms: MethodSymbol => methodName(ms)
    case ts: TermSymbol => fieldName(ts)
  }

}
