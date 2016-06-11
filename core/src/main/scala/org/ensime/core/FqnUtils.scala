package org.ensime.core

import org.ensime.indexer.{ ClassName, PackageName }

object FqnUtils {
  import ClassName._
  val ScalaPackageName: PackageName = PackageName(List("scala"))
  val normaliseClass: ClassName => ClassName = Map(
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
}
