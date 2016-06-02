// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import org.ensime.api._
import org.ensime.fixture._
import org.ensime.util.EnsimeSpec
import DeclaredAs.{ Nil => _, _ }

class TypeToScalaNameSpec extends EnsimeSpec
    with IsolatedRichPresentationCompilerFixture
    with RichPresentationCompilerTestUtils
    with ReallyRichPresentationCompilerFixture {
  import ReallyRichPresentationCompilerFixture._

  val original = EnsimeConfigFixture.ShapelessTestProject

  it should "calculate the TypeInfo at point" in withPresCompiler { (config, cc) =>
    runForPositionInCompiledSource(
      config, cc,
      "package com.example",
      "import shapeless._, labelled._, syntax.singleton._",
      "class Thing {",
      "    val in@int@t: Int = 13",
      "    def met@method1@hod1(i: Int): String = i.toString",
      "    val ar@arrow1@row1: Int => String = (i: Int) => met@call1@hod1(i)",
      "    def met@method2@hod2(i: Int, j: Long): String = i.toString",
      "    val arrow2: Int => Long => String = (i: Int, j: Long) => met@call2@hod2(i, j)",
      "    val arrow0: () => Int = null ; ar@call0@row0()",
      "    def tu@tuple2@ple2: (String, Int) = null",
      "    def hl@hlist@ist: Int :: String :: HNil = null",
      "    def re@refined@fined = 1.narrow",
      "    def ex@exciting@citing = 'f' ->> 23.narrow",
      "}"
    ) { (p, label, cc) =>
        withClue(label) {
          cc.askTypeInfoAt(p).getOrElse { fail } shouldBe {
            label match {
              case "int" =>
                BasicTypeInfo("Int", Class, "scala.Int", Nil, Nil, None)
              case "method1" | "method2" =>
                // the return type
                BasicTypeInfo("String", Class, "java.lang.String", Nil, Nil, None)
              case "call1" =>
                ArrowTypeInfo(
                  // we used to skip the surrounding brackets, but
                  // it's confusing when chaining multiple functions
                  "(Int) => String",
                  "(scala.Int) => java.lang.String",
                  BasicTypeInfo(
                    "String",
                    Class,
                    "java.lang.String",
                    Nil, Nil, None
                  ),
                  List(ParamSectionInfo(List(("i", BasicTypeInfo("Int", Class, "scala.Int", Nil, Nil, None))), false))
                )
              case "arrow1" =>
                ArrowTypeInfo(
                  "(Int) => String",
                  "(scala.Int) => java.lang.String",
                  BasicTypeInfo("String", Class, "java.lang.String", Nil, Nil, None),
                  List(ParamSectionInfo(
                    List(("_0", BasicTypeInfo("Int", Class, "scala.Int", Nil, Nil, None))), false
                  ))
                )

              case "call0" =>
                ArrowTypeInfo(
                  "() => Int",
                  "() => scala.Int",
                  BasicTypeInfo("Int", Class, "scala.Int", Nil, Nil, None),
                  List(ParamSectionInfo(Nil, false))
                )

              case "call2" =>
                ArrowTypeInfo(
                  "(Int, Long) => String",
                  "(scala.Int, scala.Long) => java.lang.String",
                  BasicTypeInfo("String", Class, "java.lang.String", Nil, Nil, None),
                  List(ParamSectionInfo(List(
                    ("i", BasicTypeInfo("Int", Class, "scala.Int", Nil, Nil, None)),
                    ("j", BasicTypeInfo("Long", Class, "scala.Long", Nil, Nil, None))
                  ), false))
                )
              case "tuple2" =>
                BasicTypeInfo(
                  "(String, Int)",
                  Class,
                  "(java.lang.String, scala.Int)",
                  List(
                    BasicTypeInfo("String", Class, "java.lang.String", Nil, Nil, None),
                    BasicTypeInfo("Int", Class, "scala.Int", Nil, Nil, None)
                  ),
                  Nil, None
                )

              case "hlist" =>
                BasicTypeInfo(
                  "Int :: String :: HNil",
                  Class,
                  "scala.Int shapeless.:: java.lang.String shapeless.:: shapeless.HNil",
                  List(
                    BasicTypeInfo("Int", Class, "scala.Int", Nil, Nil, None),
                    BasicTypeInfo("String :: HNil", Class, "java.lang.String shapeless.:: shapeless.HNil",
                      List(
                        BasicTypeInfo("String", Class, "java.lang.String", Nil, Nil, None),
                        BasicTypeInfo("HNil", Trait, "shapeless.HNil", Nil, Nil, None)
                      ), Nil, None)
                  ), Nil, None
                )

              case "refined" =>
                BasicTypeInfo(
                  "Int(1)",
                  Class,
                  "scala.Int(1)",
                  Nil, Nil, None
                )

              case "exciting" =>
                // potential canary, we might want to prettify KeyTag
                BasicTypeInfo(
                  "Int(23) with KeyTag[Char('f'), Int(23)]",
                  Class,
                  "scala.Int(23) with shapeless.labelled.KeyTag[scala.Char('f'), scala.Int(23)]",
                  Nil, Nil, None
                )
            }
          }
        }
      }
  }

}
