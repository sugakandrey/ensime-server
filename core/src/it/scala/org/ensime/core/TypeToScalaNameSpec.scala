// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import org.ensime.api, api.{ BasicTypeInfo => _, _ }
import org.ensime.fixture._
import org.ensime.model.BasicTypeInfo
import org.ensime.util.EnsimeSpec
import DeclaredAs.{ Nil => _, _ }
import org.scalactic.Equality

class TypeToScalaNameSpec extends EnsimeSpec
    with IsolatedRichPresentationCompilerFixture
    with RichPresentationCompilerTestUtils
    with ReallyRichPresentationCompilerFixture {
  import ReallyRichPresentationCompilerFixture._

  val original = EnsimeConfigFixture.ShapelessTestProject

  // ignores the source information when performing equality
  implicit object TypeInfoSimplifiedEquality extends Equality[TypeInfo] {
    def areEqual(a: TypeInfo, b: Any): Boolean = b match {
      case api.BasicTypeInfo(name, declAs, fullName, _, _, _, _) =>
        a.name === name &&
          a.declAs === declAs &&
          a.fullName === fullName
      case api.ArrowTypeInfo(name, fullName, resultType, paramSections, _) =>
        a.name === name &&
          a.fullName === fullName && {
            a match {
              case api.ArrowTypeInfo(_, _, aRes, aParam, _) => aRes === resultType && {
                // saves creating an Equality[ParamSectionInfo]
                aParam.map(_.isImplicit) === paramSections.map(_.isImplicit) &&
                  aParam.map(_.params.map(_._1)) === paramSections.map(_.params.map(_._1)) &&
                  aParam.map(_.params.map(_._2)) === paramSections.map(_.params.map(_._2))
              }
              case _ => false
            }
          }
    }
  }

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
      "    def rep@repeated@eated(i: Int, s: String, l: Long*): Long = l.head",
      "    val lon@long@g: Long = rep@callrepeated@eated(123, \"hello\", 1L, 2L, 3L)",
      "    val arrow0: () => Int = null ; ar@call0@row0()",
      "    def tu@tuple2@ple2: (String, Int) = null",
      "    def hl@hlist@ist: Int :: String :: HNil = null",
      "    def re@refined@fined = 1.narrow",
      "    def ex@exciting@citing = 'f' ->> 23.narrow",
      "    def by@byname@name(i: Int, s: => (Int, String)): String = s._2",
      "    val s: String = by@bynamecall@name(15, (16, \"hello\"))",
      "}"
    ) { (p, label, cc) =>
        withClue(label) {
          cc.askTypeInfoAt(p).getOrElse { fail } shouldEqual {
            label match {
              case "int" =>
                BasicTypeInfo("Int", Class, "scala.Int")
              case "long" =>
                BasicTypeInfo("Long", Class, "scala.Long")
              case "method1" | "method2" =>
                // the return type
                BasicTypeInfo("String", Class, "java.lang.String")
              case "call1" =>
                ArrowTypeInfo(
                  // we used to skip the surrounding brackets, but
                  // it's confusing when chaining multiple functions
                  "(Int) => String",
                  "(scala.Int) => java.lang.String",
                  BasicTypeInfo(
                    "String",
                    Class,
                    "java.lang.String"
                  ),
                  List(ParamSectionInfo(List(("i", BasicTypeInfo("Int", Class, "scala.Int"))), false)),
                  Nil
                )
              case "arrow1" =>
                ArrowTypeInfo(
                  "(Int) => String",
                  "(scala.Int) => java.lang.String",
                  BasicTypeInfo("String", Class, "java.lang.String"),
                  List(ParamSectionInfo(
                    List(("_0", BasicTypeInfo("Int", Class, "scala.Int"))), false
                  )),
                  Nil
                )
              case "repeated" =>
                BasicTypeInfo("Long", Class, "scala.Long")
              case "callrepeated" =>
                ArrowTypeInfo(
                  "(Int, String, Long*) => Long",
                  "(scala.Int, java.lang.String, scala.Long*) => scala.Long",
                  BasicTypeInfo(
                    "Long",
                    Class,
                    "scala.Long"
                  ),
                  List(ParamSectionInfo(List(
                    ("i", BasicTypeInfo("Int", Class, "scala.Int")),
                    ("s", BasicTypeInfo("String", Class, "java.lang.String")),
                    ("l", api.BasicTypeInfo("Long*", Class, "scala.Long*",
                      List(BasicTypeInfo("Long", Class, "scala.Long")), Nil, None, Nil))
                  ), false)),
                  Nil
                )
              case "call0" =>
                ArrowTypeInfo(
                  "() => Int",
                  "() => scala.Int",
                  BasicTypeInfo("Int", Class, "scala.Int"),
                  List(ParamSectionInfo(Nil, false)),
                  Nil
                )

              case "call2" =>
                ArrowTypeInfo(
                  "(Int, Long) => String",
                  "(scala.Int, scala.Long) => java.lang.String",
                  BasicTypeInfo("String", Class, "java.lang.String"),
                  List(ParamSectionInfo(List(
                    ("i", BasicTypeInfo("Int", Class, "scala.Int")),
                    ("j", BasicTypeInfo("Long", Class, "scala.Long"))
                  ), false)),
                  Nil
                )
              case "tuple2" =>
                api.BasicTypeInfo(
                  "(String, Int)",
                  Class,
                  "(java.lang.String, scala.Int)",
                  List(
                    BasicTypeInfo("String", Class, "java.lang.String"),
                    BasicTypeInfo("Int", Class, "scala.Int")
                  ),
                  Nil, None, Nil
                )

              case "hlist" =>
                api.BasicTypeInfo(
                  "Int :: String :: HNil",
                  Class,
                  "scala.Int shapeless.:: java.lang.String shapeless.:: shapeless.HNil",
                  List(
                    BasicTypeInfo("Int", Class, "scala.Int"),
                    api.BasicTypeInfo("String :: HNil", Class, "java.lang.String shapeless.:: shapeless.HNil",
                      List(
                        BasicTypeInfo("String", Class, "java.lang.String"),
                        BasicTypeInfo("HNil", Trait, "shapeless.HNil")
                      ), Nil, None, Nil)
                  ), Nil, None, Nil
                )

              case "refined" =>
                BasicTypeInfo(
                  "Int(1)",
                  Class,
                  "scala.Int(1)"
                )

              case "exciting" =>
                // potential canary, we might want to prettify KeyTag
                BasicTypeInfo(
                  "Int(23) with KeyTag[Char('f'), Int(23)]",
                  Class,
                  "scala.Int(23) with shapeless.labelled.KeyTag[scala.Char('f'), scala.Int(23)]"
                )

              case "byname" =>
                BasicTypeInfo(
                  "String",
                  Class,
                  "java.lang.String"
                )

              case "bynamecall" =>
                ArrowTypeInfo(
                  "(Int, => (Int, String)) => String",
                  "(scala.Int, => (scala.Int, java.lang.String)) => java.lang.String",
                  BasicTypeInfo("String", Class, "java.lang.String"),
                  List(ParamSectionInfo(List(
                    ("i", BasicTypeInfo("Int", Class, "scala.Int")),
                    ("s", ArrowTypeInfo("=> (Int, String)", "=> (scala.Int, java.lang.String)",
                      api.BasicTypeInfo("(Int, String)", Class, "(scala.Int, java.lang.String)",
                        List(
                          BasicTypeInfo("Int", Class, "scala.Int"),
                          BasicTypeInfo("String", Class, "java.lang.String")
                        ), Nil, None, Nil), Nil, Nil))
                  ), false)), Nil
                )
            }
          }
        }
      }
  }

}
