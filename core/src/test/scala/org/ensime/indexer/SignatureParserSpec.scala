// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.indexer

import org.ensime.indexer.SignatureParser.parseGeneric
import org.ensime.util.EnsimeSpec

class SignatureParserSpec extends EnsimeSpec {

  private val JavaLangPackage = PackageName(List("java", "lang"))
  private val JavaUtilPackage = PackageName(List("java", "util"))
  private val EnsimePackage = PackageName(List("org", "ensime"))
  private val ScalaCollectionPackage = PackageName(List("scala", "collection"))
  private val ObjectSignature = ClassName(JavaLangPackage, "Object")
  private val StringSignature = ClassName(JavaLangPackage, "String")

  "SignatureParser" should "fail to parse the empty string" in {
    intercept[Exception](parseGeneric(""))
  }

  it should "fail to parse a bad string" in {
    intercept[Exception](parseGeneric("not valid"))
  }

  it should "parse a simple class without generics" in {
    parseGeneric("Ljava/lang/Object;") should ===(
      GenericClass(
        Seq.empty,
        Vector(GenericClassName(ObjectSignature))
      )
    )

    parseGeneric("Ljava/lang/Object;Lorg/ensime/SomeClass;") should ===(
      GenericClass(
        Seq.empty,
        Vector(
          GenericClassName(ObjectSignature),
          GenericClassName(
            ClassName(EnsimePackage, "SomeClass"), Seq.empty
          )
        )
      )
    )
  }

  it should "parse generic class with generic param" in {
    parseGeneric("<X:Ljava/lang/Object;>Ljava/lang/Object;") should ===(
      GenericClass(
        Vector(
          GenericParam("X", Vector(GenericClassName(ObjectSignature)))
        ), Vector(
          GenericClassName(ObjectSignature)
        )
      )
    )

    parseGeneric("<T:Ljava/lang/Object;U:Ljava/lang/Object;>Ljava/lang/Object;") should ===(
      GenericClass(
        Vector(
          GenericParam("T", Vector(GenericClassName(ObjectSignature))),
          GenericParam("U", Vector(GenericClassName(ObjectSignature)))
        ), Vector(
          GenericClassName(ObjectSignature)
        )
      )
    )

    parseGeneric("<U:Ljava/lang/Object;V:Lorg/ensime/DummyParent<TU;>;>Lorg/ensime/DummyParent<TV;>;") should ===(
      GenericClass(
        Vector(
          GenericParam("U", Vector(GenericClassName(ObjectSignature))),
          GenericParam("V", Vector(GenericClassName(
            ClassName(EnsimePackage, "DummyParent"),
            Vector(SpecifiedGenericArg(None, GenericVar("U")))
          )))
        ), Vector(
          GenericClassName(
            ClassName(EnsimePackage, "DummyParent"),
            Vector(SpecifiedGenericArg(None, GenericVar("V")))
          )
        )
      )
    )
  }

  it should "handle generic params with many classes" in {
    parseGeneric("<T:Lorg/ensime/ClassOne;:Lorg/ensime/TraitTwo;>Ljava/lang/Object;") should ===(
      GenericClass(
        Vector(
          GenericParam("T", Vector(
            GenericClassName(ClassName(EnsimePackage, "ClassOne")),
            GenericClassName(ClassName(EnsimePackage, "TraitTwo"))
          ))
        ), Vector(GenericClassName(ObjectSignature))
      )
    )

    parseGeneric("<T::Lorg/ensime/TraitOne;:Lorg/ensime/TraitTwo;>Ljava/lang/Object;") should ===(
      GenericClass(
        Vector(
          GenericParam("T", Vector(
            GenericClassName(ClassName(EnsimePackage, "TraitOne")),
            GenericClassName(ClassName(EnsimePackage, "TraitTwo"))
          ))
        ), Vector(GenericClassName(ObjectSignature))
      )
    )

  }

  it should "parse class with generic in super class" in {
    parseGeneric("Ljava/lang/Comparable<Ljava/lang/String;>;") should ===(
      GenericClass(
        Seq.empty,
        Vector(GenericClassName(
          ClassName(JavaLangPackage, "Comparable"),
          Vector(SpecifiedGenericArg(None, GenericClassName(StringSignature)))
        ))
      )
    )

    parseGeneric("Ljava/lang/Object;Ljava/io/Serializable;" +
      "Ljava/lang/Comparable<Ljava/lang/String;>;Ljava/lang/CharSequence;") should ===(
      GenericClass(
        Seq.empty,
        Vector(
          GenericClassName(ObjectSignature),
          GenericClassName(
            ClassName(PackageName(List("java", "io")), "Serializable")
          ),
          GenericClassName(
            ClassName(JavaLangPackage, "Comparable"),
            Vector(
              SpecifiedGenericArg(
                None,
                GenericClassName(StringSignature)
              )
            )
          ),
          GenericClassName(
            ClassName(JavaLangPackage, "CharSequence")
          )
        )
      )
    )
  }

  it should "parse class with nonstandard generic in super class" in {

    parseGeneric("Lorg/ensime/Dummy<Ljava/util/List<*>;>;") should ===(
      GenericClass(
        Seq.empty,
        Vector(
          GenericClassName(
            ClassName(EnsimePackage, "Dummy"),
            Vector(SpecifiedGenericArg(
              None,
              GenericClassName(
                ClassName(JavaUtilPackage, "List"),
                Vector(ExtendsObjectGenericArg)
              )
            ))
          )
        )
      )
    )

    parseGeneric("Lorg/ensime/Dummy<Ljava/util/List<+Ljava/lang/Number;>;>;") should ===(
      GenericClass(
        Seq.empty,
        Vector(
          GenericClassName(
            ClassName(EnsimePackage, "Dummy"),
            Vector(
              SpecifiedGenericArg(None, GenericClassName(
                ClassName(JavaUtilPackage, "List"),
                Vector(SpecifiedGenericArg(
                  Some(UpperBound), GenericClassName(ClassName(JavaLangPackage, "Number"))
                ))
              ))
            )
          )
        )
      )
    )

    parseGeneric("Lorg/ensime/Dummy<Ljava/util/List<-Ljava/lang/Number;>;>;") should ===(
      GenericClass(
        Seq.empty,
        Vector(
          GenericClassName(
            ClassName(PackageName(List("org", "ensime")), "Dummy"),
            Vector(SpecifiedGenericArg(None, GenericClassName(
              ClassName(JavaUtilPackage, "List"),
              Vector(SpecifiedGenericArg(
                Some(LowerBound), GenericClassName(
                  ClassName(JavaLangPackage, "Number")
                )
              ))
            )))
          )
        )
      )
    )
  }

  it should "parse class with param and generic in super class" in {
    parseGeneric("<A:Ljava/lang/Object;>Ljava/lang/Comparable<Ljava/lang/String;>;") should ===(
      GenericClass(
        Vector(GenericParam(
          "A",
          Vector(GenericClassName(ObjectSignature))
        )), Vector(GenericClassName(
          ClassName(JavaLangPackage, "Comparable"),
          Vector(
            SpecifiedGenericArg(
              None,
              GenericClassName(StringSignature)
            )
          )
        ))
      )
    )
  }

  it should "parse class with generic variable" in {
    parseGeneric("<A:Ljava/lang/Object;>Lscala/collection/AbstractSeq<TA;>;") should ===(
      GenericClass(
        Vector(
          GenericParam(
            "A", Vector(GenericClassName(ObjectSignature))
          )
        ), Vector(
          GenericClassName(
            ClassName(ScalaCollectionPackage, "AbstractSeq"),
            Vector(
              SpecifiedGenericArg(None, GenericVar("A"))
            )
          )
        )
      )
    )
  }

  it should "parse class with generic variable in generic param" in {
    parseGeneric("<TypeName:Ljava/lang/Object;>Lscala/collection/LinearSeqOptimized<TTypeName;" +
      "Lscala/collection/immutable/List<TTypeName;>;>;") should ===(
      GenericClass(
        Vector(
          GenericParam("TypeName", Vector(GenericClassName(ObjectSignature)))
        ), Vector(
          GenericClassName(
            ClassName(ScalaCollectionPackage, "LinearSeqOptimized"),
            Vector(
              SpecifiedGenericArg(None, GenericVar("TypeName")),
              SpecifiedGenericArg(None, GenericClassName(
                ClassName(PackageName(List("scala", "collection", "immutable")), "List"),
                Vector(SpecifiedGenericArg(None, GenericVar("TypeName")))
              ))
            )
          )
        )
      )
    )
  }

  it should "handle traits correctly" in {
    parseGeneric("<T::Lorg/ensime/SomeTrait;>Ljava/lang/Object;") should ===(
      GenericClass(
        Vector(
          GenericParam("T", Vector(GenericClassName(
            ClassName(EnsimePackage, "SomeTrait")
          )))
        ), Vector(
          GenericClassName(ObjectSignature)
        )
      )
    )

    parseGeneric("<T::Lorg/ensime/SomeTrait;U::Lorg/ensime/OtherTrait;>Ljava/lang/Object;") should ===(
      GenericClass(
        Vector(
          GenericParam("T", Vector(GenericClassName(
            ClassName(EnsimePackage, "SomeTrait")
          ))),
          GenericParam("U", Vector(GenericClassName(
            ClassName(EnsimePackage, "OtherTrait")
          )))
        ), Vector(
          GenericClassName(ObjectSignature)
        )
      )
    )
  }

  it should "handle inner classes correctly" in {
    parseGeneric("Lorg/ensime/Outer$Inner<Ljava/lang/Integer;>;") should ===(
      GenericClass(
        Seq.empty,
        Vector(
          GenericClassName(
            ClassName(EnsimePackage, "Outer$Inner"),
            Vector(
              SpecifiedGenericArg(
                None,
                GenericClassName(ClassName(JavaLangPackage, "Integer"))
              )
            )
          )
        )
      )
    )

    parseGeneric("Lorg/scalatest/SuperEngine<TT;>.Node;") should ===(
      GenericClass(
        Seq.empty,
        Vector(
          GenericClassName(
            ClassName(PackageName(List("org", "scalatest")), "SuperEngine"),
            Vector(
              SpecifiedGenericArg(
                None,
                GenericVar("T")
              )
            ),
            Vector(InnerClassName("Node"))
          )
        )
      )
    )

    parseGeneric("Lorg/ensime/Outer<TT;>.Inner<Ljava/lang/Object;>;") should ===(
      GenericClass(
        Seq.empty,
        Vector(
          GenericClassName(
            ClassName(EnsimePackage, "Outer"),
            Vector(
              SpecifiedGenericArg(
                None,
                GenericVar("T")
              )
            ),
            Vector(InnerClassName("Inner", Vector(
              SpecifiedGenericArg(
                None,
                GenericClassName(ObjectSignature)
              )
            )))
          )
        )
      )
    )

    parseGeneric("Lorg/ensime/Outer<TT;>.Inner<Ljava/lang/Object;>.InnerInner<Ljava/lang/Object;>;") should ===(
      GenericClass(
        Seq.empty,
        Vector(
          GenericClassName(
            ClassName(EnsimePackage, "Outer"),
            Vector(
              SpecifiedGenericArg(
                None,
                GenericVar("T")
              )
            ),
            Vector(
              InnerClassName("Inner", Vector(
                SpecifiedGenericArg(
                  None,
                  GenericClassName(ObjectSignature)
                )
              )),
              InnerClassName("InnerInner", Vector(
                SpecifiedGenericArg(
                  None,
                  GenericClassName(ObjectSignature)
                )
              ))
            )
          )
        )
      )
    )
  }

  it should "parse class with array in generic super class" in {
    parseGeneric("Lorg/ensime/Dummy<[Ljava/lang/Integer;>;") should ===(
      GenericClass(
        Seq.empty,
        Vector(
          GenericClassName(
            ClassName(EnsimePackage, "Dummy"),
            Vector(
              SpecifiedGenericArg(
                None,
                GenericArray(GenericClassName(
                  ClassName(JavaLangPackage, "Integer")
                ))
              )
            )
          )
        )
      )
    )

    parseGeneric("Lorg/ensime/Dummy<[Ljava/util/List<-[D>;>;") should ===(
      GenericClass(
        Seq.empty,
        Vector(
          GenericClassName(
            ClassName(EnsimePackage, "Dummy"),
            Vector(
              SpecifiedGenericArg(
                None,
                GenericArray(GenericClassName(
                  ClassName(JavaUtilPackage, "List"),
                  Vector(SpecifiedGenericArg(
                    Some(LowerBound),
                    GenericArray(
                      GenericClassName(ClassName.PrimitiveDouble)
                    )
                  ))
                ))
              )
            )
          )
        )
      )
    )

    parseGeneric("Lorg/ensime/Dummy<[Ljava/util/List<-[[[D>;>;") should ===(
      GenericClass(
        Seq.empty,
        Vector(
          GenericClassName(
            ClassName(EnsimePackage, "Dummy"),
            Vector(
              SpecifiedGenericArg(
                None,
                GenericArray(GenericClassName(
                  ClassName(JavaUtilPackage, "List"),
                  Vector(SpecifiedGenericArg(
                    Some(LowerBound),
                    GenericArray(GenericArray(GenericArray(
                      GenericClassName(ClassName.PrimitiveDouble)
                    )))
                  ))
                ))
              )
            )
          )
        )
      )
    )

    parseGeneric("<T:Ljava/lang/Object;>Lscala/collection/mutable/ArrayLike<TT;[TT;>;") should ===(
      GenericClass(
        Vector(
          GenericParam(
            "T",
            Vector(GenericClassName(ObjectSignature))
          )
        ), Vector(
          GenericClassName(
            ClassName(PackageName(List("scala", "collection", "mutable")), "ArrayLike"),
            Vector(
              SpecifiedGenericArg(
                None,
                GenericVar("T")
              ),
              SpecifiedGenericArg(
                None,
                GenericArray(GenericVar("T"))
              )
            )
          )
        )
      )
    )
  }
}
