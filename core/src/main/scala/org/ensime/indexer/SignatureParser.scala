// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.indexer

import fastparse.all._

object SignatureParser extends ClassParser {

  protected val GenericClassSig: Parser[GenericClassName] =
    P(GenericClassSigWithArgs | GenericClassSigWithoutArgs)

  protected val GenericClassSigWithArgs: Parser[GenericClassName] =
    P(ClassNameSig ~ "<" ~ GenericArgs ~ ">" ~ InnerClassSig.rep ~ ";")
      .map((GenericClassName.apply _).tupled)

  protected val GenericClassSigWithoutArgs: Parser[GenericClassName] =
    P(ClassNameSig ~ InnerClassSig.rep ~ ";").map {
      case (className, innerClass) => GenericClassName(className, Seq.empty, innerClass)
    }

  protected val InnerClassSig: Parser[InnerClassName] =
    P(InnerClassSigWithArgs | InnerClassSigWithoutArgs)

  protected val InnerClassSigWithArgs: Parser[InnerClassName] =
    P("." ~ Name ~ "<" ~ GenericArgs ~ ">")
      .map((InnerClassName.apply _).tupled)

  protected val InnerClassSigWithoutArgs: Parser[InnerClassName] =
    P("." ~ Name)
      .map(InnerClassName(_, Seq.empty))

  protected val PrimitiveClassSig: Parser[GenericClassName] =
    P(PrimitiveClass)
      .map(GenericClassName(_, Seq.empty))

  private val GenericName: Parser[String] =
    P(GenericNameCharPredicate.rep(1).!)

  private val Generic: Parser[GenericClass] =
    P((GenericWithParam | GenericSuper) ~ End)

  private val GenericWithParam: Parser[GenericClass] =
    P("<" ~ GenericSigParam.rep(1) ~ ">" ~ GenericClassSig.rep(1))
      .map((GenericClass.apply _).tupled)

  private val GenericSuper: Parser[GenericClass] =
    P(GenericClassSig.rep(1))
      .map(GenericClass(Seq.empty, _))

  // class SomeClass[T <: SomeTrait] will have two : in signature
  private val GenericSigParam: Parser[GenericParam] =
    P(GenericName ~ ":" ~ (":".? ~ FieldTypeSignature).rep(1))
      .map((GenericParam.apply _).tupled)

  private val GenericArgs: Parser[Seq[GenericArg]] =
    P((ExtendsObject | GenericArgWithSignature).rep(1))

  private val ExtendsObject: Parser[GenericArg] =
    P("*")
      .map(_ => ExtendsObjectGenericArg)

  private val GenericArgWithSignature: Parser[GenericArg] =
    P((LowerBoundary | UpperBoundary).? ~ FieldTypeSignature)
      .map((SpecifiedGenericArg.apply _).tupled)

  private val LowerBoundary: Parser[BoundType] =
    P("-")
      .map(_ => LowerBound)

  private val UpperBoundary: Parser[BoundType] =
    P("+")
      .map(_ => UpperBound)

  private val FieldTypeSignature: Parser[RealTypeSignature] =
    P(GenericClassSig | GenericArraySig | TypeVar)

  private val GenericArraySig: Parser[GenericArray] =
    P("[" ~ (PrimitiveClassSig | GenericClassSig | GenericArraySig | TypeVar))
      .map(GenericArray)

  private val TypeVar: Parser[GenericVar] =
    P("T" ~ GenericNameCharPredicate.rep(1).! ~ ";").map(GenericVar)

  def parseGeneric(desc: String): GenericClass = {
    Generic.parse(desc) match {
      case Parsed.Success(sig, _) => sig
      case f: Parsed.Failure =>
        throw new Exception(s"Failed to parse generic: ${f.msg}")
    }
  }

  protected val GenericNameCharPredicate = CharPred(c => ":;/ ".forall(_ != c))

  override val PackageNamePredicate = CharPred(c => "<;/ ".forall(_ != c))
  override val ClassNameCharPredicate = CharPred(c => "<;/ ".forall(_ != c))
}
