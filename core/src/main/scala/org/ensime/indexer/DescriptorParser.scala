// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.indexer

import org.ensime.indexer.ClassName._
import fastparse.all._

trait ClassParser {
  protected val Class: Parser[ClassName] = P(ClassNameSig ~ ";")

  protected val ClassNameSig: Parser[ClassName] = P("L" ~ Package ~ Name).map((ClassName.apply _).tupled)

  protected val Name: Parser[String] = P(ClassNameCharPredicate.rep(1).!)

  protected val Package: Parser[PackageName] =
    P((PackageNamePredicate.rep(1).! ~ "/").rep).map(seq => PackageName(seq.toList))

  protected val PrimitiveClass: Parser[ClassName] =
    P(Boolean | Byte | Char | Short | Int | Long | Float | Double | Void)

  protected val PackageNamePredicate: P0
  protected val ClassNameCharPredicate: P0

  protected val Boolean: Parser[ClassName] = P("Z").map(_ => PrimitiveBoolean)
  protected val Byte: Parser[ClassName] = P("B").map(_ => PrimitiveByte)
  protected val Char: Parser[ClassName] = P("C").map(_ => PrimitiveChar)
  protected val Short: Parser[ClassName] = P("S").map(_ => PrimitiveShort)
  protected val Int: Parser[ClassName] = P("I").map(_ => PrimitiveInt)
  protected val Long: Parser[ClassName] = P("J").map(_ => PrimitiveLong)
  protected val Float: Parser[ClassName] = P("F").map(_ => PrimitiveFloat)
  protected val Double: Parser[ClassName] = P("D").map(_ => PrimitiveDouble)
  protected val Void: Parser[ClassName] = P("V").map(_ => PrimitiveVoid)
}

object DescriptorParser extends ClassParser {

  private val Desc: Parser[Descriptor] =
    P("(" ~ Type.rep ~ ")" ~ Type ~ End).map {
      case (paramSeq: Seq[DescriptorType], retType: DescriptorType) => Descriptor(paramSeq.toList, retType)
    }

  private val Type: Parser[DescriptorType] =
    P(Class | Boolean | Byte | Char | Short | Int | Long | Float | Double | Void | Array)

  private val Array: Parser[DescriptorType] = P("[" ~ Type).map(ArrayDescriptor)

  override val PackageNamePredicate = CharPred(c => ";/ ".forall(_ != c))
  override val ClassNameCharPredicate = CharPred(c => ";/ ".forall(_ != c))

  def parse(desc: String): Descriptor = {
    Desc.parse(desc) match {
      case Parsed.Success(d, _) => d
      case f: Parsed.Failure =>
        val msg = f.msg
        throw new Exception(s"Failed to parse descriptor: $msg")
    }
  }

  def parseType(desc: String): DescriptorType = {
    Type.parse(desc) match {
      case Parsed.Success(d, _) => d
      case f: Parsed.Failure =>
        val msg = f.msg
        throw new Exception(s"Failed to parse descriptor: $msg")
    }
  }
}
