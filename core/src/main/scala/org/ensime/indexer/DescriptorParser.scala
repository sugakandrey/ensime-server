// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.indexer

import org.ensime.indexer.ClassName._
import org.parboiled2.Parser
import org.parboiled2.Rule1
import org.parboiled2._

import scala.annotation.switch
import scala.util.{ Failure, Success }

trait ClassParser extends Parser {

  protected def Class: Rule1[ClassName] = rule {
    ClassNameSig ~ ';'
  }

  protected def ClassNameSig: Rule1[ClassName] = rule {
    'L' ~ Package ~ Name ~> ClassName.apply _
  }

  protected def Name: Rule1[String] = rule {
    capture(oneOrMore(ClassNameCharPredicate))
  }

  protected def Package: Rule1[PackageName] = rule {
    zeroOrMore(capture(oneOrMore(PackageNamePredicate)) ~ '/') ~> { seq: Seq[String] => PackageName(seq.toList) }
  }

  protected def PrimitiveClass: Rule1[ClassName] = rule {
    Boolean | Byte | Char | Short | Int | Long | Float | Double | Void
  }

  protected def PackageNamePredicate: CharPredicate
  protected def ClassNameCharPredicate: CharPredicate

  protected def Boolean: Rule1[ClassName] = rule { 'Z' ~ push(PrimitiveBoolean) }
  protected def Byte: Rule1[ClassName] = rule { 'B' ~ push(PrimitiveByte) }
  protected def Char: Rule1[ClassName] = rule { 'C' ~ push(PrimitiveChar) }
  protected def Short: Rule1[ClassName] = rule { 'S' ~ push(PrimitiveShort) }
  protected def Int: Rule1[ClassName] = rule { 'I' ~ push(PrimitiveInt) }
  protected def Long: Rule1[ClassName] = rule { 'J' ~ push(PrimitiveLong) }
  protected def Float: Rule1[ClassName] = rule { 'F' ~ push(PrimitiveFloat) }
  protected def Double: Rule1[ClassName] = rule { 'D' ~ push(PrimitiveDouble) }
  protected def Void: Rule1[ClassName] = rule { 'V' ~ push(PrimitiveVoid) }
}

object DescriptorParser {
  def parse(desc: String): Descriptor = {
    val parser = new DescriptorParser(desc)
    parser.Desc.run() match {
      case Success(d) => d
      case Failure(error: ParseError) =>
        val msg = parser.formatError(error, new ErrorFormatter(showTraces = true))
        throw new Exception(s"Failed to parse descriptor: $msg")
      case Failure(other) =>
        throw new Exception("Failed to parse descriptor: ", other)
    }
  }

  def parseType(desc: String): DescriptorType = {
    val parser = new DescriptorParser(desc)
    parser.Type.run() match {
      case Success(d) => d
      case Failure(error: ParseError) =>
        val msg = parser.formatError(error, new ErrorFormatter(showTraces = true))
        throw new Exception(s"Failed to parse descriptor type: $msg")
      case Failure(other) =>
        throw new Exception("Failed to parse descriptor type: ", other)
    }
  }
}

class DescriptorParser(val input: ParserInput) extends ClassParser {

  def Desc: Rule1[Descriptor] = rule {
    '(' ~ zeroOrMore(Type) ~ ')' ~ Type ~ EOI ~> {
      (paramSeq: Seq[DescriptorType], retType: DescriptorType) => Descriptor(paramSeq.toList, retType)
    }
  }

  def Type: Rule1[DescriptorType] = rule {
    // based on the example in the JSON Parser from Parboiled2 doing a one character lookahead here
    // all descriptor types can be inferred from the first character
    run {
      (cursorChar: @switch) match {
        case 'L' => Class
        case 'Z' => Boolean
        case 'B' => Byte
        case 'C' => Char
        case 'S' => Short
        case 'I' => Int
        case 'J' => Long
        case 'F' => Float
        case 'D' => Double
        case 'V' => Void
        case '[' => Array
        case _ => MISMATCH
      }
    }
  }

  private def Array: Rule1[DescriptorType] = rule {
    '[' ~ Type ~> { c => ArrayDescriptor(c) }
  }

  override val PackageNamePredicate = CharPredicate.All -- ";/ "
  override val ClassNameCharPredicate = CharPredicate.All -- ";/ "
}
