// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.config

import org.ensime.sexp._
import org.ensime.sexp.formats._
import scalariform.formatter.preferences._

trait ScalariformFormat {
  this: BasicFormats =>

  implicit object FormattingPreferencesFormat extends SexpFormat[FormattingPreferences] {

    private def key(d: PreferenceDescriptor[_]) = SexpSymbol(":" + d.key)

    private def deser(
      descriptor: PreferenceDescriptor[_],
      data: Map[SexpSymbol, Sexp]
    ): Option[Any] =
      data.get(key(descriptor)).map { sexp =>
        descriptor.preferenceType match {
          case BooleanPreference => sexp.convertTo[Boolean]
          case IntegerPreference(_, _) => sexp.convertTo[Int]
          case IntentPreference => sexp.convertTo[String].toLowerCase match {
            case "preserve" => Preserve
            case "force" => Force
            case "prevent" => Prevent
            case intent => throw new DeserializationException(s"Unknown intent: $intent")
          }
        }
      }

    def read(s: Sexp): FormattingPreferences = s match {
      case SexpNil =>
        FormattingPreferences()

      case SexpData(data) =>
        val custom = for {
          descriptor <- AllPreferences.preferences
          value <- deser(descriptor, data)
        } yield (descriptor, value)
        new FormattingPreferences(custom.toMap)

      case x => deserializationError(x)
    }

    private def ser(
      descriptor: PreferenceDescriptor[_],
      value: Any
    ): Sexp = descriptor.preferenceType match {
      case BooleanPreference => value.asInstanceOf[Boolean].toSexp
      case IntegerPreference(_, _) => value.asInstanceOf[Int].toSexp
      case IntentPreference => value.getClass.getSimpleName.replaceAll("\\$", "").toLowerCase.toSexp
    }

    def write(f: FormattingPreferences) = {
      val data = f.preferencesMap.map {
        case (d, v) => key(d) -> ser(d, v)
      }
      SexpData(data.toList)
    }
  }
}
