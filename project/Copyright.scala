import sbt._
import de.heikoseeberger.sbtheader._
import scala.util.matching.Regex

object Copyright extends AutoPlugin {
  override def requires = HeaderPlugin
  override def trigger = allRequirements

  val HeaderRegex = "(?s)(// Copyright[^\\n]*[\\n]// Licen[cs]e[^\\n]*[\\n])(.*)".r

  val CopyrightHeader = "// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs"
  val ApacheHeader = "// License: http://www.apache.org/licenses/LICENSE-2.0"
  val GplHeader = "// License: http://www.gnu.org/licenses/gpl-3.0.en.html"

  def LicenseWithCopyright(license: String) = HeaderRegex -> s"$CopyrightHeader\n$license\n"

  val ApacheMap = Map(
    "scala" -> LicenseWithCopyright(ApacheHeader),
    "java" -> LicenseWithCopyright(ApacheHeader)
  )

  val GplMap = Map(
    "scala" -> LicenseWithCopyright(GplHeader),
    "java" -> LicenseWithCopyright(GplHeader)
  )

}
