// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

trait DocResolverBackCompat {
  def scalaSigToLocalUri(prefix: String, jarName: String, scalaSig: DocSig): String = {
    val anchor = scalaFqnToPath(scalaSig.fqn) +
      scalaSig.member.map("#" + _).getOrElse("")
    s"$prefix/$jarName/$anchor"
  }

  def scalaFqnToPath(fqn: DocFqn): String = {
    if (fqn.typeName == "package") {
      fqn.pack.replace(".", "/") + "/index.html"
    } else fqn.pack.replace(".", "/") + "/" + fqn.typeName + ".html"
  }
}
