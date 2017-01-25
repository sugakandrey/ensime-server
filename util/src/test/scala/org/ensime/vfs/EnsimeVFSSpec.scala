// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.vfs

import java.io.File
import org.scalatest._

class EnsimeVFSSpec extends FlatSpec with Matchers {
  "RichVFS.vjar" should "return jar files with URI components in their file name" in {
    val jar = EnsimeVFS().vjar(new File("src/test/fixtures/oh%3ahai+there.jar"))
    jar.isReadable shouldBe true
  }
}
