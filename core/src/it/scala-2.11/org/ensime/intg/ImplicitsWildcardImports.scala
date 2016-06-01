// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.intg

import org.ensime.api._
import org.ensime.fixture._
import org.ensime.util.EnsimeSpec
import org.ensime.util.file._

/**
 * This is a reproduction of https://github.com/ensime/ensime-server/issues/1176
 * which might be caused by https://github.com/scala/scala/pull/4777
 */
class ImplicitsWildcardImports extends EnsimeSpec
    with IsolatedEnsimeConfigFixture
    with IsolatedTestKitFixture
    with IsolatedProjectFixture {

  val original = EnsimeConfigFixture.ImplicitsTestProject

  "ensime-server" should "allow getting symbols also after marking implicits imported with a wildcard import" in {
    withEnsimeConfig { implicit config =>
      withTestKit { implicit testkit =>
        withProject { (project, asyncHelper) =>
          import testkit._

          val sourceRoot = scalaMain(config)
          val exampleFile = sourceRoot / "org/example/Example.scala"

          project ! SymbolAtPointReq(Left(exampleFile), 116)
          expectMsgType[SymbolInfo].name should be("seconds")

          project ! ImplicitInfoReq(Left(exampleFile), OffsetRange(0, 121))
          expectMsgType[ImplicitInfos]

          project ! SymbolAtPointReq(Left(exampleFile), 116)
          expectMsgType[SymbolInfo].name should be("seconds")
        }
      }
    }
  }
}
