// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.util

import com.typesafe.config._

object Timing {
  val dilation: Double =
    try ConfigFactory.load().getDouble("akka.test.timefactor")
    catch {
      case e: ConfigException.Missing => 1.0
    }
}
