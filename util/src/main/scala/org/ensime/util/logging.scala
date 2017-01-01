// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.util

import org.slf4j.bridge.SLF4JBridgeHandler

object Slf4jSetup {
  SLF4JBridgeHandler.removeHandlersForRootLogger()
  SLF4JBridgeHandler.install()

  /**
   * Call for the side effect of setting up SLF4J's bridge.
   */
  def init(): Unit = ()
}
