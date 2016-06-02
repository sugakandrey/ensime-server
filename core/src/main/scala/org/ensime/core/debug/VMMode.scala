// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core.debug

sealed abstract class VmMode {
  /**
   * @return True if the vm should be existed for this mode
   */
  def shouldExit: Boolean
}

private final case class VmAttach(hostname: String, port: String) extends VmMode {
  override def shouldExit: Boolean = false
}
private final case class VmStart(commandLine: String) extends VmMode {
  override def shouldExit: Boolean = true
}
