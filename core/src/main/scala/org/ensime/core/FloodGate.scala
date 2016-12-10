// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import FloodGate.Activate
import akka.actor._

/**
 * Holds messages until an Activate is received.
 */
class FloodGate(
    target: ActorRef
) extends Actor with ActorLogging with Stash {

  override def receive = waiting

  val waiting: Receive = {
    case Activate =>
      context.become(pass)
      unstashAll()

    case message =>
      stash()
  }

  val pass: Receive = {
    case message => target forward message
  }

}

object FloodGate {
  object Activate

  def apply(target: ActorRef): Props = Props(new FloodGate(target))
}
