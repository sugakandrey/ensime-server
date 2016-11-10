// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime

import akka.actor.{ ActorSystem, Terminated }

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

object AkkaBackCompat {
  implicit class ActorSystemShutdownBackCompat(val actorSystem: ActorSystem) {
    def close(): Try[Unit] = Try(actorSystem.terminate)

    def awaitClosure(d: Duration = Duration.Inf): Try[Terminated] = Try(Await.result(actorSystem.whenTerminated, d))
  }
}
