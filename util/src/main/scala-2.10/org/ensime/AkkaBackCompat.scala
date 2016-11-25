// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime

import akka.actor.ActorSystem

import scala.concurrent.blocking
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

trait AkkaBackCompat {
  implicit class ActorSystemShutdownBackCompat(val actorSystem: ActorSystem) {
    def terminate(): Future[Unit] = Future.successful(actorSystem.shutdown())

    def whenTerminated: Future[Unit] = Future {
      blocking { actorSystem.awaitTermination(Duration.Inf) }
    }(global)
  }
}
