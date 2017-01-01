// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.core

import scala.concurrent.duration._

import akka.testkit._
import org.ensime.fixture.SharedTestKitFixture
import org.ensime.util.EnsimeSpec

class FloodGateSpec extends EnsimeSpec with SharedTestKitFixture {

  val ping = "ping"

  "FloodGate" should "not send messages to the target" in withTestKit { fix =>
    import fix._
    val target = TestProbe()
    val delay = TestActorRef(FloodGate(target.ref))

    delay ! ping
    delay ! ping
    delay ! ping

    target.expectNoMsg(3 seconds)
  }

  it should "send messages after activation" in withTestKit { fix =>
    import fix._
    val target = TestProbe()
    val delay = TestActorRef(FloodGate(target.ref))

    delay ! ping
    delay ! ping
    delay ! ping

    delay ! FloodGate.Activate

    target.expectMsg(ping)
    target.lastSender shouldBe self
    target.expectMsg(ping)
    target.lastSender shouldBe self
    target.expectMsg(ping)
    target.lastSender shouldBe self
  }

}
