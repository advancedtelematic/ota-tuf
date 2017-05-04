package com.advancedtelematic.tuf.keyserver

import java.time.Instant

import akka.actor.ActorSystem
import akka.testkit.{TestKitBase, TestProbe}
import com.advancedtelematic.libats.test.LongTest
import com.advancedtelematic.tuf.keyserver.vault.VaultTokenRenewer
import com.advancedtelematic.tuf.keyserver.vault.VaultTokenRenewer.Renewed
import com.advancedtelematic.tuf.util.TufKeyserverSpec
import org.scalatest.BeforeAndAfterAll

import scala.concurrent.duration._

class VaultTokenRenewerSpec extends TufKeyserverSpec with TestKitBase with LongTest with BeforeAndAfterAll {

  implicit lazy val system = ActorSystem(this.getClass.getSimpleName)

  test("renews token on start") {
    val now = Instant.now()
    val probe = TestProbe()
    probe.childActorOf(VaultTokenRenewer.withBackoff(fakeVault, 1.hour))

    probe.expectMsg(Renewed)
    fakeVault.lastRenew.map(_.isAfter(now)) should contain(true)
  }

  test("renews before renewIncrement") {
    val now = Instant.now()
    val probe = TestProbe()
    probe.childActorOf(VaultTokenRenewer.withBackoff(fakeVault, 1.second))

    probe.expectMsg(Renewed)
    probe.expectMsg(Renewed)

    fakeVault.lastRenew.map(_.isAfter(now)) should contain(true)
  }

  override protected def afterAll(): Unit = {
    system.terminate()
  }
}
