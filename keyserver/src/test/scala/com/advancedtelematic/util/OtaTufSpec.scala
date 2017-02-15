package com.advancedtelematic.util

import java.security.Security
import java.util.concurrent.ConcurrentHashMap

import akka.actor.ActorSystem
import akka.http.scaladsl.testkit.RouteTestTimeout
import com.advancedtelematic.ota_tuf.Settings
import com.advancedtelematic.ota_tuf.data.DataType.KeyId
import com.advancedtelematic.ota_tuf.vault.VaultClient
import com.advancedtelematic.ota_tuf.vault.VaultClient.VaultKey
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.scalatest.concurrent.{PatienceConfiguration, ScalaFutures}
import org.scalatest.{FunSuite, Matchers}

import scala.concurrent.duration._
import akka.testkit.TestDuration
import org.scalatest.time.{Millis, Seconds, Span}

import scala.concurrent.Future

trait LongHttpRequest {
  implicit def default(implicit system: ActorSystem) =
    RouteTestTimeout(10.seconds.dilated(system))
}

trait LongTest extends PatienceConfiguration {
  override implicit def patienceConfig = PatienceConfig(timeout = Span(3, Seconds), interval = Span(100, Millis))
}

abstract class OtaTufSpec extends FunSuite with Matchers with ScalaFutures with Settings {

  Security.addProvider(new BouncyCastleProvider())

  val fakeVault = new VaultClient {
    private val keys = new ConcurrentHashMap[KeyId, VaultKey]

    override def createKey(key: VaultKey): Future[Unit] = {
      keys.put(key.id, key)
      Future.successful(())
    }

    override def findKey(keyId: KeyId): Future[VaultKey] =
      Future.successful(keys.get(keyId))
  }
}
