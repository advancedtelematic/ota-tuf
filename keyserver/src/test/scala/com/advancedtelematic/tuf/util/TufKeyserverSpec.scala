package com.advancedtelematic.tuf.util

import java.security.Security
import java.time.Instant
import java.util.concurrent.ConcurrentHashMap

import akka.http.scaladsl.util.FastFuture
import com.advancedtelematic.libtuf.data.TufDataType.KeyId
import com.advancedtelematic.tuf.keyserver.Settings
import com.advancedtelematic.tuf.keyserver.vault.VaultClient
import com.advancedtelematic.tuf.keyserver.vault.VaultClient.{VaultKey, VaultKeyNotFound}
import net.i2p.crypto.eddsa.EdDSASecurityProvider
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FunSuite, Matchers}

import scala.collection.mutable
import scala.concurrent.Future

class FakeVault extends VaultClient {
  private val keys = new ConcurrentHashMap[KeyId, VaultKey]
  val renewedTimestamps = mutable.MutableList[Instant]()

  override def createKey(key: VaultKey): Future[Unit] = {
    keys.put(key.id, key)
    Future.successful(())
  }

  override def findKey(keyId: KeyId): Future[VaultKey] =
    Option(keys.get(keyId)) match {
      case Some(k) => Future.successful(k)
      case None => Future.failed(VaultKeyNotFound)
    }

  override def deleteKey(keyId: KeyId): Future[Unit] =
    Future.successful(keys.remove(keyId))

  override def renewToken(): Future[Unit] = {
    this.synchronized {
      Instant.now +=: renewedTimestamps
    }
    FastFuture.successful(())
  }

  def lastRenew: Option[Instant] = this.synchronized { renewedTimestamps.headOption }
}

abstract class TufKeyserverSpec extends FunSuite with Matchers with ScalaFutures with Settings {
  Security.addProvider(new BouncyCastleProvider)
  Security.addProvider(new EdDSASecurityProvider)

  val fakeVault = new FakeVault
}
