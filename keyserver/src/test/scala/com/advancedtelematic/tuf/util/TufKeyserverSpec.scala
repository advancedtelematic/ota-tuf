package com.advancedtelematic.tuf.util

import java.security.Security
import java.util.concurrent.ConcurrentHashMap

import com.advancedtelematic.tuf.keyserver.Settings
import com.advancedtelematic.tuf.keyserver.vault.VaultClient
import com.advancedtelematic.tuf.keyserver.vault.VaultClient.{VaultKey, VaultKeyNotFound}
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FunSuite, Matchers}
import com.advancedtelematic.libtuf.data.TufDataType.KeyId

import scala.concurrent.Future


abstract class TufKeyserverSpec extends FunSuite with Matchers with ScalaFutures with Settings {

  Security.addProvider(new BouncyCastleProvider())

  val fakeVault = new VaultClient {
    private val keys = new ConcurrentHashMap[KeyId, VaultKey]

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
  }
}
