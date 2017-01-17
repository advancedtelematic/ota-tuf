package com.advancedtelematic.util

import java.security.Security
import java.util.concurrent.ConcurrentHashMap

import com.advancedtelematic.ota_tuf.data.DataType.KeyId
import com.advancedtelematic.ota_tuf.vault.VaultClient
import com.advancedtelematic.ota_tuf.vault.VaultClient.VaultKey
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FunSuite, Matchers}

import scala.concurrent.Future

abstract class OtaTufSpec extends FunSuite with Matchers with ScalaFutures {
  Security.addProvider(new BouncyCastleProvider())

  val fakeVault = new VaultClient {
    val keys = new ConcurrentHashMap[KeyId, VaultKey]

    override def createKey(key: VaultKey): Future[Unit] = {
      keys.put(key.id, key)
      Future.successful(())
    }
  }
}
