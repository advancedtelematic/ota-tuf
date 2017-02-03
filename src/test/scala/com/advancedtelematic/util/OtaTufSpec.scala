package com.advancedtelematic.util

import java.security.Security
import java.util.concurrent.ConcurrentHashMap

import akka.http.scaladsl.model.Uri
import com.advancedtelematic.ota_tuf.data.DataType.KeyId
import com.advancedtelematic.ota_tuf.vault.VaultClient
import com.advancedtelematic.ota_tuf.vault.VaultClient.VaultKey
import com.typesafe.config.ConfigFactory
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FunSuite, Matchers}

import scala.concurrent.Future

abstract class OtaTufSpec extends FunSuite with Matchers with ScalaFutures {

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

  private lazy val config = ConfigFactory.load()

  lazy val vaultAddr = Uri(config.getString("vault.address"))

  lazy val vaultToken = config.getString("vault.token")

  lazy val vaultMount = config.getString("vault.mount")
}
