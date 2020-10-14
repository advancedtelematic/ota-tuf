package com.advancedtelematic.libtuf.data

import java.security.Security
import java.time.Instant

import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.data.ClientDataType.{RoleKeys, RootRole}
import com.advancedtelematic.libtuf.data.RootManipulationOps._
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.{ROOT, TARGETS}
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.scalatest.{FunSuite, Matchers}

class RootManipulationOpsSpec extends FunSuite with Matchers {

  Security.addProvider(new BouncyCastleProvider)

  private def generateRoot: RootRole = {
    val keyPair = TufCrypto.rsaCrypto.generateKeyPair()
    val roles = Map(ROOT -> RoleKeys(List(keyPair.pubkey.id), threshold = 1))
    val clientKeys = Map(keyPair.pubkey.id -> keyPair.pubkey)
    RootRole(clientKeys, roles, expires = Instant.now, version = 1)
  }

  test("root keys get only added once without threshold") {
    val rootRole: RootRole = generateRoot
    val newKey = TufCrypto.rsaCrypto.generateKeyPair().pubkey
    rootRole.withRoleKeys(ROOT, newKey, newKey).roles(ROOT).keyids.count(_ == newKey.id) shouldBe 1
  }

  test("target keys get only added once with threshold") {
    val rootRole: RootRole = generateRoot
    val newKey = TufCrypto.rsaCrypto.generateKeyPair().pubkey
    rootRole.withRoleKeys(TARGETS, 1, newKey, newKey).roles(TARGETS).keyids.count(_ == newKey.id) shouldBe 1
  }

}
