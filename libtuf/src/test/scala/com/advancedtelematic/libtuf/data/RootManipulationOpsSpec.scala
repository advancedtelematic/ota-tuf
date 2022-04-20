package com.advancedtelematic.libtuf.data

import java.security.Security
import java.time.Instant
import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.data.ClientDataType.{RoleKeys, RootRole}
import com.advancedtelematic.libtuf.data.RootManipulationOps._
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.{ROOT, TARGETS}
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.scalatest.TryValues.convertTryToSuccessOrFailure
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

  test("sets threshold for specified role in root.json") {
    val rootRole: RootRole = generateRoot
    val newKey = TufCrypto.rsaCrypto.generateKeyPair().pubkey
    val newKey2 = TufCrypto.rsaCrypto.generateKeyPair().pubkey
    val rootRoleOldThreshold = rootRole.withRoleKeys(TARGETS, 1, newKey, newKey2)

    val rootRoleNewThreshold = rootRoleOldThreshold.withRoleThreshold(TARGETS, 2)

    rootRoleNewThreshold.success.value.roles(TARGETS).threshold shouldBe 2
  }

  test("threshold is not set when threshold is greater than number of keys for role") {
    val rootRole: RootRole = generateRoot
    val newKey = TufCrypto.rsaCrypto.generateKeyPair().pubkey
    val rootRoleOldThreshold = rootRole.withRoleKeys(TARGETS, 1, newKey)

    val rootRoleInvalidThresholdError = rootRoleOldThreshold.withRoleThreshold(TARGETS, 2).failure.exception

    rootRoleInvalidThresholdError shouldBe a [InvalidThresholdError]
    rootRoleInvalidThresholdError.getMessage shouldBe InvalidThresholdErrorMessage
  }

  test("threshold is not set when threshold is less than one") {
    val rootRole: RootRole = generateRoot
    val newKey = TufCrypto.rsaCrypto.generateKeyPair().pubkey
    val newKey2 = TufCrypto.rsaCrypto.generateKeyPair().pubkey
    val rootRoleOldThreshold = rootRole.withRoleKeys(TARGETS, 1, newKey, newKey2)

    val rootRoleInvalidThresholdError = rootRoleOldThreshold.withRoleThreshold(TARGETS, 0).failure.exception

    rootRoleInvalidThresholdError shouldBe a [InvalidThresholdError]
    rootRoleInvalidThresholdError.getMessage shouldBe InvalidThresholdErrorMessage
  }
}
