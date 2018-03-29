package com.advancedtelematic.tuf.keyserver.http


import scala.async.Async._
import com.advancedtelematic.tuf.util.TufKeyserverSpec
import com.advancedtelematic.libats.test.DatabaseSpec
import org.scalatest.Inspectors
import io.circe.syntax._
import com.advancedtelematic.libtuf.crypt.CanonicalJson._
import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.data.TufDataType._
import com.advancedtelematic.tuf.keyserver.daemon.DefaultKeyGenerationOp
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.{KeyGenId, KeyGenRequest}
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.KeyGenRequestStatus
import com.advancedtelematic.tuf.keyserver.roles.SignedRootRoles
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.TufDataType.RepoId
import com.advancedtelematic.tuf.keyserver.db.{KeyGenRequestSupport, SignedRootRoleSupport}
import org.scalatest.concurrent.PatienceConfiguration
import org.scalatest.time.{Seconds, Span}

import scala.concurrent.ExecutionContext

class SignedRootRolesSpec extends TufKeyserverSpec with DatabaseSpec
with Inspectors with PatienceConfiguration
with KeyGenRequestSupport
with SignedRootRoleSupport {

  implicit val ec = ExecutionContext.global

  override implicit def patienceConfig = PatienceConfig().copy(timeout = Span(10, Seconds))

  val signedRootRoles = new SignedRootRoles(fakeVault)
  val keyGenerationOp = DefaultKeyGenerationOp(fakeVault)

  def keySpecific(keyType: KeyType, name: String): Unit = {

    test("root role payload must be signed with root key " + name) {
      val repoId = RepoId.generate()
      val rootKeyGenRequest = KeyGenRequest(KeyGenId.generate(),
        repoId, KeyGenRequestStatus.REQUESTED, RoleType.ROOT, keyType.crypto.defaultKeySize, keyType)

      async {
        await(keyGenRepo.persist(rootKeyGenRequest))
        await(keyGenerationOp(rootKeyGenRequest))
        val signed = await(signedRootRoles.findAndPersist(repoId))

        val rootKeyId = signed.signed.roles(RoleType.ROOT).keyids.head
        val publicKey = signed.signed.keys(rootKeyId).keyval

        val clientSignature = signed.signatures.head

        val sig = Signature(clientSignature.sig, clientSignature.method)

        TufCrypto.isValid(sig, publicKey, signed.signed.asJson.canonical.getBytes) shouldBe true
      }.futureValue
    }

    test("persists signed payload when finding " + name) {
      val repoId = RepoId.generate()
      val rootKeyGenRequest = KeyGenRequest(KeyGenId.generate(),
        repoId, KeyGenRequestStatus.REQUESTED, RoleType.ROOT, keyType.crypto.defaultKeySize, keyType)


      async {
        await(keyGenRepo.persist(rootKeyGenRequest))
        await(keyGenerationOp(rootKeyGenRequest))

        val signed = await(signedRootRoles.findAndPersist(repoId))

        await(signedRootRoleRepo.findLatest(repoId)).asJson shouldBe signed.asJson
      }.futureValue
    }

  }

  testsFor(keySpecific(RsaKeyType, "RSA"))
  testsFor(keySpecific(Ed25519KeyType, "Ed25519"))
}
