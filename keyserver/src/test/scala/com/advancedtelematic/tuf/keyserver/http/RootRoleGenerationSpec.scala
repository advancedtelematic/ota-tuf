package com.advancedtelematic.tuf.keyserver.http

import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.TufCodecs._

import scala.async.Async._
import com.advancedtelematic.tuf.util.TufKeyserverSpec
import com.advancedtelematic.libats.test.DatabaseSpec
import org.scalatest.Inspectors
import io.circe.syntax._
import com.advancedtelematic.libtuf.crypt.CanonicalJson._
import com.advancedtelematic.libtuf.crypt.RsaKeyPair
import com.advancedtelematic.libtuf.data.TufDataType._
import com.advancedtelematic.tuf.keyserver.daemon.KeyGenerationOp
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.{KeyGenId, KeyGenRequest}
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.KeyGenRequestStatus
import com.advancedtelematic.tuf.keyserver.roles.RootRoleGeneration
import org.scalatest.concurrent.PatienceConfiguration
import org.scalatest.time.{Seconds, Span}

import scala.concurrent.ExecutionContext
import com.advancedtelematic.libtuf.data.TufDataType.RepoId
import com.advancedtelematic.tuf.keyserver.db.KeyGenRequestSupport

class RootRoleGenerationSpec extends TufKeyserverSpec with DatabaseSpec
with Inspectors with PatienceConfiguration
with KeyGenRequestSupport {

  implicit val ec = ExecutionContext.global

  override implicit def patienceConfig = PatienceConfig().copy(timeout = Span(5, Seconds))

  val rootGeneration = new RootRoleGeneration()
  val keyGenerationOp = new KeyGenerationOp(fakeVault)

  test("root role payload must be signed with root key") {
    pending
//    val repoId = RepoId.generate()
//    val rootKeyGenRequest = KeyGenRequest(KeyGenId.generate(),
//      repoId, KeyGenRequestStatus.GENERATED, RoleType.ROOT, keySize = 1024)
//
//    async {
//      await(keyGenRepo.persist(rootKeyGenRequest))
//
//      await(keyGenerationOp.processGenerationRequest(rootKeyGenRequest))
//
//      val signed = await(rootGeneration.generateSigned(repoId))
//
//      val rootKeyId = signed.signed.roles(RoleType.ROOT).keyids.head
//      val publicKey = signed.signed.keys(rootKeyId).keyval
//
//      val clientSignature = signed.signatures.head
//
//      val sig = Signature(clientSignature.sig, clientSignature.method)
//
//      RsaKeyPair.isValid(publicKey, sig,
//        signed.signed.asJson.canonical.getBytes) shouldBe true
//    }
  }
}
