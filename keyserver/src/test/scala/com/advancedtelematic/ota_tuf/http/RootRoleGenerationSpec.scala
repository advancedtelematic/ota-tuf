package com.advancedtelematic.ota_tuf.http

import com.advancedtelematic.ota_tuf.data.Codecs._
import com.advancedtelematic.libtuf.data.TufCodecs._
import scala.async.Async._
import com.advancedtelematic.util.OtaTufSpec
import org.genivi.sota.core.DatabaseSpec
import org.scalatest.Inspectors
import io.circe.syntax._
import CanonicalJson._
import com.advancedtelematic.libtuf.crypt.RsaKeyPair
import com.advancedtelematic.libtuf.data.TufDataType.{RoleType, Signature}
import com.advancedtelematic.ota_tuf.daemon.KeyGenerationOp
import com.advancedtelematic.ota_tuf.data.KeyServerDataType.{KeyGenId, KeyGenRequest}
import com.advancedtelematic.ota_tuf.data.KeyServerDataType.KeyGenRequestStatus
import com.advancedtelematic.ota_tuf.db.KeyGenRequestSupport
import com.advancedtelematic.ota_tuf.roles.RootRoleGeneration
import org.scalatest.concurrent.PatienceConfiguration
import org.scalatest.time.{Seconds, Span}

import scala.concurrent.ExecutionContext
import com.advancedtelematic.libtuf.data.TufDataType.RepoId

class RootRoleGenerationSpec extends OtaTufSpec with DatabaseSpec
with Inspectors with PatienceConfiguration
with KeyGenRequestSupport {

  implicit val ec = ExecutionContext.global

  override implicit def patienceConfig = PatienceConfig().copy(timeout = Span(5, Seconds))

  val rootGeneration = new RootRoleGeneration(fakeVault)
  val keyGenerationOp = new KeyGenerationOp(fakeVault)

  test("root role payload must be signed with root key") {
    val repoId = RepoId.generate()
    val rootKeyGenRequest = KeyGenRequest(KeyGenId.generate(),
      repoId, KeyGenRequestStatus.GENERATED, RoleType.ROOT)

    async {
      await(keyGenRepo.persist(rootKeyGenRequest))

      await(keyGenerationOp.processGenerationRequest(rootKeyGenRequest))

      val signed = await(rootGeneration.findSigned(repoId))

      val rootKeyId = signed.signed.roles(RoleType.ROOT.toString).keyids.head
      val publicKey = signed.signed.keys(rootKeyId).keyval

      val clientSignature = signed.signatures.head

      val sig = Signature(clientSignature.sig, clientSignature.method)

      RsaKeyPair.isValid(publicKey, sig,
        signed.signed.asJson.canonical.getBytes) shouldBe true
    }
  }
}
