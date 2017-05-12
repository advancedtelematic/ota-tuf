package com.advancedtelematic.tuf.keyserver.http

import java.security.PrivateKey

import com.advancedtelematic.libtuf.crypt.CanonicalJson._
import akka.http.scaladsl.model.StatusCodes
import cats.data.NonEmptyList
import com.advancedtelematic.tuf.util.{ResourceSpec, TufKeyserverSpec}
import io.circe.generic.auto._
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import cats.syntax.show._
import com.advancedtelematic.libtuf.crypt.RsaKeyPair.RsaKeyIdConversion
import com.advancedtelematic.libtuf.crypt.RsaKeyPair
import com.advancedtelematic.tuf.keyserver.daemon.KeyGenerationOp
import com.advancedtelematic.libtuf.data.TufDataType._
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.{Key, KeyGenId}
import com.advancedtelematic.libtuf.data.TufDataType.RepoId
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.KeyGenRequestStatus
import io.circe.{Decoder, Encoder, Json}
import org.scalatest.Inspectors
import org.scalatest.concurrent.PatienceConfiguration
import io.circe.syntax._
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.ClientDataType.{ClientPrivateKey, RootRole}
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.tuf.keyserver.db.{KeyGenRequestSupport, KeyRepositorySupport}
import eu.timepit.refined.api.Refined
import org.scalatest.time.{Millis, Seconds, Span}
import io.circe.generic.semiauto._
import scala.concurrent.{ExecutionContext, Future}

class RootRoleResourceSpec extends TufKeyserverSpec
  with ResourceSpec
  with KeyGenRequestSupport
  with KeyRepositorySupport
  with PatienceConfiguration
  with Inspectors {

  implicit val ec = ExecutionContext.global

  val keyGenerationOp = new KeyGenerationOp(fakeVault)

  override implicit def patienceConfig = PatienceConfig(timeout = Span(10, Seconds), interval = Span(100, Millis))

  test("POST returns Accepted") {
    Post(apiUri(s"root/${RepoId.generate().show}"), ClientRootGenRequest()) ~> routes ~> check {
      status shouldBe StatusCodes.Accepted
    }
  }

  test("POST creates key gen request for all types of roles") {
    val repoId = RepoId.generate()

    Post(apiUri(s"root/${repoId.show}"), ClientRootGenRequest()) ~> routes ~> check {
      status shouldBe StatusCodes.Accepted
    }

    val requests = keyGenRepo.findBy(repoId).futureValue

    requests.size shouldBe RoleType.ALL.size

    requests.map(_.status) should contain only KeyGenRequestStatus.REQUESTED
  }

  test("POST creates all roles") {
    val repoId = RepoId.generate()

    Post(apiUri(s"root/${repoId.show}"), ClientRootGenRequest()) ~> routes ~> check {
      status shouldBe StatusCodes.Accepted
    }

    val requests = keyGenRepo.findBy(repoId).futureValue

    requests.size shouldBe RoleType.ALL.size

    requests.map(_.roleType) should contain allElementsOf RoleType.ALL
  }

  test("PUT forces a retry on ERROR requests") {
    val repoId = RepoId.generate()

    Post(apiUri(s"root/${repoId.show}"), ClientRootGenRequest()) ~> routes ~> check {
      status shouldBe StatusCodes.Accepted
    }

    val requests = keyGenRepo.findBy(repoId).futureValue
    keyGenRepo.setStatusAll(requests.map(_.id), KeyGenRequestStatus.ERROR).futureValue
    keyGenRepo.findBy(repoId).futureValue.map(_.status) should contain only KeyGenRequestStatus.ERROR

    Put(apiUri(s"root/${repoId.show}"), ClientRootGenRequest()) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }

    val updatedRequests = keyGenRepo.findBy(repoId).futureValue
    updatedRequests.map(_.status) should contain only KeyGenRequestStatus.REQUESTED
  }

  test("POST fails if key for role already exists") {
    val repoId = RepoId.generate()

    Post(apiUri(s"root/${repoId.show}"), ClientRootGenRequest()) ~> routes ~> check {
      status shouldBe StatusCodes.Accepted
    }

    Post(apiUri(s"root/${repoId.show}"), ClientRootGenRequest()) ~> routes ~> check {
      status shouldBe StatusCodes.Conflict
    }
  }

  test("GET returns NotFound if keys do not exist") {
    val repoId = RepoId.generate()

    Get(apiUri(s"root/${repoId.show}")) ~> routes ~> check {
      status shouldBe StatusCodes.NotFound
    }
  }

  test("GET returns Locked if keys are not ready") {
    val repoId = RepoId.generate()

    Post(apiUri(s"root/${repoId.show}"), ClientRootGenRequest()) ~> routes ~> check {
      status shouldBe StatusCodes.Accepted
    }

    Get(apiUri(s"root/${repoId.show}")) ~> routes ~> check {
      status shouldBe StatusCodes.Locked
    }
  }

  test("GET returns 200 if keys are ready") {
    val repoId = RepoId.generate()

    generateRootRole(repoId).futureValue

    Get(apiUri(s"root/${repoId.show}")) ~> routes ~> check {
      status shouldBe StatusCodes.OK

      println(responseAs[Json].spaces2)

      val signedPayload = responseAs[SignedPayload[RootRole]]
      val rootRole = signedPayload.signed

      signedPayload.signatures should have size 1 // Signed with root only

      rootRole.keys should have size RoleType.ALL.size

      rootRole.roles should have size RoleType.ALL.size
      rootRole.roles.keys should contain allElementsOf RoleType.ALL
    }
  }

  test("GET returns 200 with all keys if threshold > 1 ") {
    val repoId = RepoId.generate()

    generateRootRole(repoId, threshold = 4).futureValue

    Get(apiUri(s"root/${repoId.show}")) ~> routes ~> check {
      status shouldBe StatusCodes.OK

      val signedPayload = responseAs[SignedPayload[RootRole]]
      val rootRole = signedPayload.signed

      signedPayload.signatures should have size 4 // Signed with root only

      rootRole.keys should have size RoleType.ALL.size * 4

      forAll(RoleType.ALL) { t =>
        rootRole.roles(t).threshold shouldBe 4
        rootRole.roles(t).keyids should have size 4
      }
    }
  }

  test("POST to repoId/roletype signs any payload with existing keys ") {
    val repoId = RepoId.generate()

    generateRootRole(repoId).futureValue

    val toSignPayload = Json.fromFields(List("some" -> Json.fromString("signed stuff")))

    Post(apiUri(s"root/${repoId.show}/targets"), toSignPayload) ~> routes ~> check {
      status shouldBe StatusCodes.OK

      val signedPayload = responseAs[SignedPayload[Json]]

      println(signedPayload.asJson.spaces2)

      signedPayload.signatures shouldNot be(empty)

      val targetKey = keyRepo.repoKeys(repoId, RoleType.TARGETS).futureValue.head

      signedPayload.signed shouldBe toSignPayload

      forAll(signedPayload.signatures) { sig =>
        sig.keyid shouldBe targetKey.id

        val isValidT = RoleSigning.isValid(signedPayload.signed, sig, targetKey.publicKey)
        isValidT shouldBe true
      }
    }
  }

  test("returns 404 if there are no keys for given repo/roletype") {
    val repoId = RepoId.generate()

    Post(apiUri(s"root/${repoId.show}/targets"), Json.Null) ~> routes ~> check {
      status shouldBe StatusCodes.NotFound
    }
  }

  test("GET on private key returns 404 when key does not exist") {
    val repoId = RepoId.generate()
    val keyId: KeyId = Refined.unsafeApply("8a17927d32c40ca87d71e74123b85a4f465d76c2edb0c8e364559bd5fc3d035a")

    Get(apiUri(s"root/${repoId.show}/private_keys/${keyId.value}")) ~> routes ~> check {
      status shouldBe StatusCodes.NotFound
    }
  }

  test("DELETE on private_key wipes private key") {
    val repoId = RepoId.generate()

    generateRootRole(repoId).futureValue

    val rootKeyId = Get(apiUri(s"root/${repoId.show}")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[SignedPayload[RootRole]].signed.roles(RoleType.ROOT).keyids.head
    }

    Delete(apiUri(s"root/${repoId.show}/private_keys/${rootKeyId.value}")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[ClientPrivateKey].keyval shouldBe a[PrivateKey]
    }

    Get(apiUri(s"root/${repoId.show}/private_keys/${rootKeyId.value}")) ~> routes ~> check {
      status shouldBe StatusCodes.NotFound
    }
  }

  test("wiping private key still allows download of root.json") {
    val repoId = RepoId.generate()
    generateRootRole(repoId).futureValue

    val rootKeyId = Get(apiUri(s"root/${repoId.show}")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[SignedPayload[RootRole]].signed.roles(RoleType.ROOT).keyids.head
    }

    Delete(apiUri(s"root/${repoId.show}/private_keys/${rootKeyId.value}")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[ClientPrivateKey].keyval shouldBe a[PrivateKey]
    }

    Get(apiUri(s"root/${repoId.show}")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[SignedPayload[RootRole]].signed shouldBe a[RootRole]
    }
  }

  test("XXX GET on an unsigned root returns unsigned root") {
    val repoId = RepoId.generate()
    generateRootRole(repoId).futureValue

    Get(apiUri(s"root/${repoId.show}/stuff")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[RootRole] shouldBe a[RootRole]
    }

    // TODO: Check version is 2
  }

  test("XXX POST returns 4xx when signature is not valid") {
    val repoId = RepoId.generate()
    generateRootRole(repoId).futureValue

    val rootRole = Get(apiUri(s"root/${repoId.show}/stuff")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[RootRole]
    }

    val rootKeyId = rootRole.roles(RoleType.ROOT).keyids.head
    val signedPayload = clientSignPayload(rootKeyId, rootRole, "sign something else")

    Get(apiUri(s"root/${repoId.show}/stuff")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[RootRole] shouldBe a[RootRole]
    }

    Post(apiUri(s"root/${repoId.show}/stuff"), signedPayload) ~> routes ~> check {
      status shouldBe StatusCodes.BadRequest
      responseAs[JsonErrors].head should include(s"Invalid signature for key $rootKeyId")
    }
  }

  test("XXX POST returns error when public key is missing") {
    val repoId = RepoId.generate()
    generateRootRole(repoId).futureValue

    val rootRole = Get(apiUri(s"root/${repoId.show}/stuff")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[RootRole]
    }

    val keyPair = RsaKeyPair.generate(size = 1024)
    val signature = RsaKeyPair.sign(keyPair.getPrivate, rootRole.asJson.canonical.getBytes)
    val clientSignature = ClientSignature(keyPair.id, signature.method, signature.sig)
    val signedPayload = SignedPayload(Seq(clientSignature), rootRole)

    Get(apiUri(s"root/${repoId.show}/stuff")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[RootRole] shouldBe a[RootRole]
    }

    Post(apiUri(s"root/${repoId.show}/stuff"), signedPayload) ~> routes ~> check {
      status shouldBe StatusCodes.BadRequest
      responseAs[JsonErrors].head should  include(s"no public key with id ${keyPair.id} found")
    }
  }


  test("XXX POST returns 204 when signature is valid") {
    val repoId = RepoId.generate()
    generateRootRole(repoId).futureValue

    val rootRole = Get(apiUri(s"root/${repoId.show}/stuff")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[RootRole]
    }

    val rootKeyId = rootRole.roles(RoleType.ROOT).keyids.head
    val signedPayload = clientSignPayload(rootKeyId, rootRole, rootRole)

    Post(apiUri(s"root/${repoId.show}/stuff"), signedPayload) ~> routes ~> check {
      status shouldBe StatusCodes.NoContent
    }
  }

  def clientSignPayload[T : Encoder](rootKeyId: KeyId, role: RootRole, payloadToSign: T): SignedPayload[RootRole] = {
    val privateKey = fakeVault.findKey(rootKeyId).futureValue.privateKey
    val rsaKeyPair = RsaKeyPair.parseKeyPair(privateKey).get
    val signature = RsaKeyPair.sign(rsaKeyPair.getPrivate, payloadToSign.asJson.canonical.getBytes)
    val clientSignature = ClientSignature(rootKeyId, signature.method, signature.sig)
    SignedPayload(Seq(clientSignature), role)
  }

  def generateRootRole(repoId: RepoId, threshold: Int = 1): Future[Seq[Key]] = {
    Post(apiUri(s"root/${repoId.show}"), ClientRootGenRequest(threshold = threshold)) ~> routes ~> check {
      status shouldBe StatusCodes.Accepted

      val ids = responseAs[Seq[KeyGenId]]

      Future.sequence {
        ids.map { id =>
          keyGenRepo
            .find(id)
            .flatMap(keyGenerationOp.processGenerationRequest)
        }
      }.map(_.flatten)
    }
  }
}

object JsonErrors {
  implicit val jsonErrorsEncoder: Encoder[JsonErrors] = deriveEncoder
  implicit val jsonErrorsDecoder: Decoder[JsonErrors] = deriveDecoder
}

case class JsonErrors(errors: NonEmptyList[String]) {
  def head: String = errors.head
}