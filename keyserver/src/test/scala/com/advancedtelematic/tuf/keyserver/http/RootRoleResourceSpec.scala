package com.advancedtelematic.tuf.keyserver.http

import java.time.{Duration, Instant}
import java.time.temporal.ChronoUnit

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.RouteTest
import com.advancedtelematic.tuf.util.{KeyTypeSpecSupport, ResourceSpec, RootGenerationSpecSupport, TufKeyserverSpec}
import io.circe.generic.auto._
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import cats.syntax.show._
import com.advancedtelematic.libats.data.ErrorRepresentation
import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.data.TufDataType.{RepoId, TufPrivateKey, _}
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.{Key, KeyGenId, KeyGenRequestStatus, SignedRootRole}
import io.circe.{Encoder, Json}
import org.scalatest.Inspectors
import org.scalatest.concurrent.PatienceConfiguration
import io.circe.syntax._
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.ClientDataType.{RoleKeys, RootRole}
import com.advancedtelematic.libtuf.data.ErrorCodes
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.tuf.keyserver.db.{KeyGenRequestSupport, KeyRepository, KeyRepositorySupport, SignedRootRoleSupport}
import eu.timepit.refined.api.Refined
import org.scalatest.time.{Millis, Seconds, Span}
import com.advancedtelematic.libtuf.data.RootManipulationOps._
import KeyRepository.KeyNotFound
import cats.syntax.either._
import com.advancedtelematic.tuf.keyserver.roles.SignedRootRoles

import scala.async.Async.await
import scala.concurrent.{ExecutionContext, Future}


class RootRoleResourceSpec extends TufKeyserverSpec
  with ResourceSpec
  with KeyGenRequestSupport
  with KeyRepositorySupport
  with RootGenerationSpecSupport
  with PatienceConfiguration
  with HttpResponseTestOps
  with KeyTypeSpecSupport
  with SignedRootRoleSupport
  with Inspectors {

  implicit val ec = ExecutionContext.global

  override implicit def patienceConfig = PatienceConfig(timeout = Span(20, Seconds), interval = Span(500, Millis))

  test("GET returns NotFound if keys do not exist") {
    val repoId = RepoId.generate()

    Get(apiUri(s"root/${repoId.show}")) ~> routes ~> check {
      status shouldBe StatusCodes.NotFound
    }
  }

  test("returns 412 if there are no private keys for given repo/roletype") {
    val repoId = RepoId.generate()

    Post(apiUri(s"root/${repoId.show}/targets"), Json.Null) ~> routes ~> check {
      status shouldBe StatusCodes.PreconditionFailed
      responseAs[ErrorRepresentation].code shouldBe ErrorCodes.KeyServer.PrivateKeysNotFound
    }
  }

  test("GET on private key returns 404 when key does not exist") {
    val repoId = RepoId.generate()
    val keyId: KeyId = Refined.unsafeApply("8a17927d32c40ca87d71e74123b85a4f465d76c2edb0c8e364559bd5fc3d035a")

    Get(apiUri(s"root/${repoId.show}/private_keys/${keyId.value}")) ~> routes ~> check {
      status shouldBe StatusCodes.NotFound
    }
  }

  test("GET target key pairs error on key not found in DB") {
    val repoId = RepoId.generate()

    Get(apiUri(s"root/${repoId.show}/keys/targets/pairs")) ~> routes ~> check {
      status shouldBe StatusCodes.NotFound
      responseAs[ErrorRepresentation].description shouldBe "Missing entity: RootRole"
    }
  }

  keyTypeTest("POST returns Accepted") { keyType =>
    Post(apiUri(s"root/${RepoId.generate().show}"), ClientRootGenRequest(keyType = keyType)) ~> routes ~> check {
      status shouldBe StatusCodes.Accepted
    }
  }

  keyTypeTest("POST creates key gen request for all types of roles") { keyType =>
    val repoId = RepoId.generate()

    Post(apiUri(s"root/${repoId.show}"), ClientRootGenRequest(keyType = keyType)) ~> routes ~> check {
      status shouldBe StatusCodes.Accepted
    }

    val requests = keyGenRepo.findBy(repoId).futureValue

    requests.size shouldBe RoleType.ALL.size

    requests.map(_.status) should contain only KeyGenRequestStatus.REQUESTED
  }

  keyTypeTest("POST creates roles with valid keys") { keyType =>
    val repoId = RepoId.generate()

    Post(apiUri(s"root/${repoId.show}"), ClientRootGenRequest(keyType = keyType)) ~> routes ~> check {
      status shouldBe StatusCodes.Accepted
    }

    processKeyGenerationRequest(repoId).futureValue

    Get(apiUri(s"root/${repoId.show}")) ~> routes ~> check {
      status shouldBe StatusCodes.OK

      val signedPayload = responseAs[SignedPayload[RootRole]]
      val rootRole = signedPayload.signed

      signedPayload.signatures should have size 1 // Signed with root only

      rootRole.keys should have size RoleType.ALL.size

      forAll(rootRole.keys.values) { key ⇒
        key.keytype shouldBe keyType
      }
    }
  }

  keyTypeTest("POST creates keys for all roles") { keyType =>
    val repoId = RepoId.generate()

    Post(apiUri(s"root/${repoId.show}"), ClientRootGenRequest(keyType = keyType)) ~> routes ~> check {
      status shouldBe StatusCodes.Accepted
    }

    val requests = keyGenRepo.findBy(repoId).futureValue

    requests.size shouldBe RoleType.ALL.size

    requests.map(_.roleType) should contain allElementsOf RoleType.ALL
  }

  keyTypeTest("PUT forces a retry on ERROR requests ") { keyType =>
    val repoId = RepoId.generate()

    Post(apiUri(s"root/${repoId.show}"), ClientRootGenRequest(keyType = keyType)) ~> routes ~> check {
      status shouldBe StatusCodes.Accepted
    }

    val requests = keyGenRepo.findBy(repoId).futureValue
    keyGenRepo.setStatusAll(requests.map(_.id), KeyGenRequestStatus.ERROR).futureValue
    keyGenRepo.findBy(repoId).futureValue.map(_.status) should contain only KeyGenRequestStatus.ERROR

    Put(apiUri(s"root/${repoId.show}"), ClientRootGenRequest(keyType = keyType)) ~> routes ~> check {
      status shouldBe StatusCodes.OK
    }

    val updatedRequests = keyGenRepo.findBy(repoId).futureValue
    updatedRequests.map(_.status) should contain only KeyGenRequestStatus.REQUESTED
  }

  keyTypeTest("POST fails if key for role already exists") { keyType =>
    val repoId = RepoId.generate()

    Post(apiUri(s"root/${repoId.show}"), ClientRootGenRequest(keyType = keyType)) ~> routes ~> check {
      status shouldBe StatusCodes.Accepted
    }

    Post(apiUri(s"root/${repoId.show}"), ClientRootGenRequest(keyType = keyType)) ~> routes ~> check {
      status shouldBe StatusCodes.Conflict
    }
  }

  keyTypeTest("GET returns Locked if keys are not ready ") { keyType =>
    val repoId = RepoId.generate()

    Post(apiUri(s"root/${repoId.show}"), ClientRootGenRequest(keyType = keyType)) ~> routes ~> check {
      status shouldBe StatusCodes.Accepted
    }

    Get(apiUri(s"root/${repoId.show}")) ~> routes ~> check {
      status shouldBe StatusCodes.Locked
    }
  }

  keyTypeTest("GET returns 502 if key generation failed ") { keyType =>
    val repoId = RepoId.generate()

    Post(apiUri(s"root/${repoId.show}"), ClientRootGenRequest(keyType = keyType)) ~> routes ~> check {
      status shouldBe StatusCodes.Accepted
    }

    val requests = keyGenRepo.findBy(repoId).futureValue
    val error = new Exception("test: generation failed")
    val keyGenId = requests.head.id
    keyGenRepo.setStatus(keyGenId, KeyGenRequestStatus.ERROR, Option(error)).futureValue

    Get(apiUri(s"root/${repoId.show}")) ~> routes ~> check {
      status shouldBe StatusCodes.InternalServerError
      val errorRepr = responseAs[ErrorRepresentation]
      errorRepr.cause.flatMap(_.as[Map[KeyGenId, String]].toOption).flatMap(_.get(keyGenId)) should contain(s"Exception|test: generation failed")
    }
  }

  keyTypeTest("GET returns 200 if keys are ready ") { keyType =>
    val repoId = RepoId.generate()

    generateRootRole(repoId, keyType).futureValue

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

  keyTypeTest("GET returns 200 with all keys if threshold > 1 ") { keyType =>
    val repoId = RepoId.generate()

    generateRootRole(repoId, keyType, threshold = 4).futureValue

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

  keyTypeTest("POST to repoId/roletype signs any payload with existing keys") { keyType =>
    val repoId = RepoId.generate()

    generateRootRole(repoId, keyType).futureValue

    val toSignPayload = Json.fromFields(List("some" -> Json.fromString("signed stuff")))

    val targetKey = Get(apiUri(s"root/${repoId.show}/keys/targets/pairs")) ~> routes ~> check {
      responseAs[Seq[TufKeyPair]].head
    }

    Post(apiUri(s"root/${repoId.show}/targets"), toSignPayload) ~> routes ~> check {
      status shouldBe StatusCodes.OK

      val signedPayload = responseAs[JsonSignedPayload]

      signedPayload.signatures shouldNot be(empty)

      signedPayload.signed shouldBe toSignPayload

      forAll(signedPayload.signatures) { sig =>
        sig.keyid shouldBe targetKey.pubkey.id

        val isValidT = TufCrypto.isValid(sig, targetKey.pubkey, signedPayload.signed)
        isValidT shouldBe true
      }
    }
  }

  keyTypeTest("POST to repoId/roletype return 412 when key is offline") { keyType =>
    val repoId = RepoId.generate()

    generateRootRole(repoId, keyType).futureValue

    val targetKeyId = Get(apiUri(s"root/${repoId.show}")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[SignedPayload[RootRole]].signed.roles(RoleType.TARGETS).keyids.head
    }

    Delete(apiUri(s"root/${repoId.show}/private_keys/${targetKeyId.value}")) ~> routes ~> check {
      status shouldBe StatusCodes.NoContent
    }

    Post(apiUri(s"root/${repoId.show}/targets"), Json.Null) ~> routes ~> check {
      println(responseAs[Json].noSpaces)

      status shouldBe StatusCodes.PreconditionFailed
    }
  }

  keyTypeTest("DELETE on private_key wipes root private key ") { keyType =>
    val repoId = RepoId.generate()

    generateRootRole(repoId, keyType).futureValue

    val rootKeyId = Get(apiUri(s"root/${repoId.show}")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[SignedPayload[RootRole]].signed.roles(RoleType.ROOT).keyids.head
    }

    Delete(apiUri(s"root/${repoId.show}/private_keys/${rootKeyId.value}")) ~> routes ~> check {
      status shouldBe StatusCodes.NoContent
    }

    keyRepo.find(rootKeyId).failed.futureValue shouldBe KeyRepository.KeyNotFound
  }

  keyTypeTest("DELETE on private_key wipes non root private key ") { keyType =>
    val repoId = RepoId.generate()

    generateRootRole(repoId, keyType).futureValue

    val keyId = Get(apiUri(s"root/${repoId.show}")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[SignedPayload[RootRole]].signed.roles(RoleType.TARGETS).keyids.head
    }

    Delete(apiUri(s"root/${repoId.show}/private_keys/${keyId.value}")) ~> routes ~> check {
      status shouldBe StatusCodes.NoContent
    }

    keyRepo.find(keyId).failed.futureValue shouldBe KeyRepository.KeyNotFound
  }

  keyTypeTest("wiping private key still allows download of root.json ") { keyType =>
    val repoId = RepoId.generate()
    generateRootRole(repoId, keyType).futureValue

    val rootKeyId = Get(apiUri(s"root/${repoId.show}")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[SignedPayload[RootRole]].signed.roles(RoleType.ROOT).keyids.head
    }

    Delete(apiUri(s"root/${repoId.show}/private_keys/${rootKeyId.value}")) ~> routes ~> check {
      status shouldBe StatusCodes.NoContent
    }

    Get(apiUri(s"root/${repoId.show}")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[SignedPayload[RootRole]].signed shouldBe a[RootRole]
    }
  }

  keyTypeTest("GET on an unsigned root returns unsigned root ") { keyType =>
    val repoId = RepoId.generate()
    generateRootRole(repoId, keyType).futureValue

    Get(apiUri(s"root/${repoId.show}/unsigned")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[RootRole] shouldBe a[RootRole]
    }
  }

  keyTypeTest("GET on unsigned root role returns root role with increased version ") { keyType =>
    val repoId = RepoId.generate()
    generateRootRole(repoId, keyType).futureValue

    Get(apiUri(s"root/${repoId.show}")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[SignedPayload[RootRole]].signed.version shouldBe 1
    }

    Get(apiUri(s"root/${repoId.show}/unsigned")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[RootRole].version shouldBe 2
    }
  }

  keyTypeTest("rejects offline signed root when it was signed with invalid encoder") { keyType =>
    val repoId = RepoId.generate()
    generateRootRole(repoId, keyType).futureValue

    val lastRoot = Get(apiUri(s"root/${repoId.show}/unsigned")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[RootRole]
    }

    val lastRootKeyId = lastRoot.roles(RoleType.ROOT).keyids.head
    val lastRootKey = keyRepo.find(lastRootKeyId).futureValue

    val newRootWithField = lastRoot.asJson.mapObject(_.add("some_field", Json.fromString("some_value")))

    val oldSignature = TufCrypto.signPayload(lastRootKey.privateKey, newRootWithField)
    val oldClientSig = ClientSignature(lastRootKeyId, oldSignature.method, oldSignature.sig)
    val payload = JsonSignedPayload(Seq(oldClientSig), newRootWithField)

    Post(apiUri(s"root/${repoId.show}/unsigned"), payload) ~> routes ~> check {
      status shouldBe StatusCodes.BadRequest
      val error = responseAs[ErrorRepresentation]

      error.code shouldBe ErrorCodes.KeyServer.InvalidRootRole
      error.cause.flatMap(_.as[List[String]].toOption) should contain(List("an incompatible encoder was used to encode root.json"))
    }
  }

  keyTypeTest("signatures are valid when root was signed with different format") { keyType =>
    val repoId = RepoId.generate()
    generateRootRole(repoId, keyType).futureValue

    val lastRoot = Get(apiUri(s"root/${repoId.show}/unsigned")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[RootRole]
    }

    val lastRootKeyId = lastRoot.roles(RoleType.ROOT).keyids.head
    val lastRootKey = keyRepo.find(lastRootKeyId).futureValue

    val newRootWithField = lastRoot.asJson.mapObject(_.add("some_field", Json.fromString("some_value")))

    val oldSignature = TufCrypto.signPayload(lastRootKey.privateKey, newRootWithField)
    val oldClientSig = ClientSignature(lastRootKeyId, oldSignature.method, oldSignature.sig)

    val payload = SignedPayload(Seq(oldClientSig), newRootWithField.as[RootRole].valueOr(throw _), newRootWithField)

    val signedRootRole = SignedRootRole(repoId, payload, lastRoot.expires, lastRoot.version)

    signedRootRoleRepo.persist(signedRootRole).futureValue

    Get(apiUri(s"root/${repoId.show}")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      val signedRoot = responseAs[SignedPayload[RootRole]]
      TufCrypto.isValid(signedRoot.signatures.head, lastRootKey.publicKey, signedRoot.json) shouldBe true

      val jsonPayload = responseAs[JsonSignedPayload]
      TufCrypto.isValid(jsonPayload.signatures.head, lastRootKey.publicKey, jsonPayload.signed) shouldBe true
    }
  }


  keyTypeTest("supports multiple offline signed root.json updates") { keyType =>
    val repoId = RepoId.generate()
    generateRootRole(repoId, keyType).futureValue

    def latestRoot() =
      Get(apiUri(s"root/${repoId.show}/unsigned")) ~> routes ~> check {
        status shouldBe StatusCodes.OK
        responseAs[RootRole]
      }

    def signedRoot(lastRoot: RootRole, lastKeyId: KeyId, lastKey: TufPrivateKey) = {
      val keyPair = keyType.crypto.generateKeyPair()

      val newRoot = lastRoot.withRoleKeys(RoleType.ROOT, keyPair.pubkey, lastRoot.keys(lastKeyId))

      val oldSignature = TufCrypto.signPayload(lastKey, newRoot.asJson)
      val oldClientSig = ClientSignature(lastKeyId, oldSignature.method, oldSignature.sig)

      val newSignature = TufCrypto.signPayload(keyPair.privkey, newRoot.asJson)
      val newClientSig = ClientSignature(keyPair.pubkey.id, newSignature.method, newSignature.sig)

      (keyPair, SignedPayload(Seq(oldClientSig, newClientSig), newRoot, newRoot.asJson))
    }

    val lastRoot = latestRoot()
    val lastRootKeyId = lastRoot.roles(RoleType.ROOT).keyids.head
    val lastRootKey = keyRepo.find(lastRootKeyId).futureValue

    val (newKeyPair, signedPayload0) = signedRoot(lastRoot, lastRootKeyId, lastRootKey.privateKey)

    Post(apiUri(s"root/${repoId.show}/unsigned"), signedPayload0) ~> routes ~> check { // TODO: Wrong api ? this is messed up
      status shouldBe StatusCodes.NoContent
    }

    val (_, signedPayload1) = signedRoot(latestRoot(), newKeyPair.pubkey.id, newKeyPair.privkey)

    Post(apiUri(s"root/${repoId.show}/unsigned"), signedPayload1) ~> routes ~> check {
      status shouldBe StatusCodes.NoContent
    }

    Get(apiUri(s"root/${repoId.show}")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[SignedPayload[RootRole]] shouldBe signedPayload1
    }
  }

  keyTypeTest("POST offline signed deletes all keys for role") { keyType =>
    val repoId = RepoId.generate()
    generateRootRole(repoId, keyType).futureValue

    val oldRootRole = Get(apiUri(s"root/${repoId.show}/unsigned")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[RootRole]
    }

    val keyPair = keyType.crypto.generateKeyPair()
    val (newPubKey, newPrivKey) = (keyPair.pubkey, keyPair.privkey)

    val rootKeyId = oldRootRole.roles(RoleType.ROOT).keyids.head
    val rootRole = oldRootRole.withRoleKeys(RoleType.ROOT, newPubKey)

    val newSignature = signWithKeyPair(newPubKey.id, newPrivKey, rootRole)
    val oldSignedPayload = signPayloadWithKey(rootKeyId, rootRole)
    val signedPayload = oldSignedPayload.copy(signatures = newSignature +: oldSignedPayload.signatures)

    Get(apiUri(s"root/${repoId.show}/keys/targets/pairs")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      val keyPairs = responseAs[Seq[TufKeyPair]]
      keyPairs shouldNot be(empty)
    }

    Post(apiUri(s"root/${repoId.show}/unsigned"), signedPayload) ~> routes ~> check {
      status shouldBe StatusCodes.NoContent
    }

    Get(apiUri(s"root/${repoId.show}/keys/targets/pairs")) ~> routes ~> check {
      status shouldBe StatusCodes.NotFound
      responseAs[ErrorRepresentation].code.code shouldBe "missing_entity"
    }
  }

  keyTypeTest("POST offline with same version returns bad request ") { keyType =>
    val repoId = RepoId.generate()
    generateRootRole(repoId, keyType).futureValue

    val rootRole = Get(apiUri(s"root/${repoId.show}/unsigned")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[RootRole]
    }

    val sameVersionRoot = rootRole.copy(version = rootRole.version - 1)
    val rootKeyId = sameVersionRoot.roles(RoleType.ROOT).keyids.head
    val signedPayload = signPayloadWithKey(rootKeyId, sameVersionRoot)

    Post(apiUri(s"root/${repoId.show}/unsigned"), signedPayload) ~> routes ~> check {
      responseErrors should contain("Invalid version bump from 1 to 1")
      status shouldBe StatusCodes.BadRequest
    }
  }

  keyTypeTest("POST returns 4xx when signature is not valid") { keyType =>
    val repoId = RepoId.generate()
    generateRootRole(repoId, keyType).futureValue

    val rootRole = Get(apiUri(s"root/${repoId.show}/unsigned")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[RootRole]
    }

    val rootKeyId = rootRole.roles(RoleType.ROOT).keyids.head
    val clientSignature = clientSignWithKey(rootKeyId, "not a root role")
    val signedPayload = JsonSignedPayload(Seq(clientSignature), rootRole.asJson)

    Post(apiUri(s"root/${repoId.show}/unsigned"), signedPayload) ~> routes ~> check {
      status shouldBe StatusCodes.BadRequest
      responseErrors should contain(s"Invalid signature for key $rootKeyId in root.json version 2")
    }
  }

  keyTypeTest("POST returns error payload is not signed with all keys ") { keyType =>
    val repoId = RepoId.generate()
    generateRootRole(repoId, keyType).futureValue

    val rootRole = Get(apiUri(s"root/${repoId.show}/unsigned")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[RootRole]
    }

    val signedPayload = JsonSignedPayload(Seq.empty, rootRole.asJson)

    Post(apiUri(s"root/${repoId.show}/unsigned"), signedPayload) ~> routes ~> check {
      status shouldBe StatusCodes.BadRequest
      responseErrors should contain("Root role version 1 requires 1 valid signatures in version 2, 0 supplied")
      responseErrors should contain("Root role version 2 requires 1 valid signatures in version 2, 0 supplied")
    }
  }

  keyTypeTest("POST offline signed returns 204 when signature is valid ") { keyType =>
    val repoId = RepoId.generate()
    generateRootRole(repoId, keyType).futureValue

    val rootRole = Get(apiUri(s"root/${repoId.show}/unsigned")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[RootRole]
    }

    val rootKeyId = rootRole.roles(RoleType.ROOT).keyids.head
    val signedPayload = signPayloadWithKey(rootKeyId, rootRole)

    Post(apiUri(s"root/${repoId.show}/unsigned"), signedPayload) ~> routes ~> check {
      status shouldBe StatusCodes.NoContent
    }
  }

  keyTypeTest("GET with version after POST offline signed returns old root ") { keyType =>
    val repoId = RepoId.generate()
    generateRootRole(repoId, keyType).futureValue

    val oldRootRole = Get(apiUri(s"root/${repoId.show}")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[SignedPayload[RootRole]]
    }

    val newUnsignedRole = Get(apiUri(s"root/${repoId.show}/unsigned")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[RootRole]
    }

    val rootKeyId = newUnsignedRole.roles(RoleType.ROOT).keyids.head
    val signedPayload = signPayloadWithKey(rootKeyId, newUnsignedRole)

    Post(apiUri(s"root/${repoId.show}/unsigned"), signedPayload) ~> routes ~> check {
      status shouldBe StatusCodes.NoContent
    }

    val rootRoleV1 = Get(apiUri(s"root/${repoId.show}/1")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[SignedPayload[RootRole]]
    }

    oldRootRole.asJson shouldBe rootRoleV1.asJson
  }

  keyTypeTest("POST offline signed updates keys when signature is valid ") { keyType =>
    val repoId = RepoId.generate()
    generateRootRole(repoId, keyType).futureValue

    val oldRootRole = Get(apiUri(s"root/${repoId.show}/unsigned")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[RootRole]
    }

    val keyPair = keyType.crypto.generateKeyPair()

    val rootKeyId = oldRootRole.roles(RoleType.ROOT).keyids.head
    val rootRole = oldRootRole.withRoleKeys(RoleType.ROOT, keyPair.pubkey)

    val newSignature = signWithKeyPair(keyPair.pubkey.id, keyPair.privkey, rootRole)
    val oldSignedPayload = signPayloadWithKey(rootKeyId, rootRole)
    val signedPayload = oldSignedPayload.copy(signatures = newSignature +: oldSignedPayload.signatures)

    Post(apiUri(s"root/${repoId.show}/unsigned"), signedPayload) ~> routes ~> check {
      status shouldBe StatusCodes.NoContent
    }

    Get(apiUri(s"root/${repoId.show}/unsigned")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      val resp = responseAs[RootRole]
      rootRole.copy(version = rootRole.version + 1, expires = resp.expires) shouldBe resp
    }
  }

  keyTypeTest("POST offline signed returns 400 when not signed with new ") { keyType =>
    val repoId = RepoId.generate()
    generateRootRole(repoId, keyType).futureValue

    val oldRootRole = Get(apiUri(s"root/${repoId.show}/unsigned")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[RootRole]
    }

    val newKey = keyType.crypto.generateKeyPair().pubkey

    val rootKeyId = oldRootRole.roles(RoleType.ROOT).keyids.head
    val newKeys = (oldRootRole.keys - rootKeyId) + (newKey.id -> newKey)
    val newRoles = (oldRootRole.roles - RoleType.ROOT) + (RoleType.ROOT -> RoleKeys(Seq(newKey.id), 1))
    val rootRole = oldRootRole.copy(keys = newKeys, roles = newRoles)

    val signedPayload = signPayloadWithKey(rootKeyId, rootRole)

    Post(apiUri(s"root/${repoId.show}/unsigned"), signedPayload) ~> routes ~> check {
      status shouldBe StatusCodes.BadRequest
      responseErrors should contain("Root role version 2 requires 1 valid signatures in version 2, 0 supplied")
    }
  }

  keyTypeTest("POST offline signed returns 400 when not signed with old ") { keyType =>
    val repoId = RepoId.generate()
    generateRootRole(repoId, keyType).futureValue

    val oldRootRole = Get(apiUri(s"root/${repoId.show}/unsigned")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[RootRole]
    }

    val keyPair = keyType.crypto.generateKeyPair(keyType.crypto.defaultKeySize)

    val rootKeyId = oldRootRole.roles(RoleType.ROOT).keyids.head
    val rootRole = oldRootRole.withRoleKeys(RoleType.ROOT, keyPair.pubkey)

    val signedPayload = JsonSignedPayload(List(signWithKeyPair(keyPair.pubkey.id, keyPair.privkey, rootRole)), rootRole.asJson)

    Post(apiUri(s"root/${repoId.show}/unsigned"), signedPayload) ~> routes ~> check {
      status shouldBe StatusCodes.BadRequest
      responseErrors should contain("Root role version 1 requires 1 valid signatures in version 2, 0 supplied")
    }
  }

  keyTypeTest("GET target key pairs ") { keyType =>
    val repoId = RepoId.generate()
    val publicKeys = generateRootRole(repoId, keyType).futureValue.filter(_.roleType == RoleType.TARGETS)

    Get(apiUri(s"root/${repoId.show}/keys/targets/pairs")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[Seq[TufKeyPair]].map(_.pubkey).toSet shouldBe publicKeys.map(_.publicKey).toSet
    }
  }

  keyTypeTest("GET target key pairs error when private keys do not exist") { keyType =>
    val repoId = RepoId.generate()
    val keys = generateRootRole(repoId, keyType).futureValue

    Future.sequence(keys.map(_.id).map(keyRepo.delete)).futureValue

    Get(apiUri(s"root/${repoId.show}/keys/targets/pairs")) ~> routes ~> check {
      responseAs[ErrorRepresentation].code.code shouldBe "missing_entity"
      status shouldBe StatusCodes.NotFound
    }
  }

  keyTypeTest("GET returns renewed root if old one expired ") { keyType =>
    val repoId = RepoId.generate()

    Post(apiUri(s"root/${repoId.show}"), ClientRootGenRequest(1, keyType)) ~> routes ~> check {
      status shouldBe StatusCodes.Accepted
      processKeyGenerationRequest(repoId).futureValue
    }

    val signedRootRoles = new SignedRootRoles(defaultRoleExpire = Duration.ofMillis(1))

    signedRootRoles.findFreshAndPersist(repoId).futureValue

    Get(apiUri(s"root/${repoId.show}")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      val signed = responseAs[SignedPayload[RootRole]].signed
      signed.version shouldBe 3
      signed.expires.isAfter(Instant.now.plus(30, ChronoUnit.DAYS)) shouldBe true
    }
  }

  keyTypeTest("GET returns 412 if root is expired and keys are offline") { keyType =>
    val repoId = RepoId.generate()

    Post(apiUri(s"root/${repoId.show}"), ClientRootGenRequest(1, keyType)) ~> routes ~> check {
      status shouldBe StatusCodes.Accepted
      processKeyGenerationRequest(repoId).futureValue
    }

    val signedRootRoles = new SignedRootRoles(defaultRoleExpire = Duration.ofMillis(1))

    val role = signedRootRoles.findFreshAndPersist(repoId).futureValue

    val keyIds = role.signed.roleKeys(RoleType.ROOT).map(_.id)
    Future.sequence(keyIds.map(keyRepo.delete)).futureValue

    Get(apiUri(s"root/${repoId.show}")) ~> routes ~> check {
      status shouldBe StatusCodes.PreconditionFailed
      responseAs[ErrorRepresentation].code shouldBe Errors.PrivateKeysNotFound.code
    }
  }

  test("keeps snapshot and timestamp keys online when storing user persisted root role ") {
    val repoId = RepoId.generate()
    generateRootRole(repoId, Ed25519KeyType).futureValue

    val rootRole = Get(apiUri(s"root/${repoId.show}/unsigned")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[RootRole]
    }

    val rootKeyId = rootRole.roles(RoleType.ROOT).keyids.head
    val newSignature = clientSignWithKey(rootKeyId, rootRole)
    val oldSignedPayload = signPayloadWithKey(rootKeyId, rootRole)
    val signedPayload = oldSignedPayload.copy(signatures = newSignature +: oldSignedPayload.signatures)

    Post(apiUri(s"root/${repoId.show}/unsigned"), signedPayload) ~> routes ~> check {
      status shouldBe StatusCodes.NoContent
    }

    val newRoot = signedPayload.signed.as[RootRole].right.get

    val snapshotKeyId = newRoot.roleKeys(RoleType.SNAPSHOT).head.id
    keyRepo.find(snapshotKeyId).futureValue shouldBe a[Key]

    val timestampKeyId = newRoot.roleKeys(RoleType.TIMESTAMP).head.id
    keyRepo.find(timestampKeyId).futureValue shouldBe a[Key]

    val targetsKeyId = newRoot.roleKeys(RoleType.TARGETS).head.id
    keyRepo.find(targetsKeyId).failed.futureValue shouldBe KeyNotFound

    val newRootKeyId = newRoot.roleKeys(RoleType.ROOT).head.id
    keyRepo.find(newRootKeyId).failed.futureValue shouldBe KeyNotFound
  }

  def signWithKeyPair(keyId: KeyId, priv: TufPrivateKey, role: RootRole): ClientSignature = {
    val signature = TufCrypto.signPayload(priv, role.asJson)
    ClientSignature(keyId, signature.method, signature.sig)
  }

  def clientSignWithKey[T: Encoder](keyId: KeyId, payload: T): ClientSignature = {
    val key = keyRepo.find(keyId).futureValue
    val signature = TufCrypto.signPayload(key.privateKey, payload.asJson)
    ClientSignature(keyId, signature.method, signature.sig)
  }

  def signPayloadWithKey[T : Encoder](keyId: KeyId, payloadToSign: T): JsonSignedPayload = {
    val clientSignature = clientSignWithKey(keyId, payloadToSign)
    JsonSignedPayload(Seq(clientSignature), payloadToSign.asJson)
  }

  def generateRootRole(repoId: RepoId, keyType: KeyType, threshold: Int = 1): Future[Seq[Key]] = {
    val keysF = Post(apiUri(s"root/${repoId.show}"), ClientRootGenRequest(threshold, keyType)) ~> routes ~> check {
      status shouldBe StatusCodes.Accepted

      responseAs[Seq[KeyGenId]] shouldNot be(empty)

      processKeyGenerationRequest(repoId)
    }

    keysF.map { keys ⇒
      Get(apiUri(s"root/${repoId.show}")) ~> routes ~> check {
        status shouldBe StatusCodes.OK
        responseAs[SignedPayload[RootRole]]
      }

      keys
    }
  }
}

trait HttpResponseTestOps {
  self: RouteTest =>

    def responseErrors: List[String] =
      responseAs[ErrorRepresentation].cause.flatMap(_.as[List[String]].toOption).getOrElse(List.empty)
}
