package com.advancedtelematic.tuf.keyserver.http

import java.security.PrivateKey

import cats.syntax.either._
import akka.http.scaladsl.model.StatusCodes
import cats.data.NonEmptyList
import com.advancedtelematic.tuf.util.{ResourceSpec, RootGenerationSpecSupport, TufKeyserverSpec}
import io.circe.generic.auto._
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import cats.syntax.show._
import com.advancedtelematic.libats.data.ErrorRepresentation
import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.data.TufDataType._
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.{Key, KeyGenId, KeyGenRequest, KeyGenRequestStatus}
import com.advancedtelematic.libtuf.data.TufDataType.RepoId
import io.circe.{Decoder, Encoder, Json}
import org.scalatest.Inspectors
import org.scalatest.concurrent.PatienceConfiguration
import io.circe.syntax._
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.TufDataType.{RSATufPrivateKey, TufPrivateKey}
import com.advancedtelematic.libtuf.data.ClientDataType.{RoleKeys, RootRole}
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.tuf.keyserver.daemon.{DefaultKeyGenerationOp, KeyGenerationOp}
import com.advancedtelematic.tuf.keyserver.db.{KeyGenRequestSupport, KeyRepositorySupport, RoleRepositorySupport}
import com.advancedtelematic.tuf.keyserver.vault.VaultClient.VaultResourceNotFound
import eu.timepit.refined.api.Refined
import org.scalatest.time.{Millis, Seconds, Span}
import io.circe.generic.semiauto._

import scala.concurrent.{ExecutionContext, Future}

class RootRoleResourceSpec extends TufKeyserverSpec
  with ResourceSpec
  with KeyGenRequestSupport
  with KeyRepositorySupport
  with RoleRepositorySupport
  with RootGenerationSpecSupport
  with PatienceConfiguration
  with Inspectors {

  implicit val ec = ExecutionContext.global

  override implicit def patienceConfig = PatienceConfig(timeout = Span(20, Seconds), interval = Span(500, Millis))

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

  test("POST creates keys for all roles") {
    val repoId = RepoId.generate()

    Post(apiUri(s"root/${repoId.show}"), ClientRootGenRequest()) ~> routes ~> check {
      status shouldBe StatusCodes.Accepted
    }

    val requests = keyGenRepo.findBy(repoId).futureValue

    requests.size shouldBe RoleType.ALL.size

    requests.map(_.roleType) should contain allElementsOf RoleType.ALL
  }

  test("POST creates roles with ED25519 keys if requested") {
    val repoId = RepoId.generate()

    Post(apiUri(s"root/${repoId.show}"), ClientRootGenRequest(keyType = Ec25519KeyType)) ~> routes ~> check {
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
        key shouldBe a[Ec25519TufKey]
      }
    }
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

  test("GET returns 502 if key generation failed") {
    val repoId = RepoId.generate()

    Post(apiUri(s"root/${repoId.show}"), ClientRootGenRequest()) ~> routes ~> check {
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

      val targetKey = keyRepo.repoKeysForRole(repoId, RoleType.TARGETS).futureValue.head

      signedPayload.signed shouldBe toSignPayload

      forAll(signedPayload.signatures) { sig =>
        sig.keyid shouldBe targetKey.id

        val isValidT = TufCrypto.isValid(sig, targetKey.publicKey, signedPayload.signed)
        isValidT shouldBe true
      }
    }
  }

  test("POST to repoId/roletype return 412 when key is offline") {
    val repoId = RepoId.generate()

    generateRootRole(repoId).futureValue

    val targetKeyId = Get(apiUri(s"root/${repoId.show}")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[SignedPayload[RootRole]].signed.roles(RoleType.TARGETS).keyids.head
    }

    Delete(apiUri(s"root/${repoId.show}/private_keys/${targetKeyId.value}")) ~> routes ~> check {
      status shouldBe StatusCodes.NoContent
    }

    Post(apiUri(s"root/${repoId.show}/targets"), Json.Null) ~> routes ~> check {
      status shouldBe StatusCodes.PreconditionFailed
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

  test("DELETE on private_key wipes root private key") {
    val repoId = RepoId.generate()

    generateRootRole(repoId).futureValue

    val rootKeyId = Get(apiUri(s"root/${repoId.show}")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[SignedPayload[RootRole]].signed.roles(RoleType.ROOT).keyids.head
    }

    Delete(apiUri(s"root/${repoId.show}/private_keys/${rootKeyId.value}")) ~> routes ~> check {
      status shouldBe StatusCodes.NoContent
    }

    fakeVault.findKey(rootKeyId).failed.futureValue shouldBe VaultResourceNotFound
  }

  test("DELETE on private_key wipes non root private key") {
    val repoId = RepoId.generate()

    generateRootRole(repoId).futureValue

    val keyId = Get(apiUri(s"root/${repoId.show}")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[SignedPayload[RootRole]].signed.roles(RoleType.TARGETS).keyids.head
    }

    Delete(apiUri(s"root/${repoId.show}/private_keys/${keyId.value}")) ~> routes ~> check {
      status shouldBe StatusCodes.NoContent
    }

    fakeVault.findKey(keyId).failed.futureValue shouldBe VaultResourceNotFound
  }

  test("wiping private key still allows download of root.json") {
    val repoId = RepoId.generate()
    generateRootRole(repoId).futureValue

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

  test("GET on an unsigned root returns unsigned root") {
    val repoId = RepoId.generate()
    generateRootRole(repoId).futureValue

    Get(apiUri(s"root/${repoId.show}/unsigned")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[RootRole] shouldBe a[RootRole]
    }
  }

  test("GET on unsigned root role returns root role with increased version") {
    val repoId = RepoId.generate()
    generateRootRole(repoId).futureValue

    Get(apiUri(s"root/${repoId.show}")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[SignedPayload[RootRole]].signed.version shouldBe 1
    }

    Get(apiUri(s"root/${repoId.show}/unsigned")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[RootRole].version shouldBe 2
    }
  }

  test("POST returns 4xx when signature is not valid") {
    val repoId = RepoId.generate()
    generateRootRole(repoId).futureValue

    val rootRole = Get(apiUri(s"root/${repoId.show}/unsigned")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[RootRole]
    }

    val rootKeyId = rootRole.roles(RoleType.ROOT).keyids.head
    val signedPayload = clientSignPayload(rootKeyId, rootRole, "sign something else")

    Post(apiUri(s"root/${repoId.show}/unsigned"), signedPayload) ~> routes ~> check {
      status shouldBe StatusCodes.BadRequest
      responseAs[JsonErrors].head should include(s"Invalid signature for key $rootKeyId")
    }
  }

  test("POST returns error payload is not signed with all keys") {
    val repoId = RepoId.generate()
    generateRootRole(repoId).futureValue

    val rootRole = Get(apiUri(s"root/${repoId.show}/unsigned")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[RootRole]
    }

    val signedPayload = SignedPayload(Seq.empty, rootRole)

    Post(apiUri(s"root/${repoId.show}/unsigned"), signedPayload) ~> routes ~> check {
      status shouldBe StatusCodes.BadRequest
      responseAs[JsonErrors].errors.toList should contain("Only 0 signatures present from previous root, need at least 1")
      responseAs[JsonErrors].errors.toList should contain("Only 0 signatures present from new root, need at least 1")
    }
  }

  test("POST offline signed returns 204 when signature is valid") {
    val repoId = RepoId.generate()
    generateRootRole(repoId).futureValue

    val rootRole = Get(apiUri(s"root/${repoId.show}/unsigned")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[RootRole]
    }

    val rootKeyId = rootRole.roles(RoleType.ROOT).keyids.head
    val signedPayload = clientSignPayload(rootKeyId, rootRole, rootRole)

    Post(apiUri(s"root/${repoId.show}/unsigned"), signedPayload) ~> routes ~> check {
      status shouldBe StatusCodes.NoContent
    }
  }

  test("POST offline with same version returns Conflict") {
    val repoId = RepoId.generate()
    generateRootRole(repoId).futureValue

    val rootRole = Get(apiUri(s"root/${repoId.show}/unsigned")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[RootRole]
    }

    val sameVersionRoot = rootRole.copy(version = rootRole.version - 1)
    val rootKeyId = sameVersionRoot.roles(RoleType.ROOT).keyids.head
    val signedPayload = clientSignPayload(rootKeyId, sameVersionRoot, sameVersionRoot)

    Post(apiUri(s"root/${repoId.show}/unsigned"), signedPayload) ~> routes ~> check {
      status shouldBe StatusCodes.Conflict
    }
  }

  test("GET with version after POST offline signed returns old root") {
    val repoId = RepoId.generate()
    generateRootRole(repoId).futureValue

    val oldRootRole = Get(apiUri(s"root/${repoId.show}")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[SignedPayload[RootRole]]
    }

    val newUnsignedRole = Get(apiUri(s"root/${repoId.show}/unsigned")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[RootRole]
    }

    val rootKeyId = newUnsignedRole.roles(RoleType.ROOT).keyids.head
    val signedPayload = clientSignPayload(rootKeyId, newUnsignedRole, newUnsignedRole)

    Post(apiUri(s"root/${repoId.show}/unsigned"), signedPayload) ~> routes ~> check {
      status shouldBe StatusCodes.NoContent
    }

    val rootRoleV1 = Get(apiUri(s"root/${repoId.show}/1")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[SignedPayload[RootRole]]
    }

    oldRootRole.asJson shouldBe rootRoleV1.asJson
  }

  test("POST offline signed updates keys when signature is valid") {
    val repoId = RepoId.generate()
    generateRootRole(repoId).futureValue

    val oldRootRole = Get(apiUri(s"root/${repoId.show}/unsigned")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[RootRole]
    }

    val Ec25519TufKeyPair(newPubKey, newPrivKey) = TufCrypto.generateKeyPair(Ec25519KeyType, 256)

    val rootKeyId = oldRootRole.roles(RoleType.ROOT).keyids.head
    val rootRole = oldRootRole.withRoleKeys(RoleType.ROOT, newPubKey)

    val newSignature = signWithKeyPair(newPubKey.id, newPrivKey, rootRole)
    val oldsignedPayload = clientSignPayload(rootKeyId, rootRole, rootRole)
    val signedPayload = oldsignedPayload.copy(signatures = newSignature +: oldsignedPayload.signatures)

    Post(apiUri(s"root/${repoId.show}/unsigned"), signedPayload) ~> routes ~> check {
      status shouldBe StatusCodes.NoContent
    }

    Get(apiUri(s"root/${repoId.show}/unsigned")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      val resp = responseAs[RootRole]

      rootRole.copy(version = rootRole.version + 1, expires = resp.expires) shouldBe resp
    }
  }

  test("POST offline signed updates threshold when signature is valid") {
    val repoId = RepoId.generate()
    generateRootRole(repoId).futureValue

    val oldRootRole = Get(apiUri(s"root/${repoId.show}/unsigned")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[RootRole]
    }

    val Ec25519TufKeyPair(newPubKey, newPrivKey) = TufCrypto.generateKeyPair(Ec25519KeyType, 256)
    val Ec25519TufKeyPair(newPubKey2, newPrivKey2) = TufCrypto.generateKeyPair(Ec25519KeyType, 256)

    val newRootKeys = List(newPubKey, newPubKey2).map(k => k.id -> k).toMap
    val rootKeyId = oldRootRole.roles(RoleType.ROOT).keyids.head
    val newKeys = (oldRootRole.keys - rootKeyId) ++ newRootKeys
    val newRoles = oldRootRole.roles + (RoleType.ROOT -> RoleKeys(newRootKeys.keys.toSeq, 2))
    val rootRole = oldRootRole.copy(keys = newKeys, roles = newRoles)


    val newSignature1 = signWithKeyPair(newPubKey.id, newPrivKey, rootRole)
    val newSignature2 = signWithKeyPair(newPubKey2.id, newPrivKey2, rootRole)
    val oldsignedPayload = clientSignPayload(rootKeyId, rootRole, rootRole)
    val signedPayload = oldsignedPayload.copy(signatures = List(newSignature1, newSignature2) ++ oldsignedPayload.signatures)

    Post(apiUri(s"root/${repoId.show}/unsigned"), signedPayload) ~> routes ~> check {
      status shouldBe StatusCodes.NoContent
    }

    roleRepo.find(repoId, RoleType.ROOT).futureValue.threshold shouldBe 2
  }

  test("POST offline signed returns 400 when not signed with new") {
    val repoId = RepoId.generate()
    generateRootRole(repoId).futureValue

    val oldRootRole = Get(apiUri(s"root/${repoId.show}/unsigned")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[RootRole]
    }
    val Ec25519TufKeyPair(newKey, _) = TufCrypto.generateKeyPair(Ec25519KeyType, 256)

    val rootKeyId = oldRootRole.roles(RoleType.ROOT).keyids.head
    val newKeys = (oldRootRole.keys - rootKeyId) + (newKey.id -> newKey)
    val newRoles = (oldRootRole.roles - RoleType.ROOT) + (RoleType.ROOT -> RoleKeys(Seq(newKey.id), 1))
    val rootRole = oldRootRole.copy(keys = newKeys, roles = newRoles)

    val signedPayload = clientSignPayload(rootKeyId, rootRole, rootRole)

    Post(apiUri(s"root/${repoId.show}/unsigned"), signedPayload) ~> routes ~> check {
      status shouldBe StatusCodes.BadRequest
      responseAs[JsonErrors].errors.toList should contain("Only 0 signatures present from new root, need at least 1")
    }
  }

  test("POST offline signed returns 400 when not signed with old") {
    val repoId = RepoId.generate()
    generateRootRole(repoId).futureValue

    val oldRootRole = Get(apiUri(s"root/${repoId.show}/unsigned")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[RootRole]
    }
    val Ec25519TufKeyPair(newPubKey, newPrivKey) = TufCrypto.generateKeyPair(Ec25519KeyType, 256)

    val rootKeyId = oldRootRole.roles(RoleType.ROOT).keyids.head
    val rootRole = oldRootRole.withRoleKeys(RoleType.ROOT, newPubKey)

    val signedPayload = SignedPayload(List(signWithKeyPair(newPubKey.id, newPrivKey, rootRole)), rootRole)

    Post(apiUri(s"root/${repoId.show}/unsigned"), signedPayload) ~> routes ~> check {
      status shouldBe StatusCodes.BadRequest
      responseAs[JsonErrors].head should include (s"Only 0 signatures present from previous root, need at least 1")
    }
  }

  test("POST  offline signedreturns 204 when signature is a valid ed25519 signature") {
    val repoId = RepoId.generate()
    generateRootRole(repoId, keyType = Ec25519KeyType).futureValue

    val rootRole = Get(apiUri(s"root/${repoId.show}/unsigned")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[RootRole]
    }

    val rootKeyId = rootRole.roles(RoleType.ROOT).keyids.head
    val signedPayload = clientSignPayload(rootKeyId, rootRole, rootRole)

    Post(apiUri(s"root/${repoId.show}/unsigned"), signedPayload) ~> routes ~> check {
      status shouldBe StatusCodes.NoContent
    }
  }

  test("POST offline signed returns 400 when signature is not a valid ed25519 signature") {
    val repoId = RepoId.generate()
    generateRootRole(repoId, keyType = Ec25519KeyType).futureValue

    val rootRole = Get(apiUri(s"root/${repoId.show}/unsigned")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[RootRole]
    }

    val rootKeyId = rootRole.roles(RoleType.ROOT).keyids.head
    val signedPayload = clientSignPayload(rootKeyId, rootRole, "not the same payload")

    Post(apiUri(s"root/${repoId.show}/unsigned"), signedPayload) ~> routes ~> check {
      status shouldBe StatusCodes.BadRequest
    }
  }

  test("POST to keys/targets adds new public key to root role") {
    val repoId = RepoId.generate()
    generateRootRole(repoId).futureValue

    val publicKey = TufCrypto.generateKeyPair(Ec25519KeyType, 256).pubkey

    val newRootRole = Put(apiUri(s"root/${repoId.show}/keys/targets"), publicKey) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      val rootRole = responseAs[SignedPayload[RootRole]].signed

      val targetKeys = rootRole.roles(RoleType.TARGETS)
      targetKeys.keyids should contain(publicKey.id)

      rootRole.keys(publicKey.id) shouldBe publicKey

      rootRole
    }

    Get(apiUri(s"root/${repoId.show}")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[SignedPayload[RootRole]].signed shouldBe newRootRole
    }
  }

  test("GET target key pairs") {
    val keyGenerationOp = DefaultKeyGenerationOp(fakeVault)

    val repoId = RepoId.generate()

    // generate target key pair
    val targetKeyGenRequest = KeyGenRequest(KeyGenId.generate(),
      repoId, KeyGenRequestStatus.REQUESTED, RoleType.TARGETS, 2048, RsaKeyType)
    keyGenRepo.persist(targetKeyGenRequest).futureValue
    val publicKeys = keyGenerationOp(targetKeyGenRequest).futureValue

    Get(apiUri(s"root/${repoId.show}/keys/targets/pairs")) ~> routes ~> check {
      status shouldBe StatusCodes.OK
      responseAs[Seq[TufKeyPair]].map(_.pubkey).toSet shouldBe publicKeys.map(_.toTufKey).toSet
    }
  }

  def signWithKeyPair(keyId: KeyId, priv: TufPrivateKey, role: RootRole): ClientSignature = {
    val signature = TufCrypto.signPayload(priv, role)
    ClientSignature(keyId, signature.method, signature.sig)
  }

  def clientSignPayload[T : Encoder](rootKeyId: KeyId, role: RootRole, payloadToSign: T): SignedPayload[RootRole] = {
    val vaultKey = fakeVault.findKey(rootKeyId).futureValue
    val signature = TufCrypto.signPayload(vaultKey.privateKey, payloadToSign)
    val clientSignature = ClientSignature(rootKeyId, signature.method, signature.sig)
    SignedPayload(Seq(clientSignature), role)
  }

  def generateRootRole(repoId: RepoId, threshold: Int = 1, keyType: KeyType = RsaKeyType): Future[Seq[Key]] = {
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

  implicit class RootRoleOps(value: RootRole) {
    def withRoleKeys(roleType: RoleType, keys: TufKey*): RootRole = {
      val oldRoleKeys = value.roles(roleType)
      val newRoles = value.roles + (roleType -> oldRoleKeys.copy(keyids = keys.map(_.id)))

      val newKeys = newRoles.values.flatMap(_.keyids).toSet[KeyId].map { keyid =>
        value.keys.get(keyid).orElse(keys.find(_.id == keyid)).get
      }.map(k => k.id -> k).toMap

      value.copy(keys = newKeys, roles = newRoles)
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
