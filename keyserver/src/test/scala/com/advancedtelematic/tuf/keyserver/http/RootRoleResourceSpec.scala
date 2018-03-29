package com.advancedtelematic.tuf.keyserver.http

import cats.syntax.either._
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.RouteTest
import com.advancedtelematic.tuf.util.{ResourceSpec, RootGenerationSpecSupport, TufKeyserverSpec}
import io.circe.generic.auto._
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import cats.syntax.show._
import com.advancedtelematic.libats.data.ErrorRepresentation
import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.data.TufDataType.{Ed25519KeyType, RepoId, TufPrivateKey, _}
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.{Key, KeyGenId, KeyGenRequestStatus}
import io.circe.{Encoder, Json}
import org.scalatest.Inspectors
import org.scalatest.concurrent.PatienceConfiguration
import io.circe.syntax._
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.ClientDataType.{RoleKeys, RootRole}
import com.advancedtelematic.libtuf.data.ErrorCodes
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.tuf.keyserver.db.{KeyGenRequestSupport, KeyRepositorySupport}
import com.advancedtelematic.tuf.keyserver.vault.VaultClient.VaultResourceNotFound
import eu.timepit.refined.api.Refined
import org.scalatest.time.{Millis, Seconds, Span}
import io.circe.generic.semiauto._
import com.advancedtelematic.libtuf.data.RootManipulationOps._
import scala.reflect.ClassTag
import scala.concurrent.{ExecutionContext, Future}

class RootRoleResourceSpec extends TufKeyserverSpec
  with ResourceSpec
  with KeyGenRequestSupport
  with KeyRepositorySupport
  with RootGenerationSpecSupport
  with PatienceConfiguration
  with HttpResponseTestOps
  with Inspectors {

  implicit val ec = ExecutionContext.global

  override implicit def patienceConfig = PatienceConfig(timeout = Span(20, Seconds), interval = Span(500, Millis))

  test("GET returns NotFound if keys do not exist") {
    val repoId = RepoId.generate()

    Get(apiUri(s"root/${repoId.show}")) ~> routes ~> check {
      status shouldBe StatusCodes.NotFound
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

  test("GET target key pairs error on key not found in DB") {
    val repoId = RepoId.generate()

    Get(apiUri(s"root/${repoId.show}/keys/targets/pairs")) ~> routes ~> check {
      status shouldBe StatusCodes.NotFound
      responseAs[ErrorRepresentation].description shouldBe "Missing entity: RootRole"
    }
  }

  def keySpecific[T <: KeyType, P <: T#Pub: ClassTag](keyType: KeyType, name: String): Unit = {
    test("POST returns Accepted " + name) {
      Post(apiUri(s"root/${RepoId.generate().show}"), ClientRootGenRequest(keyType = keyType)) ~> routes ~> check {
        status shouldBe StatusCodes.Accepted
      }
    }

    test("POST creates key gen request for all types of roles " + name) {
      val repoId = RepoId.generate()

      Post(apiUri(s"root/${repoId.show}"), ClientRootGenRequest(keyType = keyType)) ~> routes ~> check {
        status shouldBe StatusCodes.Accepted
      }

      val requests = keyGenRepo.findBy(repoId).futureValue

      requests.size shouldBe RoleType.ALL.size

      requests.map(_.status) should contain only KeyGenRequestStatus.REQUESTED
    }

    test("POST creates keys for all roles " + name) {
      val repoId = RepoId.generate()

      Post(apiUri(s"root/${repoId.show}"), ClientRootGenRequest(keyType = keyType)) ~> routes ~> check {
        status shouldBe StatusCodes.Accepted
      }

      val requests = keyGenRepo.findBy(repoId).futureValue

      requests.size shouldBe RoleType.ALL.size

      requests.map(_.roleType) should contain allElementsOf RoleType.ALL
    }

    test("POST creates roles with valid keys " + name) {
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
          key shouldBe a[P]
        }
      }
    }

    test("PUT forces a retry on ERROR requests " + name) {
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

    test("POST fails if key for role already exists " + name) {
      val repoId = RepoId.generate()

      Post(apiUri(s"root/${repoId.show}"), ClientRootGenRequest(keyType = keyType)) ~> routes ~> check {
        status shouldBe StatusCodes.Accepted
      }

      Post(apiUri(s"root/${repoId.show}"), ClientRootGenRequest(keyType = keyType)) ~> routes ~> check {
        status shouldBe StatusCodes.Conflict
      }
    }

    test("GET returns Locked if keys are not ready " + name) {
      val repoId = RepoId.generate()

      Post(apiUri(s"root/${repoId.show}"), ClientRootGenRequest(keyType = keyType)) ~> routes ~> check {
        status shouldBe StatusCodes.Accepted
      }

      Get(apiUri(s"root/${repoId.show}")) ~> routes ~> check {
        status shouldBe StatusCodes.Locked
      }
    }

    test("GET returns 502 if key generation failed " + name) {
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

    test("GET returns 200 if keys are ready " + name) {
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

    test("GET returns 200 with all keys if threshold > 1 " + name) {
      val repoId = RepoId.generate()

      generateRootRole(repoId, keyType = keyType, threshold = 4).futureValue

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

    test("POST to repoId/roletype signs any payload with existing keys " + name) {
      val repoId = RepoId.generate()

      generateRootRole(repoId, keyType).futureValue

      val toSignPayload = Json.fromFields(List("some" -> Json.fromString("signed stuff")))

      val targetKey = Get(apiUri(s"root/${repoId.show}/keys/targets/pairs"))  ~> routes ~> check {
        responseAs[Seq[TufKeyPair]].head
      }

      Post(apiUri(s"root/${repoId.show}/targets"), toSignPayload) ~> routes ~> check {
        status shouldBe StatusCodes.OK

        val signedPayload = responseAs[SignedPayload[Json]]

        signedPayload.signatures shouldNot be(empty)

        signedPayload.signed shouldBe toSignPayload

        forAll(signedPayload.signatures) { sig =>
          sig.keyid shouldBe targetKey.pubkey.id

          val isValidT = TufCrypto.isValid(sig, targetKey.pubkey.keyval, signedPayload.signed)
          isValidT shouldBe true
        }
      }
    }

    test("POST to repoId/roletype return 412 when key is offline " + name) {
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
        status shouldBe StatusCodes.PreconditionFailed
      }
    }

    test("DELETE on private_key wipes root private key " + name) {
      val repoId = RepoId.generate()

      generateRootRole(repoId, keyType).futureValue

      val rootKeyId = Get(apiUri(s"root/${repoId.show}")) ~> routes ~> check {
        status shouldBe StatusCodes.OK
        responseAs[SignedPayload[RootRole]].signed.roles(RoleType.ROOT).keyids.head
      }

      Delete(apiUri(s"root/${repoId.show}/private_keys/${rootKeyId.value}")) ~> routes ~> check {
        status shouldBe StatusCodes.NoContent
      }

      fakeVault.findKey(rootKeyId).failed.futureValue shouldBe VaultResourceNotFound
    }

    test("DELETE on private_key wipes non root private key " + name) {
      val repoId = RepoId.generate()

      generateRootRole(repoId, keyType).futureValue

      val keyId = Get(apiUri(s"root/${repoId.show}")) ~> routes ~> check {
        status shouldBe StatusCodes.OK
        responseAs[SignedPayload[RootRole]].signed.roles(RoleType.TARGETS).keyids.head
      }

      Delete(apiUri(s"root/${repoId.show}/private_keys/${keyId.value}")) ~> routes ~> check {
        status shouldBe StatusCodes.NoContent
      }

      fakeVault.findKey(keyId).failed.futureValue shouldBe VaultResourceNotFound
    }

    test("wiping private key still allows download of root.json " + name) {
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

    test("GET on an unsigned root returns unsigned root " + name) {
      val repoId = RepoId.generate()
      generateRootRole(repoId, keyType).futureValue

      Get(apiUri(s"root/${repoId.show}/unsigned")) ~> routes ~> check {
        status shouldBe StatusCodes.OK
        responseAs[RootRole] shouldBe a[RootRole]
      }
    }

    test("GET on unsigned root role returns root role with increased version " + name) {
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

    test("supports multiple offline signed root.json updates" + name) {
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

        val oldSignature = TufCrypto.signPayload(lastKey, newRoot)
        val oldClientSig = ClientSignature(lastKeyId, oldSignature.method, oldSignature.sig)

        val newSignature = TufCrypto.signPayload(keyPair.privkey, newRoot)
        val newClientSig = ClientSignature(keyPair.pubkey.id, newSignature.method, newSignature.sig)

        (keyPair, SignedPayload(Seq(oldClientSig, newClientSig), newRoot))
      }

      val lastRoot = latestRoot()
      val lastRootKeyId = lastRoot.roles(RoleType.ROOT).keyids.head
      val lastRootKey = fakeVault.findKey(lastRootKeyId).futureValue

      val (newKeyPair, signedPayload0) = signedRoot(lastRoot, lastRootKeyId, lastRootKey.privateKey)

      Post(apiUri(s"root/${repoId.show}/unsigned"), signedPayload0) ~> routes ~> check { // TODO: Wrong api ? this is messed up
        status shouldBe StatusCodes.NoContent
      }

      val (_, signedPayload1) = signedRoot(latestRoot(), newKeyPair.pubkey.id, newKeyPair.privkey)

      Post(apiUri(s"root/${repoId.show}/unsigned"), signedPayload1) ~> routes ~> check {
        status shouldBe StatusCodes.NoContent
      }

      Get(apiUri(s"root/${repoId.show}"), signedPayload1) ~> routes ~> check {
        status shouldBe StatusCodes.OK
        responseAs[SignedPayload[RootRole]] shouldBe signedPayload1
      }
    }

    test("POST offline signed deletes all keys for role" + name) {
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
      val oldSignedPayload = signPayloadWithVaultKey(rootKeyId, rootRole)
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
        status shouldBe StatusCodes.PreconditionFailed
        responseAs[ErrorRepresentation].code shouldBe ErrorCodes.KeyServer.PrivateKeysNotFound
      }
    }

    test("POST offline with same version returns bad request " + name) {
      val repoId = RepoId.generate()
      generateRootRole(repoId, keyType).futureValue

      val rootRole = Get(apiUri(s"root/${repoId.show}/unsigned")) ~> routes ~> check {
        status shouldBe StatusCodes.OK
        responseAs[RootRole]
      }

      val sameVersionRoot = rootRole.copy(version = rootRole.version - 1)
      val rootKeyId = sameVersionRoot.roles(RoleType.ROOT).keyids.head
      val signedPayload = signPayloadWithVaultKey(rootKeyId, sameVersionRoot)

      Post(apiUri(s"root/${repoId.show}/unsigned"), signedPayload) ~> routes ~> check {
        responseErrors should contain("Invalid version bump from 1 to 1")
        status shouldBe StatusCodes.BadRequest
      }
    }

    test("POST returns 4xx when signature is not valid" + name) {
      val repoId = RepoId.generate()
      generateRootRole(repoId, keyType).futureValue

      val rootRole = Get(apiUri(s"root/${repoId.show}/unsigned")) ~> routes ~> check {
        status shouldBe StatusCodes.OK
        responseAs[RootRole]
      }

      val rootKeyId = rootRole.roles(RoleType.ROOT).keyids.head
      val clientSignature = clientSignWithVaultKey(rootKeyId, "not a root role")
      val signedPayload = SignedPayload(Seq(clientSignature), rootRole)

      Post(apiUri(s"root/${repoId.show}/unsigned"), signedPayload) ~> routes ~> check {
        status shouldBe StatusCodes.BadRequest
        responseErrors should contain(s"Invalid signature for key $rootKeyId in root.json version 2")
      }
    }

    test("POST returns error payload is not signed with all keys " + name) {
      val repoId = RepoId.generate()
      generateRootRole(repoId, keyType).futureValue

      val rootRole = Get(apiUri(s"root/${repoId.show}/unsigned")) ~> routes ~> check {
        status shouldBe StatusCodes.OK
        responseAs[RootRole]
      }

      val signedPayload = SignedPayload(Seq.empty, rootRole)

      Post(apiUri(s"root/${repoId.show}/unsigned"), signedPayload) ~> routes ~> check {
        status shouldBe StatusCodes.BadRequest
        responseErrors should contain("Root role version 1 requires 1 valid signatures in version 2, 0 supplied")
        responseErrors should contain("Root role version 2 requires 1 valid signatures in version 2, 0 supplied")
      }
    }

    test("POST offline signed returns 204 when signature is valid " + name) {
      val repoId = RepoId.generate()
      generateRootRole(repoId, keyType).futureValue

      val rootRole = Get(apiUri(s"root/${repoId.show}/unsigned")) ~> routes ~> check {
        status shouldBe StatusCodes.OK
        responseAs[RootRole]
      }

      val rootKeyId = rootRole.roles(RoleType.ROOT).keyids.head
      val signedPayload = signPayloadWithVaultKey(rootKeyId, rootRole)

      Post(apiUri(s"root/${repoId.show}/unsigned"), signedPayload) ~> routes ~> check {
        status shouldBe StatusCodes.NoContent
      }
    }

    test("GET with version after POST offline signed returns old root " + name) {
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
      val signedPayload = signPayloadWithVaultKey(rootKeyId, newUnsignedRole)

      Post(apiUri(s"root/${repoId.show}/unsigned"), signedPayload) ~> routes ~> check {
        status shouldBe StatusCodes.NoContent
      }

      val rootRoleV1 = Get(apiUri(s"root/${repoId.show}/1")) ~> routes ~> check {
        status shouldBe StatusCodes.OK
        responseAs[SignedPayload[RootRole]]
      }

      oldRootRole.asJson shouldBe rootRoleV1.asJson
    }

  test("POST offline signed updates keys when signature is valid " + name) {
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
    val oldSignedPayload = signPayloadWithVaultKey(rootKeyId, rootRole)
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

    test("POST offline signed returns 400 when not signed with new " + name) {
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

      val signedPayload = signPayloadWithVaultKey(rootKeyId, rootRole)

      Post(apiUri(s"root/${repoId.show}/unsigned"), signedPayload) ~> routes ~> check {
        status shouldBe StatusCodes.BadRequest
        responseErrors should contain("Root role version 2 requires 1 valid signatures in version 2, 0 supplied")
      }
    }

    test("POST offline signed returns 400 when not signed with old " + name) {
      val repoId = RepoId.generate()
      generateRootRole(repoId, keyType).futureValue

      val oldRootRole = Get(apiUri(s"root/${repoId.show}/unsigned")) ~> routes ~> check {
        status shouldBe StatusCodes.OK
        responseAs[RootRole]
      }

      val keyPair = keyType.crypto.generateKeyPair(keyType.crypto.defaultKeySize)

      val rootKeyId = oldRootRole.roles(RoleType.ROOT).keyids.head
      val rootRole = oldRootRole.withRoleKeys(RoleType.ROOT, keyPair.pubkey)

      val signedPayload = SignedPayload(List(signWithKeyPair(keyPair.pubkey.id, keyPair.privkey, rootRole)), rootRole)

      Post(apiUri(s"root/${repoId.show}/unsigned"), signedPayload) ~> routes ~> check {
        status shouldBe StatusCodes.BadRequest
        responseErrors should contain("Root role version 1 requires 1 valid signatures in version 2, 0 supplied")
      }
    }

    test("GET target key pairs " + name) {
      val repoId = RepoId.generate()
      val publicKeys = generateRootRole(repoId, keyType).futureValue.filter(_.roleType == RoleType.TARGETS)

      Get(apiUri(s"root/${repoId.show}/keys/targets/pairs")) ~> routes ~> check {
        status shouldBe StatusCodes.OK
        responseAs[Seq[TufKeyPair]].map(_.pubkey).toSet shouldBe publicKeys.map(_.toTufKey).toSet
      }
    }

    test("GET target key pairs error on key not found in vault " + name) {
      val repoId = RepoId.generate()
      val keys = generateRootRole(repoId, keyType).futureValue

      Future.sequence(keys.map(_.id).map(fakeVault.deleteKey)).futureValue

      Get(apiUri(s"root/${repoId.show}/keys/targets/pairs")) ~> routes ~> check {
        responseAs[ErrorRepresentation].code.code shouldBe "vault_resource_not_found"
        status shouldBe StatusCodes.BadGateway
      }
    }
  }

  testsFor(keySpecific[RsaKeyType.type, RSATufKey](RsaKeyType, "RSA"))
  testsFor(keySpecific[Ed25519KeyType.type, Ed25519TufKey](Ed25519KeyType, "Ed25519"))

  def signWithKeyPair(keyId: KeyId, priv: TufPrivateKey, role: RootRole): ClientSignature = {
    val signature = TufCrypto.signPayload(priv, role)
    ClientSignature(keyId, signature.method, signature.sig)
  }

  def clientSignWithVaultKey[T : Encoder](keyId: KeyId, payload: T): ClientSignature = {
    val vaultKey = fakeVault.findKey(keyId).futureValue
    val signature = TufCrypto.signPayload(vaultKey.privateKey, payload)
    ClientSignature(keyId, signature.method, signature.sig)
  }

  def signPayloadWithVaultKey[T : Encoder](keyId: KeyId, payloadToSign: T): SignedPayload[T] = {
    val clientSignature = clientSignWithVaultKey(keyId, payloadToSign)
    SignedPayload(Seq(clientSignature), payloadToSign)
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
    import cats.syntax.either._
    import scala.concurrent.duration._

    def responseErrors: List[String] =
      responseAs[ErrorRepresentation].cause.flatMap(_.as[List[String]].toOption).getOrElse(List.empty)
}
