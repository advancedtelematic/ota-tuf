package com.advancedtelematic.tuf.keyserver.client

import java.security.interfaces.{RSAPublicKey}

import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.data.ClientDataType.RootRole
import com.advancedtelematic.libtuf.data.TufDataType.{Ed25519KeyType, Ed25519TufKeyPair, KeyId, KeyType, RSATufKeyPair, RepoId, RoleType, RsaKeyType, SignedPayload, ValidKeyId}
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libats.http.Errors.RawError
import com.advancedtelematic.libtuf_server.keyserver.{KeyserverClient, KeyserverHttpClient}
import com.advancedtelematic.tuf.keyserver.daemon.{DefaultKeyGenerationOp}
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.{KeyGenId, KeyGenRequest, KeyGenRequestStatus}
import com.advancedtelematic.tuf.keyserver.db.KeyGenRequestSupport
import com.advancedtelematic.tuf.util.{HttpClientSpecSupport, ResourceSpec, RootGenerationSpecSupport, TufKeyserverSpec}
import eu.timepit.refined.refineV
import io.circe.Json
import org.scalatest.concurrent.PatienceConfiguration
import org.scalatest.time.{Millis, Seconds, Span}
import scala.async.Async.{async, await}
import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.ClassTag

class KeyserverHttpClientSpec extends TufKeyserverSpec
  with ResourceSpec
  with KeyGenRequestSupport
  with RootGenerationSpecSupport
  with PatienceConfiguration
  with HttpClientSpecSupport {

  implicit val ec = ExecutionContext.global

  override implicit def patienceConfig = PatienceConfig(timeout = Span(20, Seconds), interval = Span(500, Millis))

  val client = new KeyserverHttpClient("http://test-keyserver", testHttpClient)

  def createAndProcessRoot(repoId: RepoId, keyType: KeyType): Future[Unit] = {
    for {
      _ <- client.createRoot(repoId, keyType)
      _ <- processKeyGenerationRequest(repoId)
    } yield ()
  }

  // only makes sense for RSA
  test("minimum RSA key size when creating a repo") {
    val repoId = RepoId.generate()

    val f = for {
      _ <- client.createRoot(repoId, RsaKeyType)
      _ <- processKeyGenerationRequest(repoId)
      rootRole <- client.fetchRootRole(repoId)
    } yield rootRole.signed.keys.values

    val keys = f.futureValue

    keys.foreach { key =>
      key.keyval match {
        case pubKey: RSAPublicKey =>
          pubKey.getModulus.bitLength() should be >= 2048
      }
    }
  }

  def keySpecific[T <: KeyType, P <: T#Pair: ClassTag](keyType: T, name: String): Unit = {

    def differentKeyType: KeyType = keyType match {
      case _: RsaKeyType.type => Ed25519KeyType
      case _: Ed25519KeyType.type => RsaKeyType
    }

    def manipulateSignedKey(payload: SignedPayload[RootRole], keyType: KeyType): SignedPayload[RootRole] = {
      val kid: KeyId = refineV[ValidKeyId]("0" * 64).right.get
      val key = differentKeyType.crypto.convertPublic(payload.signed.keys.values.head.keyval)
      val signedCopy = payload.signed.copy(keys = payload.signed.keys.updated(kid, key))
      payload.copy(signed = signedCopy)
    }

    test("creates a root " + name) {
      val repoId = RepoId.generate()
      client.createRoot(repoId, keyType).futureValue shouldBe a[Json]
    }

    test("adds a key to a root " + name) {
      val repoId = RepoId.generate()
      val f = createAndProcessRoot(repoId, keyType)

      whenReady(f) { _ =>
        val publicKey = TufCrypto.generateKeyPair(keyType, keyType.crypto.defaultKeySize).pubkey

        val rootRoleF = for {
          _ <- client.addTargetKey(repoId, publicKey)
          payload <- client.fetchRootRole(repoId)
        } yield payload.signed

        whenReady(rootRoleF) { rootRole =>
          val targetKeys = rootRole.roles(RoleType.TARGETS)
          targetKeys.keyids should contain(publicKey.id)

          rootRole.keys(publicKey.id) shouldBe publicKey
        }
      }
    }

    test("fetches unsigned root " + name) {
      val repoId = RepoId.generate()

      val f = for {
        _ <- createAndProcessRoot(repoId, keyType)
        unsigned <- client.fetchUnsignedRoot(repoId)
      } yield unsigned

      f.futureValue shouldBe a[RootRole]
    }

    test("updates a root " + name) {
      val repoId = RepoId.generate()

      val f = for {
        _ <- createAndProcessRoot(repoId, keyType)
        unsigned <- client.fetchUnsignedRoot(repoId)
        signed ← client.sign(repoId, RoleType.ROOT, unsigned)
        updated <- client.updateRoot(repoId, signed)
      } yield updated

      f.futureValue shouldBe (())
    }

    test("root update with invalid key returns expected error " + name) {
      val repoId = RepoId.generate()

      val f = for {
        _ <- createAndProcessRoot(repoId, keyType)
        signed <- client.fetchRootRole(repoId)
        updated <- client.updateRoot(repoId, manipulateSignedKey(signed, keyType))
      } yield updated

      val failure = f.failed.futureValue
      failure shouldBe a[RawError]
      failure.getMessage shouldBe "key cannot be processed"
    }

    test("fetches a root key pair " + name) {
      val repoId = RepoId.generate()

      val f = for {
        _ <- createAndProcessRoot(repoId, keyType)
        root ← client.fetchRootRole(repoId)
        keyId = root.signed.roles(RoleType.TARGETS).keyids.head
        keyPair <- client.fetchKeyPair(repoId, keyId)
      } yield (keyId, keyPair)

      whenReady(f) { case (keyId, keyPair) ⇒
        keyPair shouldBe a[P]
        keyPair.pubkey.id shouldBe keyId
      }
    }

    test("deletes a key " + name) {
      val repoId = RepoId.generate()

      val f = for {
        _ <- createAndProcessRoot(repoId, keyType)
        signed <- client.fetchRootRole(repoId)
        deleted <- client.deletePrivateKey(repoId, signed.signed.keys.keys.head)
      } yield deleted

      f.futureValue shouldBe (())
    }

    test("can sign json " + name) {
      val repoId = RepoId.generate()

      val f = for {
        _ <- createAndProcessRoot(repoId, keyType)
        sig <- client.sign(repoId, RoleType.TARGETS, Json.Null)
      } yield sig

      f.futureValue shouldBe a[SignedPayload[_]]
    }

    test("signing with removed key produces RoleKeyNotFound error " + name) {
      val repoId = RepoId.generate()

      val f = for {
        _ <- createAndProcessRoot(repoId, keyType)
        signed <- client.fetchRootRole(repoId)
        _ <- client.deletePrivateKey(repoId, signed.signed.roles(RoleType.TARGETS).keyids.head)
        sig <- client.sign(repoId, RoleType.TARGETS, Json.Null)
      } yield sig

      f.failed.futureValue shouldBe KeyserverClient.RoleKeyNotFound
    }

    test("uploading a key with a keyval of a different type fails " + name) {
      val repoId = RepoId.generate()
      val f = createAndProcessRoot(repoId, keyType)

      whenReady(f) { _ =>
        val publicKey = keyType.crypto.generateKeyPair().pubkey
        val other = differentKeyType.crypto.convertPublic(publicKey.keyval)
        val error = client.addTargetKey(repoId, other).failed.futureValue

        val malformed_error = keyType match {
          case _: RsaKeyType.type => "algorithm identifier 1.2.840.113549.1.1.1 in key not recognised"
          case _: Ed25519KeyType.type => "Key is not an RSAPublicKey"
        }

        error.getMessage should include(malformed_error)
      }
    }

    test("fetching target key pairs " + name) {
      val keyGenerationOp = DefaultKeyGenerationOp(fakeVault)

      val repoId = RepoId.generate()
      val keyGenRequest = KeyGenRequest(KeyGenId.generate(),
        repoId, KeyGenRequestStatus.REQUESTED, RoleType.TARGETS, keyType.crypto.defaultKeySize, keyType)

      async {
        await(keyGenRepo.persist(keyGenRequest))
        val generatedKeys = await(keyGenerationOp(keyGenRequest))
        val pairs = await(client.fetchTargetKeyPairs(repoId))
        pairs.map(_.pubkey.keyval) shouldBe generatedKeys.map(_.publicKey)
      }.futureValue

    }

    test("fetches root role by version " + name) {
      val repoId = RepoId.generate()

      val f = for {
        _ <- createAndProcessRoot(repoId, keyType)
        _ <- client.fetchRootRole(repoId)
        signed <- client.fetchRootRole(repoId, 1)
      } yield signed

      f.futureValue shouldBe a[SignedPayload[_]]
      f.futureValue.signed shouldBe a[RootRole]
    }

    test("returns KeysNotReady when keys are not yet ready " + name) {
      val repoId = RepoId.generate()
      val keyGenRequest = KeyGenRequest(KeyGenId.generate(),
        repoId, KeyGenRequestStatus.REQUESTED, RoleType.TARGETS, keyType.crypto.defaultKeySize, keyType)

      val f = for {
        _ <- keyGenRepo.persist(keyGenRequest)
        root <- client.fetchRootRole(repoId)
      } yield root

      f.failed.futureValue shouldBe KeyserverClient.KeysNotReady
    }

  }

  // the TufKeyPair types are otherwise hard to get at compile time
  testsFor(keySpecific[RsaKeyType.type, RSATufKeyPair](RsaKeyType, "RSA"))
  testsFor(keySpecific[Ed25519KeyType.type, Ed25519TufKeyPair](Ed25519KeyType, "Ed25519"))

}
