package com.advancedtelematic.tuf.keyserver.client

import java.security.interfaces.RSAPublicKey

import cats.instances.map
import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.data.ClientDataType.RootRole
import com.advancedtelematic.libtuf.data.TufDataType.{Ed25519KeyType, Ed25519TufKey, Ed25519TufKeyPair, Ed25519TufPrivateKey, KeyId, RSATufKey, RepoId, RoleType, RsaKeyType, SignedPayload, ValidKeyId}
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libats.http.Errors.RawError
import com.advancedtelematic.libtuf_server.keyserver.{KeyserverClient, KeyserverHttpClient}
import com.advancedtelematic.tuf.keyserver.daemon.{DefaultKeyGenerationOp, KeyGenerationOp}
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.{KeyGenId, KeyGenRequest, KeyGenRequestStatus}
import com.advancedtelematic.tuf.keyserver.db.KeyGenRequestSupport
import com.advancedtelematic.tuf.util.{HttpClientSpecSupport, ResourceSpec, RootGenerationSpecSupport, TufKeyserverSpec}
import eu.timepit.refined.refineV
import io.circe.Json
import org.scalatest.concurrent.PatienceConfiguration
import org.scalatest.time.{Millis, Seconds, Span}

import scala.async.Async.{async, await}
import scala.concurrent.{ExecutionContext, Future}

class KeyserverHttpClientSpec extends TufKeyserverSpec
  with ResourceSpec
  with KeyGenRequestSupport
  with RootGenerationSpecSupport
  with PatienceConfiguration
  with HttpClientSpecSupport {

  implicit val ec = ExecutionContext.global

  override implicit def patienceConfig = PatienceConfig(timeout = Span(20, Seconds), interval = Span(500, Millis))

  val client = new KeyserverHttpClient("http://test-keyserver", testHttpClient)

  def createAndProcessRoot(repoId: RepoId): Future[Unit] = {
    for {
      _ <- client.createRoot(repoId, Ed25519KeyType)
      _ <- processKeyGenerationRequest(repoId)
    } yield ()
  }

  def manipulateSignedRsaKey(payload: SignedPayload[RootRole]): SignedPayload[RootRole] = {
    val kid: KeyId = refineV[ValidKeyId]("0" * 64).right.get
    // change type of one of the RSA keys to Ed25519:
    val key = Ed25519TufKey(payload.signed.keys.values.head.keyval)
    val signedCopy = payload.signed.copy(keys = payload.signed.keys.updated(kid, key))
    payload.copy(signed = signedCopy)
  }

  test("creates a root") {
    val repoId = RepoId.generate()
    client.createRoot(repoId, Ed25519KeyType).futureValue shouldBe a[Json]
  }

  test("adds a key to a root") {
    val repoId = RepoId.generate()
    val f = createAndProcessRoot(repoId)

    whenReady(f) { _ =>
      val publicKey = TufCrypto.generateKeyPair(Ed25519KeyType, 256).pubkey

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

  test("fetches unsigned root") {
    val repoId = RepoId.generate()

    val f = for {
      _ <- createAndProcessRoot(repoId)
      unsigned <- client.fetchUnsignedRoot(repoId)
    } yield unsigned

    f.futureValue shouldBe a[RootRole]
  }

  test("updates a root") {
    val repoId = RepoId.generate()

    val f = for {
      _ <- createAndProcessRoot(repoId)
      unsigned <- client.fetchUnsignedRoot(repoId)
      signed ← client.sign(repoId, RoleType.ROOT, unsigned)
      updated <- client.updateRoot(repoId, signed)
    } yield updated

    f.futureValue shouldBe (())
  }

  test("root update with invalid key returns expected error") {
    val repoId = RepoId.generate()

    val f = for {
      _ <- createAndProcessRoot(repoId)
      signed <- client.fetchRootRole(repoId)
      updated <- client.updateRoot(repoId, manipulateSignedRsaKey(signed))
    } yield updated

    val failure = f.failed.futureValue
    failure shouldBe a[RawError]
    failure.getMessage shouldBe "key cannot be processed"
  }

  test("fetches a root key pair") {
    val repoId = RepoId.generate()

    val f = for {
      _ <- createAndProcessRoot(repoId)
      root ← client.fetchRootRole(repoId)
      keyId = root.signed.roles(RoleType.TARGETS).keyids.head
      keyPair <- client.fetchKeyPair(repoId, keyId)
    } yield (keyId, keyPair)

    whenReady(f) { case (keyId, keyPair) ⇒
      keyPair shouldBe a[Ed25519TufKeyPair]
      keyPair.pubkey.id shouldBe keyId
    }
  }

  test("deletes a key") {
    val repoId = RepoId.generate()

    val f = for {
      _ <- createAndProcessRoot(repoId)
      signed <- client.fetchRootRole(repoId)
      deleted <- client.deletePrivateKey(repoId, signed.signed.keys.keys.head)
    } yield deleted

    f.futureValue shouldBe (())
  }

  test("can sign json") {
    val repoId = RepoId.generate()

    val f = for {
      _ <- createAndProcessRoot(repoId)
      sig <- client.sign(repoId, RoleType.TARGETS, Json.Null)
    } yield sig

    f.futureValue shouldBe a[SignedPayload[_]]
  }

  test("signing with removed key produces RoleKeyNotFound error") {
    val repoId = RepoId.generate()

    val f = for {
      _ <- createAndProcessRoot(repoId)
      signed <- client.fetchRootRole(repoId)
      _ <- client.deletePrivateKey(repoId, signed.signed.roles(RoleType.TARGETS).keyids.head)
      sig <- client.sign(repoId, RoleType.TARGETS, Json.Null)
    } yield sig

    f.failed.futureValue shouldBe KeyserverClient.RoleKeyNotFound
  }

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
        case rsaPubKey : RSAPublicKey =>
          rsaPubKey.getModulus().bitLength() should be >= 2048
      }
    }
  }

  test("uploading an RSA key with a keyval of a different type fails") {
    val repoId = RepoId.generate()
    val f = createAndProcessRoot(repoId)

    whenReady(f) { _ =>
      val edPublicKey = TufCrypto.generateKeyPair(Ed25519KeyType, 256).pubkey
      val rsa = RSATufKey(edPublicKey.keyval)
      val error = client.addTargetKey(repoId, rsa).failed.futureValue

      error.getMessage should include("Key is not an RSAPublicKey")
    }
  }

  test("fetching target key pairs") {
    val keyGenerationOp = DefaultKeyGenerationOp(fakeVault)

    val repoId = RepoId.generate()
    val keyGenRequest = KeyGenRequest(KeyGenId.generate(),
      repoId, KeyGenRequestStatus.REQUESTED, RoleType.TARGETS, 2048, RsaKeyType)

    async {
      await(keyGenRepo.persist(keyGenRequest))
      val generatedKeys = await(keyGenerationOp(keyGenRequest))
      val pairs = await(client.fetchTargetKeyPairs(repoId))
      pairs.map(_.pubkey.keyval) shouldBe generatedKeys.map(_.publicKey)
    }.futureValue

  }

  test("fetches root role by version") {
    val repoId = RepoId.generate()

    val f = for {
      _ <- createAndProcessRoot(repoId)
      _ <- client.fetchRootRole(repoId)
      signed <- client.fetchRootRole(repoId, 1)
    } yield signed

    f.futureValue shouldBe a[SignedPayload[_]]
    f.futureValue.signed shouldBe a[RootRole]
  }

  test("returns KeysNotReady when keys are not yet ready") {
    val repoId = RepoId.generate()
    val keyGenRequest = KeyGenRequest(KeyGenId.generate(),
      repoId, KeyGenRequestStatus.REQUESTED, RoleType.TARGETS, 2048, RsaKeyType)

    val f = for {
      _ <- keyGenRepo.persist(keyGenRequest)
      root <- client.fetchRootRole(repoId)
    } yield root

    f.failed.futureValue shouldBe KeyserverClient.KeysNotReady
  }
}
