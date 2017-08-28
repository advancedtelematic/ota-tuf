package com.advancedtelematic.tuf.keyserver.client

import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.data.ClientDataType.RootRole
import com.advancedtelematic.libtuf.data.TufDataType.{EdKeyType, RepoId, RoleType}
import com.advancedtelematic.libtuf.keyserver.KeyserverHttpClient
import com.advancedtelematic.tuf.keyserver.db.KeyGenRequestSupport
import com.advancedtelematic.tuf.util.{HttpClientSpecSupport, ResourceSpec, RootGenerationSpecSupport, TufKeyserverSpec}
import io.circe.Json
import org.scalatest.concurrent.PatienceConfiguration
import org.scalatest.time.{Millis, Seconds, Span}

import scala.concurrent.{ExecutionContext, Future}

class KeyserverHttpClientSpec extends TufKeyserverSpec
  with ResourceSpec
  with KeyGenRequestSupport
  with RootGenerationSpecSupport
  with PatienceConfiguration
  with HttpClientSpecSupport {

  implicit val ec = ExecutionContext.global

  override implicit def patienceConfig = PatienceConfig(timeout = Span(20, Seconds), interval = Span(500, Millis))

  val client = new KeyserverHttpClient("http://localhost", testHttpClient)

  def createAndProcessRoot(repoId: RepoId): Future[Unit] = {
    for {
      _ <- client.createRoot(repoId, EdKeyType)
      _ <- processKeyGenerationRequest(repoId)
    } yield ()
  }

  test("creates a root") {
    val repoId = RepoId.generate()
    client.createRoot(repoId, EdKeyType).futureValue shouldBe a[Json]
  }

  test("adds a key to a root") {
    val repoId = RepoId.generate()
    val f = createAndProcessRoot(repoId)

    whenReady(f) { _ =>
      val (publicKey, _) = TufCrypto.generateKeyPair(EdKeyType, 256)

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
      signed <- client.fetchRootRole(repoId)
      updated <- client.updateRoot(repoId, signed)
    } yield updated

    f.futureValue shouldBe (())
  }
}
