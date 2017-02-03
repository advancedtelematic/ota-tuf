package com.advancedtelematic.util

import java.security.{KeyPair, PrivateKey, PublicKey}
import java.util.NoSuchElementException
import java.util.concurrent.ConcurrentHashMap

import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.advancedtelematic.ota_tuf.crypt.RsaKeyPair
import com.advancedtelematic.ota_tuf.data.ClientDataType.{ClientKey, ClientSignature, RoleKeys, RootRole, SignatureToClientSignatureOps, SignedPayload}
import com.advancedtelematic.ota_tuf.data.DataType.RepoId
import com.advancedtelematic.ota_tuf.data.RoleType.RoleType
import com.advancedtelematic.ota_tuf.http.OtaTufRoutes
import io.circe.{Encoder, Json}
import org.genivi.sota.core.DatabaseSpec
import io.circe.syntax._
import com.advancedtelematic.ota_tuf.http.CanonicalJson._
import cats.syntax.show._

import scala.concurrent.Future
import RsaKeyPair._
import akka.http.scaladsl.util.FastFuture
import com.advancedtelematic.ota_tuf.data.{KeyType, RoleType}
import com.advancedtelematic.ota_tuf.data.Codecs._
import com.advancedtelematic.ota_tuf.repo_store.RoleKeyStoreClient

import scala.collection.JavaConverters._
import scala.util.Try


object FakeRoleStore extends RoleKeyStoreClient {

  def publicKey(repoId: RepoId): PublicKey =
    keys.asScala(repoId).getPublic

  private def keyPair(repoId: RepoId): KeyPair =
    keys.asScala(repoId)

  private val keys = new ConcurrentHashMap[RepoId, KeyPair]()

  def rootRole(repoId: RepoId) = {
    val rootKey = keys.asScala(repoId)
    val clientKeys = Map(rootKey.id -> ClientKey(KeyType.RSA, rootKey.getPublic))

    val roles = RoleType.ALL.map { role =>
      role.show -> RoleKeys(List(rootKey.id), threshold = 1)
    }.toMap

    RootRole(clientKeys, roles, version = 1)
  }

  def generateKey(repoId: RepoId): KeyPair = {
    val rootKey = RsaKeyPair.generate(1024)
    keys.put(repoId, rootKey)
  }

  override def sign[T: Encoder](repoId: RepoId, roleType: RoleType, payload: T): Future[SignedPayload[Json]] = {
    val signature = signWithRoot(repoId, payload)
    FastFuture.successful(SignedPayload(List(signature), payload.asJson))
  }

  override def fetchRootRole(repoId: RepoId): Future[SignedPayload[Json]] = {
    Future.fromTry {
      Try {
        val role = rootRole(repoId)
        val signature = signWithRoot(repoId, role)
        SignedPayload(List(signature), role.asJson)
      }.recover {
        case ex: NoSuchElementException =>
          throw RootRoleNotFound
      }
    }
  }

  private def signWithRoot[T : Encoder](repoId: RepoId, payload: T): ClientSignature = {
    val key = keyPair(repoId)
    RsaKeyPair
      .sign(key.getPrivate, payload.asJson.canonical.getBytes)
      .toClient(key.id)
  }
}

trait ResourceSpec extends OtaTufSpec with ScalatestRouteTest with DatabaseSpec {
  def apiUri(path: String): String = "/api/v1/" + path

  val fakeRoleStore = FakeRoleStore

  lazy val routes = new OtaTufRoutes(fakeVault, fakeRoleStore).routes
}
