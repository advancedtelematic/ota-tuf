package com.advancedtelematic.util

import java.security.{KeyPair, PrivateKey, PublicKey}
import java.util.NoSuchElementException
import java.util.concurrent.ConcurrentHashMap

import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.advancedtelematic.ota_tuf.crypt.RsaKeyPair
import com.advancedtelematic.ota_tuf.data.ClientDataType.{ClientKey, ClientSignature, RoleKeys, RootRole, SignatureToClientSignatureOps, SignedPayload}
import com.advancedtelematic.ota_tuf.data.DataType.GroupId
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

  def publicKey(groupId: GroupId): PublicKey =
    keys.asScala(groupId).getPublic

  private def keyPair(groupId: GroupId): KeyPair =
    keys.asScala(groupId)

  private val keys = new ConcurrentHashMap[GroupId, KeyPair]()

  def rootRole(groupId: GroupId) = {
    val rootKey = keys.asScala(groupId)
    val clientKeys = Map(rootKey.id -> ClientKey(KeyType.RSA, rootKey.getPublic))

    val roles = RoleType.ALL.map { role =>
      role.show -> RoleKeys(List(rootKey.id), threshold = 1)
    }.toMap

    RootRole(clientKeys, roles, version = 1)
  }

  def generateKey(groupId: GroupId): KeyPair = {
    val rootKey = RsaKeyPair.generate(1024)
    keys.put(groupId, rootKey)
  }

  override def sign[T: Encoder](groupId: GroupId, roleType: RoleType, payload: T): Future[SignedPayload[Json]] = {
    val signature = signWithRoot(groupId, payload)
    FastFuture.successful(SignedPayload(List(signature), payload.asJson))
  }

  override def fetchRootRole(groupId: GroupId): Future[SignedPayload[Json]] = {
    Future.fromTry {
      Try {
        val role = rootRole(groupId)
        val signature = signWithRoot(groupId, role)
        SignedPayload(List(signature), role.asJson)
      }.recover {
        case ex: NoSuchElementException =>
          throw RootRoleNotFound
      }
    }
  }

  private def signWithRoot[T : Encoder](groupId: GroupId, payload: T): ClientSignature = {
    val key = keyPair(groupId)
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
