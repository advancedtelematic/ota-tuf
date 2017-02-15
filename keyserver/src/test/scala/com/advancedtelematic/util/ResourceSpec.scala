package com.advancedtelematic.util

import java.security.{KeyPair, PrivateKey, PublicKey}
import java.util.NoSuchElementException
import java.util.concurrent.ConcurrentHashMap

import com.advancedtelematic.libtuf.crypt.RsaKeyPair._
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import com.advancedtelematic.libtuf.data.TufDataType._
import com.advancedtelematic.keyserver.http.OtaTufRoutes
import io.circe.{Encoder, Json}
import org.genivi.sota.core.DatabaseSpec
import io.circe.syntax._
import com.advancedtelematic.keyserver.http.CanonicalJson._
import cats.syntax.show._
import com.advancedtelematic.keyserver.data.KeyServerDataType._

import scala.concurrent.Future
import akka.actor.ActorSystem
import akka.http.scaladsl.util.FastFuture
import com.advancedtelematic.keyserver.data.KeyServerCodecs._
import com.advancedtelematic.libtuf.data.TufCodecs._

import scala.concurrent.duration._
import scala.collection.JavaConverters._
import scala.util.Try
import akka.testkit.TestDuration
import com.advancedtelematic.libtuf.crypt.RsaKeyPair
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.libtuf.data.TufDataType.{KeyType, RoleType}
import com.advancedtelematic.libtuf.repo_store.RoleKeyStoreClient
import com.advancedtelematic.keyserver.data.KeyServerDataType
import com.advancedtelematic.keyserver.data.ClientDataType.{ClientKey, RoleKeys, RootRole}
import com.advancedtelematic.libtuf.data.TufDataType.RepoId

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

trait ResourceSpec extends OtaTufSpec
  with ScalatestRouteTest
  with DatabaseSpec
  with LongHttpRequest {
  def apiUri(path: String): String = "/api/v1/" + path

  val fakeRoleStore = FakeRoleStore

  lazy val routes = new OtaTufRoutes(fakeVault, fakeRoleStore).routes
}
