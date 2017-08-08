package com.advancedtelematic.tuf.reposerver.util

import java.nio.file.Files
import java.security.{KeyPair, PublicKey}
import java.time.Instant
import java.util.NoSuchElementException
import java.util.concurrent.ConcurrentHashMap

import com.advancedtelematic.libtuf.crypt.TufCrypto._
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import com.advancedtelematic.libtuf.data.TufDataType._
import io.circe.{Decoder, Encoder, Json}
import com.advancedtelematic.libats.test.DatabaseSpec
import io.circe.syntax._
import com.advancedtelematic.libtuf.crypt.CanonicalJson._

import scala.concurrent.Future
import akka.actor.ActorSystem
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.{Directive1, Directives}
import akka.http.scaladsl.util.FastFuture
import com.advancedtelematic.libtuf.data.ClientCodecs._

import scala.concurrent.duration._
import scala.collection.JavaConverters._
import scala.util.Try
import akka.testkit.TestDuration
import com.advancedtelematic.libats.auth.NamespaceDirectives
import com.advancedtelematic.libats.data.Namespace
import com.advancedtelematic.libats.messaging.MemoryMessageBus
import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.libtuf.data.TufDataType.RoleType
import com.advancedtelematic.libtuf.keyserver.KeyserverClient
import com.advancedtelematic.libtuf.data.ClientDataType.{RoleKeys, RootRole}
import com.advancedtelematic.libtuf.data.TufDataType.RepoId
import com.advancedtelematic.tuf.reposerver.http.{NamespaceValidation, TufReposerverRoutes}
import com.advancedtelematic.tuf.reposerver.target_store.{LocalTargetStore, TargetUpload}

object FakeRoleStore extends KeyserverClient {

  private val keys = new ConcurrentHashMap[RepoId, KeyPair]()

  def publicKey(repoId: RepoId): PublicKey =
    keys.asScala(repoId).getPublic

  private def keyPair(repoId: RepoId): KeyPair =
    keys.asScala(repoId)

  def rootRole(repoId: RepoId) = {
    val rootKey = keys.asScala(repoId)
    val clientKeys = Map(rootKey.getPublic.id -> RSATufKey(rootKey.getPublic))

    val roles = RoleType.ALL.map { role =>
      role -> RoleKeys(List(rootKey.getPublic.id), threshold = 1)
    }.toMap

    RootRole(clientKeys, roles, expires = Instant.now.plusSeconds(3600), version = 1)
  }

  def generateKey(repoId: RepoId): KeyPair = {
    val (publicKey, privateKey) = TufCrypto.generateKeyPair(RsaKeyType, 2048)
    keys.put(repoId, new KeyPair(publicKey.keyval, privateKey.keyval))
  }

  override def createRoot(repoId: RepoId, keyType: KeyType): Future[Json] = {
    if (keys.contains(repoId)) {
      FastFuture.failed(RootRoleConflict)
    } else {
      generateKey(repoId)
      FastFuture.successful(Json.obj())
    }
  }

  override def sign[T: Decoder : Encoder](repoId: RepoId, roleType: RoleType, payload: T): Future[SignedPayload[T]] = {
    val signature = signWithRoot(repoId, payload)
    FastFuture.successful(SignedPayload(List(signature), payload))
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
    TufCrypto
      .sign(RsaKeyType, key.getPrivate, payload.asJson.canonical.getBytes)
      .toClient(key.getPublic.id)
  }
}

trait LongHttpRequest {
  implicit def default(implicit system: ActorSystem) = RouteTestTimeout(10.seconds.dilated(system))
}

trait FakeHttpClientSpec {
  class FakeHttpClient extends (HttpRequest => Future[HttpResponse]) {
    val fileUri: Uri = "http://testfile"
    lazy val fileBody = HttpEntity.apply(ContentTypes.`text/plain(UTF-8)`, "Test text 1".getBytes())

    override def apply(req: HttpRequest): Future[HttpResponse] = Future.successful {
      req match {
        case HttpRequest(_, uri, _, _, _) if uri.toString().endsWith("testfile") =>
          HttpResponse(entity = fileBody)
        case HttpRequest(_, uri, _, _, _) =>
          HttpResponse(StatusCodes.NotFound, entity = s"[fakehttpserver] $uri not found")
      }
    }
  }

  val fakeHttpClient = new FakeHttpClient
}


trait ResourceSpec extends TufReposerverSpec
  with ScalatestRouteTest
  with DatabaseSpec
  with FakeHttpClientSpec
  with LongHttpRequest
  with Directives {

  def apiUri(path: String): String = "/api/v1/" + path

  val fakeRoleStore = FakeRoleStore

  val defaultNamespaceExtractor = NamespaceDirectives.defaultNamespaceExtractor.map(_.namespace)

  val namespaceValidation = new NamespaceValidation(defaultNamespaceExtractor) {
    override def apply(repoId: RepoId): Directive1[Namespace] = defaultNamespaceExtractor
  }

  val localStorage = new LocalTargetStore(Files.createTempDirectory("target-storage").toFile)
  lazy val targetUpload = new TargetUpload(fakeRoleStore, localStorage, fakeHttpClient, messageBusPublisher)

  val memoryMessageBus = new MemoryMessageBus
  val messageBusPublisher = memoryMessageBus.publisher()

  lazy val routes = new TufReposerverRoutes(fakeRoleStore, namespaceValidation, targetUpload, messageBusPublisher).routes
}
