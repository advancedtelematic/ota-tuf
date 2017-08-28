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
import com.advancedtelematic.tuf.reposerver.target_store.{LocalTargetStoreEngine, TargetStore}

object FakeKeyserverClient extends KeyserverClient {

  import scala.concurrent.ExecutionContext.Implicits.global

  private val keys = new ConcurrentHashMap[RepoId, KeyPair]()

  private val rootRoles = new ConcurrentHashMap[RepoId, RootRole]()

  def publicKey(repoId: RepoId): PublicKey =
    keys.asScala(repoId).getPublic

  private def keyPair(repoId: RepoId): KeyPair =
    keys.asScala(repoId)

  private def generateRoot(repoId: RepoId): RootRole = {
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

  def deleteRepo(repoId: RepoId): Option[KeyPair] =
    Option(keys.remove(repoId))

  override def createRoot(repoId: RepoId, keyType: KeyType): Future[Json] = {
    if (keys.contains(repoId)) {
      FastFuture.failed(RootRoleConflict)
    } else {
      generateKey(repoId)
      rootRoles.put(repoId, generateRoot(repoId))
      FastFuture.successful(Json.obj())
    }
  }

  override def sign[T: Decoder : Encoder](repoId: RepoId, roleType: RoleType, payload: T): Future[SignedPayload[T]] = {
    val signature = signWithRoot(repoId, payload)
    FastFuture.successful(SignedPayload(List(signature), payload))
  }

  override def fetchRootRole(repoId: RepoId): Future[SignedPayload[RootRole]] = Future.fromTry {
    Try {
      val role = rootRoles.asScala(repoId)
      val signature = signWithRoot(repoId, role)
      SignedPayload(List(signature), role)
    }.recover {
      case _: NoSuchElementException => throw RootRoleNotFound
    }
  }

  private def signWithRoot[T : Encoder](repoId: RepoId, payload: T): ClientSignature = {
    val key = keyPair(repoId)
    TufCrypto.signPayload(RSATufPrivateKey(key.getPrivate), payload).toClient(key.getPublic.id)
  }

  override def addTargetKey(repoId: RepoId, key: TufKey): Future[Unit] = {
    if(!rootRoles.containsKey(repoId))
      FastFuture.failed(RootRoleNotFound)
    else {
      rootRoles.computeIfPresent(repoId, (_: RepoId, role: RootRole) => {
        val newKeys = role.keys + (key.id -> key)
        val targetRoleKeys = role.roles(RoleType.TARGETS)
        val newTargetKeys = RoleKeys(targetRoleKeys.keyids :+ key.id, targetRoleKeys.threshold)

        role.copy(keys = newKeys, roles = role.roles + (RoleType.TARGETS -> newTargetKeys))
      })

      FastFuture.successful(())
    }
  }

  override def fetchUnsignedRoot(repoId: RepoId): Future[RootRole] = fetchRootRole(repoId).map(_.signed)

  override def updateRoot(repoId: RepoId, signedPayload: SignedPayload[RootRole]): Future[Unit] = FastFuture.successful {
    rootRoles.computeIfPresent(repoId, (t: RepoId, u: RootRole) => {
      assert(u != null, "fake keyserver, Role does not exist")
      signedPayload.signed
    })
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

  val fakeKeyserverClient = FakeKeyserverClient

  val defaultNamespaceExtractor = NamespaceDirectives.defaultNamespaceExtractor.map(_.namespace)

  val namespaceValidation = new NamespaceValidation(defaultNamespaceExtractor) {
    override def apply(repoId: RepoId): Directive1[Namespace] = defaultNamespaceExtractor
  }

  val localStorage = new LocalTargetStoreEngine(Files.createTempDirectory("target-storage").toFile)
  lazy val targetStore = new TargetStore(fakeKeyserverClient, localStorage, fakeHttpClient, messageBusPublisher)

  val memoryMessageBus = new MemoryMessageBus
  val messageBusPublisher = memoryMessageBus.publisher()

  lazy val routes = new TufReposerverRoutes(fakeKeyserverClient, namespaceValidation, targetStore, messageBusPublisher).routes
}
