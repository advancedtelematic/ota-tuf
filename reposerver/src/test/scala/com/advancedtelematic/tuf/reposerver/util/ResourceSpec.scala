package com.advancedtelematic.tuf.reposerver.util

import java.nio.file.Files
import java.security.PublicKey
import java.time.Instant
import java.time.temporal.ChronoUnit
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
import akka.http.scaladsl.server.{Directive1, Directives, Route}
import akka.http.scaladsl.util.FastFuture
import com.advancedtelematic.libtuf.data.ClientCodecs._

import scala.concurrent.duration._
import scala.collection.JavaConverters._
import scala.util.Try
import akka.testkit.TestDuration
import com.advancedtelematic.libats.auth.NamespaceDirectives
import com.advancedtelematic.libats.data.DataType.Namespace
import com.advancedtelematic.libats.messaging.MemoryMessageBus
import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.libtuf.data.TufDataType.RoleType
import com.advancedtelematic.libtuf.data.ClientDataType.{RoleKeys, RootRole}
import com.advancedtelematic.libtuf.data.TufDataType.RepoId
import com.advancedtelematic.libtuf_server.keyserver.KeyserverClient
import com.advancedtelematic.tuf.reposerver.http.{NamespaceValidation, TufReposerverRoutes}
import com.advancedtelematic.tuf.reposerver.target_store.{LocalTargetStoreEngine, TargetStore}

import scala.concurrent.Promise
import cats.syntax.either._
import com.advancedtelematic.libats.http.tracing.NullRequestTracing

import scala.concurrent.ExecutionContext.Implicits.global

class FakeKeyserverClient extends KeyserverClient {

  import KeyserverClient._
  import io.circe.syntax._

  private val keys = new ConcurrentHashMap[RepoId, Map[RoleType, TufKeyPair]]()

  private val rootRoles = new ConcurrentHashMap[RepoId, SignedPayload[RootRole]]()

  private val pendingRequests = new ConcurrentHashMap[RepoId, Boolean]()

  def publicKey(repoId: RepoId, roleType: RoleType): TufKey = keys.get(repoId)(roleType).pubkey

  def resetKeyServer(): Unit = this.synchronized {
    keys.clear()
    rootRoles.clear()
  }

  def forceKeyGenerationPending(repoId: RepoId): Unit = {
    deleteRepo(repoId)
    pendingRequests.put(repoId, true)
  }

  private lazy val preGeneratedKeys: Map[KeyType, Map[RoleType, TufKeyPair]] =
    Seq(RsaKeyType, Ed25519KeyType).map { keyType =>
      keyType -> RoleType.ALL.map { role =>
        role -> keyType.crypto.generateKeyPair()
      }.toMap
    }.toMap

  def updateRepoKeys(repoId: RepoId, role: RoleType, keyPair: TufKeyPair): Map[RoleType, TufKeyPair] =
    keys.compute(repoId, (t: RepoId, u: Map[RoleType, TufKeyPair]) => {
      if (u == null)
        Map(role -> keyPair)
      else
        u + (role -> keyPair)
    })

  private def generateRoot(repoId: RepoId, keyType: KeyType): RootRole = {
    updateRepoKeys(repoId, RoleType.ROOT, keyType.crypto.generateKeyPair())

    val roles = keys.get(repoId).map { case (role, keyPair) =>
      role -> RoleKeys(List(keyPair.pubkey.id), threshold = 1)
    }

    val clientKeys = keys.get(repoId).map { case (_, keyPair) =>
      keyPair.pubkey.id -> keyPair.pubkey
    }

    // expires truncated to seconds since circe codecs will code it that way, we cannot save it with more precision than that
    RootRole(clientKeys, roles, expires = Instant.now.plusSeconds(3600).truncatedTo(ChronoUnit.SECONDS), version = 1)
  }

  private def generateKeys(repoId: RepoId, keyType: KeyType): List[TufKeyPair] = {
    preGeneratedKeys(keyType).map { case (role, keyPair) =>
      updateRepoKeys(repoId, role, keyPair)

      keyPair
    }
  }.toList

  def deleteRepo(repoId: RepoId): Option[RootRole] =
    keys.asScala.remove(repoId).flatMap(_ => rootRoles.asScala.remove(repoId).map(_.signed))

  override def createRoot(repoId: RepoId, keyType: KeyType, forceSync: Boolean): Future[Json] = {
    if (keys.contains(repoId)) {
      FastFuture.failed(RootRoleConflict)
    } else {
      generateKeys(repoId, keyType)
      val rootRole = generateRoot(repoId, keyType)
      sign(repoId, RoleType.ROOT, rootRole.asJson).map { jsonSignedPayload =>
        val signedPayload = SignedPayload(jsonSignedPayload.signatures, rootRole, jsonSignedPayload.signed)
        rootRoles.put(repoId, signedPayload)
        rootRole.asJson
      }
    }
  }

  override def sign(repoId: RepoId, roleType: RoleType, payload: Json): Future[JsonSignedPayload] = {
    val okey = keys.asScala.get(repoId).flatMap(_.get(roleType))
    val fkey = okey.map(FastFuture.successful).getOrElse(FastFuture.failed(RoleKeyNotFound))

    fkey.map { tufKeyPair =>
      val signature = TufCrypto.signPayload(tufKeyPair.privkey, payload).toClient(tufKeyPair.pubkey.id)
      JsonSignedPayload(List(signature), payload)
    }
  }

  override def fetchRootRole(repoId: RepoId): Future[SignedPayload[RootRole]] =
    Future.fromTry {
      Try {
        if(pendingRequests.asScala.getOrElse(repoId, false))
          throw KeysNotReady

        rootRoles.asScala(repoId)
      }.recover {
        case _: NoSuchElementException => throw RootRoleNotFound
      }
    }

  override def fetchUnsignedRoot(repoId: RepoId): Future[RootRole] =
    fetchRootRole(repoId).map(_.signed)

  override def updateRoot(repoId: RepoId, newRoot: SignedPayload[RootRole]): Future[Unit] = FastFuture.successful {
    rootRoles.computeIfPresent(repoId, (t: RepoId, oldRoot: SignedPayload[RootRole]) => {
      assert(oldRoot != null, "fake keyserver, Role does not exist")
      newRoot
    })
  }

  override def deletePrivateKey(repoId: RepoId, keyId: KeyId): Future[Unit] = FastFuture.successful {
    keys.asScala.get(repoId).flatMap(_.values.find(_.pubkey.id == keyId)).getOrElse(throw RoleKeyNotFound)

    keys.computeIfPresent(repoId, (id: RepoId, existingKeys: Map[RoleType, TufKeyPair]) => {
      existingKeys.filter(_._2.pubkey.id != keyId)
    })
  }

  override def fetchTargetKeyPairs(repoId: RepoId): Future[Seq[TufKeyPair]] =  FastFuture.successful {
    Seq(keys.asScala.getOrElse(repoId, throw RoleKeyNotFound).getOrElse(RoleType.TARGETS, throw RoleKeyNotFound))
  }

  override def fetchRootRole(repoId: RepoId, version: Int): Future[SignedPayload[RootRole]] =
    fetchRootRole(repoId).filter(_.signed.version == version)

  override def fetchKeyPair(repoId: RepoId, keyId: KeyId): Future[TufKeyPair] = Future.fromTry { Try {
    keys.asScala.get(repoId).flatMap(_.values.find(_.pubkey.id == keyId)).getOrElse(throw KeyPairNotFound)
  } }
}

trait LongHttpRequest {
  implicit def default(implicit system: ActorSystem) = RouteTestTimeout(60.seconds.dilated(system))
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

trait HttpClientSpecSupport {
  self: ResourceSpec =>

  def testHttpClient(req: akka.http.scaladsl.model.HttpRequest): Future[akka.http.scaladsl.model.HttpResponse] = {
    val p = Promise[akka.http.scaladsl.model.HttpResponse]()
    req ~> Route.seal(routes) ~> check { p.success(response) }
    p.future
  }
}

trait ResourceSpec extends TufReposerverSpec
  with ScalatestRouteTest
  with DatabaseSpec
  with FakeHttpClientSpec
  with LongHttpRequest
  with Directives {

  def apiUri(path: String): String = "/api/v1/" + path

  val fakeKeyserverClient: FakeKeyserverClient = new FakeKeyserverClient

  val defaultNamespaceExtractor = NamespaceDirectives.defaultNamespaceExtractor.map(_.namespace)

  val namespaceValidation = new NamespaceValidation(defaultNamespaceExtractor) {
    override def apply(repoId: RepoId): Directive1[Namespace] = defaultNamespaceExtractor
  }

  val storageRoot = Files.createTempDirectory("target-storage").toFile
  val localStorage = new LocalTargetStoreEngine(storageRoot)
  lazy val targetStore = new TargetStore(fakeKeyserverClient, localStorage, fakeHttpClient, messageBusPublisher)

  val memoryMessageBus = new MemoryMessageBus
  val messageBusPublisher = memoryMessageBus.publisher()

  lazy val routes = new TufReposerverRoutes(fakeKeyserverClient, namespaceValidation, targetStore, messageBusPublisher).routes

  implicit lazy val tracing = new NullRequestTracing
}
