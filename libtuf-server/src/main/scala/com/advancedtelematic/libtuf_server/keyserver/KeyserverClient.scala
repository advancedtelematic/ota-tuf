package com.advancedtelematic.libtuf_server.keyserver

import akka.actor.ActorSystem
import akka.http.scaladsl.model.Uri.Path
import akka.http.scaladsl.model.Uri.Path.{Empty, Slash}
import akka.http.scaladsl.model.{StatusCodes, _}
import akka.stream.ActorMaterializer
import cats.syntax.show._
import com.advancedtelematic.libats.data.ErrorCode
import com.advancedtelematic.libats.http.Errors.RawError
import com.advancedtelematic.libtuf.data.ClientDataType.RootRole
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.libtuf.data.TufDataType.{KeyId, KeyType, RepoId, RsaKeyType, SignedPayload, TufKey, TufPrivateKey, TufKeyPair}
import com.advancedtelematic.libtuf.data.TufDataType.RoleType._
import io.circe.{Decoder, Encoder, Json}

import scala.concurrent.Future
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf_server.http.{ServiceHttpClient, ServiceHttpClientSupport}

object KeyserverClient {
  val KeysNotReady = RawError(ErrorCode("keys_not_ready"), StatusCodes.Locked, "Keys not ready in remote keyserver")
  val RootRoleNotFound = RawError(ErrorCode("root_role_not_found"), StatusCodes.FailedDependency, "root role was not found in upstream key store")
  val RootRoleConflict = RawError(ErrorCode("root_role_conflict"), StatusCodes.Conflict, "root role already exists")
  val RoleKeyNotFound = RawError(ErrorCode("role_key_not_found"), StatusCodes.PreconditionFailed, s"can't sign since role was not found in upstream key store")
  val KeyError = RawError(ErrorCode("key_error"), StatusCodes.BadRequest, "key cannot be processed")
  val KeyPairNotFound = RawError(ErrorCode("keypair_not_found"), StatusCodes.NotFound, "keypair not found in keyserver")
}

trait KeyserverClient {
  def createRoot(repoId: RepoId, keyType: KeyType = RsaKeyType): Future[Json]

  def sign[T : Decoder : Encoder](repoId: RepoId, roleType: RoleType, payload: T): Future[SignedPayload[T]]

  def fetchRootRole(repoId: RepoId): Future[SignedPayload[RootRole]]

  def fetchRootRole(repoId: RepoId, version: Int): Future[SignedPayload[RootRole]]

  def addTargetKey(repoId: RepoId, key: TufKey): Future[Unit]

  def fetchUnsignedRoot(repoId: RepoId): Future[RootRole]

  def updateRoot(repoId: RepoId, signedPayload: SignedPayload[RootRole]): Future[Unit]

  def deletePrivateKey(repoId: RepoId, keyId: KeyId): Future[Unit]

  def fetchKeyPair(repoId: RepoId, keyId: KeyId): Future[TufKeyPair]

  def fetchTargetKeyPairs(repoId: RepoId): Future[Seq[TufKeyPair]]
}

object KeyserverHttpClient extends ServiceHttpClientSupport {
  def apply(uri: Uri)(implicit system: ActorSystem, mat: ActorMaterializer): KeyserverHttpClient =
    new KeyserverHttpClient(uri, defaultHttpClient)
}

class KeyserverHttpClient(uri: Uri, httpClient: HttpRequest => Future[HttpResponse])
                         (implicit system: ActorSystem, mat: ActorMaterializer) extends ServiceHttpClient(httpClient) with KeyserverClient {
  import io.circe.syntax._
  import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
  import KeyserverClient._

  private def apiUri(path: Path) =
    uri.withPath(Empty / "api" / "v1" ++ Slash(path))

  override def createRoot(repoId: RepoId, keyType: KeyType): Future[Json] = {
    val entity = Json.obj("threshold" -> 1.asJson, "keyType" -> keyType.asJson)
    val req = HttpRequest(HttpMethods.POST, uri = apiUri(Path("root") / repoId.show))

    execJsonHttp[Json, Json](req, entity) {
      case StatusCodes.Conflict =>
        Future.failed(RootRoleConflict)
      case StatusCodes.Locked =>
        Future.failed(KeysNotReady)
    }
  }

  override def sign[T : Decoder : Encoder](repoId: RepoId, roleType: RoleType, payload: T): Future[SignedPayload[T]] = {
    val req = HttpRequest(HttpMethods.POST, uri = apiUri(Path("root") / repoId.show / roleType.show))
    execJsonHttp[SignedPayload[T], T](req, payload) {
      case StatusCodes.PreconditionFailed =>
        Future.failed(RoleKeyNotFound)
    }
  }

  override def fetchRootRole(repoId: RepoId): Future[SignedPayload[RootRole]] = {
    val req = HttpRequest(HttpMethods.GET, uri = apiUri(Path("root") / repoId.show))

    execHttp[SignedPayload[RootRole]](req) {
      case StatusCodes.NotFound =>
        Future.failed(RootRoleNotFound)
      case StatusCodes.Locked =>
        Future.failed(KeysNotReady)
    }
  }

  override def addTargetKey(repoId: RepoId, key: TufKey): Future[Unit] = {
    val req = HttpRequest(HttpMethods.PUT, uri = apiUri(Path("root") / repoId.show / "keys" / "targets"))
    execJsonHttp[Unit, TufKey](req, key)()
  }

  override def fetchUnsignedRoot(repoId: RepoId): Future[RootRole] = {
    val req = HttpRequest(HttpMethods.GET, uri = apiUri(Path("root") / repoId.show / "unsigned"))
    execHttp[RootRole](req)()
  }

  override def updateRoot(repoId: RepoId, signedPayload: SignedPayload[RootRole]): Future[Unit] = {
    val req = HttpRequest(HttpMethods.POST, uri = apiUri(Path("root") / repoId.show / "unsigned"))
    execJsonHttp[Unit, SignedPayload[RootRole]](req, signedPayload) {
      case StatusCodes.BadRequest => Future.failed(KeyError)
    }
  }

  override def deletePrivateKey(repoId: RepoId, keyId: KeyId): Future[Unit] = {
    val req = HttpRequest(HttpMethods.DELETE, uri = apiUri(Path("root") / repoId.show / "private_keys" / keyId.value))
    execHttp[Unit](req)()
  }

  override def fetchTargetKeyPairs(repoId: RepoId): Future[Seq[TufKeyPair]] = {
    val req = HttpRequest(HttpMethods.GET, uri = apiUri(Path("root") / repoId.show / "keys" / "targets" / "pairs"))
    execHttp[Seq[TufKeyPair]](req)()
  }

  override def fetchRootRole(repoId: RepoId, version: Int): Future[SignedPayload[RootRole]] = {
    val req = HttpRequest(HttpMethods.GET, uri = apiUri(Path("root") / repoId.show / version.toString))

    execHttp[SignedPayload[RootRole]](req) {
      case StatusCodes.NotFound =>
        Future.failed(RootRoleNotFound)
    }
  }

  override def fetchKeyPair(repoId: RepoId, keyId: KeyId): Future[TufKeyPair] = {
    val req = HttpRequest(HttpMethods.GET, uri = apiUri(Path("root") / repoId.show / "keys" / keyId.value))

    execHttp[TufKeyPair](req) {
      case StatusCodes.NotFound =>
        Future.failed(KeyPairNotFound)
    }
  }
}
