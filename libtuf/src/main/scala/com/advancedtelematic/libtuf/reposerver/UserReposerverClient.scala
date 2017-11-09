package com.advancedtelematic.libtuf.reposerver

import java.net.URI

import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.ClientDataType.{ETag, RootRole, TargetsRole}
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.TufDataType.{KeyId, SignedPayload, TufKey, TufPrivateKey}
import com.advancedtelematic.libtuf.http.SHttpjServiceClient
import com.advancedtelematic.libtuf.http.SHttpjServiceClient.HttpResponse
import com.advancedtelematic.libtuf.reposerver.UserReposerverClient.{EtagNotValid, TargetsResponse}
import io.circe.Decoder

import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.ClassTag
import scala.util.control.NoStackTrace
import scalaj.http.{Http, HttpRequest}

object UserReposerverClient {
  case class TargetsResponse(targets: SignedPayload[TargetsRole], etag: Option[ETag])

  case object EtagNotValid extends Exception("could not overwrite targets, trying to update an older version of role. Did you run `targets pull` ?") with NoStackTrace
}

trait UserReposerverClient {
  def root(): Future[SignedPayload[RootRole]]

  def deleteKey(keyId: KeyId): Future[TufPrivateKey]

  def pushSignedRoot(signedRoot: SignedPayload[RootRole]): Future[Unit]

  def targets(): Future[TargetsResponse]

  def pushTargets(targetsRole: SignedPayload[TargetsRole], etag: Option[ETag]): Future[Unit]

  def pushTargetsKey(key: TufKey): Future[TufKey]
}

class UserReposerverHttpClient(reposerverUri: URI,
                               httpClient: HttpRequest => Future[scalaj.http.HttpResponse[Array[Byte]]],
                               token: String)(implicit ec: ExecutionContext)
  extends SHttpjServiceClient(httpClient) with UserReposerverClient {

  override protected def execHttp[T : ClassTag : Decoder](request: HttpRequest)(errorHandler: PartialFunction[Int, Future[T]]) =
    super.execHttp(request.header("Authorization", s"Bearer $token"))(errorHandler)

  private def apiUri(path: String): String =
    URI.create(reposerverUri.toString + "/api/v1/user_repo/" + path).toString

  def root(): Future[SignedPayload[RootRole]] = {
    val req = Http(apiUri("root.json")).method("GET")
    execHttp[SignedPayload[RootRole]](req)().map(_.body)
  }

  def deleteKey(keyId: KeyId): Future[TufPrivateKey] = {
    val req = Http(apiUri("root/private_keys/" + keyId.value)).method("DELETE")
    execHttp[TufPrivateKey](req)().map(_.body)
  }

  def pushSignedRoot(signedRoot: SignedPayload[RootRole]): Future[Unit] = {
    val req = Http(apiUri("root")).method("POST")
    execJsonHttp[Unit, SignedPayload[RootRole]](req, signedRoot)()
  }

  def targets(): Future[TargetsResponse] = {
    val req = Http(apiUri("targets.json")).method("GET")
    execHttp[SignedPayload[TargetsRole]](req)().map {
      case HttpResponse(payload, response) =>
        val etag = response.header("ETag").map(ETag.apply)
        TargetsResponse(payload, etag)
    }
  }

  def pushTargets(role: SignedPayload[TargetsRole], etag: Option[ETag]): Future[Unit] = {
    val put = Http(apiUri("targets")).method("PUT")
    val req = etag.map(e => put.header("If-Match", e.value)).getOrElse(put)

    execJsonHttp[Unit, SignedPayload[TargetsRole]](req, role) {
      case 412 | 428 =>
        Future.failed(EtagNotValid)
    }
  }

  override def pushTargetsKey(key: TufKey): Future[TufKey] = {
    val req = Http(apiUri("keys/targets")).method("PUT")
    execJsonHttp[Unit, TufKey](req, key)().map(_ => key)
  }
}
