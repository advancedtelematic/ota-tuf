package com.advancedtelematic.libtuf.reposerver

import akka.actor.ActorSystem
import akka.http.scaladsl.model.Uri.Path
import akka.http.scaladsl.model.Uri.Path.Slash
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import akka.stream.Materializer
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.ClientDataType.{RootRole, TargetsRole}
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.TufDataType.{KeyId, SignedPayload, TufKey, TufPrivateKey}
import com.advancedtelematic.libtuf.http.ServiceHttpClient
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._

import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.ClassTag

trait UserReposerverClient {
  def root(): Future[SignedPayload[RootRole]]

  def deleteKey(keyId: KeyId): Future[TufPrivateKey]

  def pushSignedRoot(signedRoot: SignedPayload[RootRole]): Future[Unit]

  def targets(): Future[SignedPayload[TargetsRole]]

  def pushTargets(targetsRole: SignedPayload[TargetsRole]): Future[Unit]

  def pushTargetsKey(key: TufKey): Future[TufKey]
}

class UserReposerverHttpClient(reposerverUri: Uri,
                               httpClient: HttpRequest => Future[HttpResponse],
                               token: String)
                              (implicit ec: ExecutionContext, system: ActorSystem, mat: Materializer)
  extends ServiceHttpClient(httpClient) with UserReposerverClient {

  lazy val authHeader = RawHeader("Authorization", s"Bearer $token")

  override protected def execHttp[T: ClassTag](request: HttpRequest)(errorHandler: PartialFunction[StatusCode, Future[T]])(implicit um: FromEntityUnmarshaller[T]) =
    super.execHttp(request.addHeader(authHeader))(errorHandler)

  private def apiUri(path: Path) =
    reposerverUri.withPath(Path("/api") / "v1" / "user_repo" ++ Slash(path))

  def root(): Future[SignedPayload[RootRole]] = {
    val req = HttpRequest(HttpMethods.GET, apiUri(Path("root.json")))
    execHttp[SignedPayload[RootRole]](req)()
  }

  def deleteKey(keyId: KeyId): Future[TufPrivateKey] = {
    val req = HttpRequest(HttpMethods.DELETE, apiUri(Path("root") / "private_keys" / keyId.value))
    execHttp[TufPrivateKey](req)()
  }

  def pushSignedRoot(signedRoot: SignedPayload[RootRole]): Future[Unit] = {
    val req = HttpRequest(HttpMethods.POST, apiUri(Path("root")))
    execJsonHttp[Unit, SignedPayload[RootRole]](req, signedRoot)()
  }

  def targets(): Future[SignedPayload[TargetsRole]] = {
    val req = HttpRequest(HttpMethods.GET, apiUri(Path("targets.json")))
    execHttp[SignedPayload[TargetsRole]](req)()
  }

  def pushTargets(role: SignedPayload[TargetsRole]): Future[Unit] = {
    val req = HttpRequest(HttpMethods.PUT, apiUri(Path("targets")))
    execJsonHttp[Unit, SignedPayload[TargetsRole]](req, role)()
  }

  override def pushTargetsKey(key: TufKey): Future[TufKey] = {
    val req = HttpRequest(HttpMethods.PUT, apiUri(Path("keys") / "targets"))
    execJsonHttp[Unit, TufKey](req, key)().map(_ => key)
  }
}
