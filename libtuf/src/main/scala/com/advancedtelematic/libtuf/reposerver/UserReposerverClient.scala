package com.advancedtelematic.libtuf.reposerver

import java.net.URI

import com.advancedtelematic.libats.data.DataType.ValidChecksum
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.ClientDataType.{RootRole, TargetsRole}
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.TufDataType.{KeyId, SignedPayload, TufKeyPair}
import com.advancedtelematic.libtuf.http.SHttpjServiceClient
import com.advancedtelematic.libtuf.http.SHttpjServiceClient.HttpResponse
import com.advancedtelematic.libtuf.reposerver.UserReposerverClient.{RoleChecksumNotValid, RoleNotFound, TargetsResponse}
import eu.timepit.refined.api.Refined
import io.circe.Decoder

import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.ClassTag
import scala.util.control.NoStackTrace
import scalaj.http.{Http, HttpRequest}
import eu.timepit.refined._
import com.advancedtelematic.libats.data.ErrorRepresentation

object UserReposerverClient {
  case class TargetsResponse(targets: SignedPayload[TargetsRole], checksum: Option[Refined[String, ValidChecksum]])

  case object RoleChecksumNotValid extends Exception("could not overwrite targets, trying to update an older version of role. Did you run `targets pull` ?") with NoStackTrace

  case class RoleNotFound(msg: String) extends Exception(s"role not found: $msg") with NoStackTrace
}

trait UserReposerverClient {
  def root(version: Option[Int] = None): Future[SignedPayload[RootRole]]

  def fetchKeyPair(keyId: KeyId): Future[TufKeyPair]

  def deleteKey(keyId: KeyId): Future[Unit]

  def pushSignedRoot(signedRoot: SignedPayload[RootRole]): Future[Unit]

  def targets(): Future[TargetsResponse]

  def pushTargets(targetsRole: SignedPayload[TargetsRole], previousChecksum: Option[Refined[String, ValidChecksum]]): Future[Unit]
}

class UserReposerverHttpClient(reposerverUri: URI,
                               httpClient: HttpRequest => Future[scalaj.http.HttpResponse[Array[Byte]]],
                               token: Option[String])(implicit ec: ExecutionContext)
  extends SHttpjServiceClient(httpClient) with UserReposerverClient {

  override protected def execHttp[T : ClassTag : Decoder](request: HttpRequest)(errorHandler: PartialFunction[(Int, ErrorRepresentation), Future[T]]) =
    token match {
      case Some(t) =>
        super.execHttp(request.header("Authorization", s"Bearer $t"))(errorHandler)
      case None =>
        super.execHttp(request)(errorHandler)
    }

  private def apiUri(path: String): String =
    URI.create(reposerverUri.toString + "/api/v1/user_repo/" + path).toString

  def root(version: Option[Int] = None): Future[SignedPayload[RootRole]] = {
    val filename = version match {
      case Some(v) => v + ".root.json"
      case None => "root.json"
    }
    val req = Http(apiUri(filename)).method("GET")
    execHttp[SignedPayload[RootRole]](req) {
      case (404, error) => Future.failed(RoleNotFound(error.description))
    }.map(_.body)
  }

  override def fetchKeyPair(keyId: KeyId): Future[TufKeyPair] = {
    val req = Http(apiUri("root/private_keys/" + keyId.value))
    execHttp[TufKeyPair](req)().map(_.body)
  }

  def deleteKey(keyId: KeyId): Future[Unit] = {
    val req = Http(apiUri("root/private_keys/" + keyId.value)).method("DELETE")
    execHttp[Unit](req)().map(_.body)
  }

  def pushSignedRoot(signedRoot: SignedPayload[RootRole]): Future[Unit] = {
    val req = Http(apiUri("root")).method("POST")
    execJsonHttp[Unit, SignedPayload[RootRole]](req, signedRoot)()
  }

  def targets(): Future[TargetsResponse] = {
    val req = Http(apiUri("targets.json")).method("GET")
    execHttp[SignedPayload[TargetsRole]](req)().map {
      case HttpResponse(payload, response) =>
        val checksumO = response.header("x-ats-role-checksum").flatMap { v => refineV[ValidChecksum](v).toOption }
        TargetsResponse(payload, checksumO)
    }
  }

  def pushTargets(role: SignedPayload[TargetsRole], previousChecksum: Option[Refined[String, ValidChecksum]]): Future[Unit] = {
    val put = Http(apiUri("targets")).method("PUT")
    val req = previousChecksum.map(e => put.header("x-ats-role-checksum", e.value)).getOrElse(put)

    execJsonHttp[Unit, SignedPayload[TargetsRole]](req, role) {
      case (412, errorRepr) if errorRepr.code.code == "role_checksum_mismatch" =>
        Future.failed(RoleChecksumNotValid)
      case (428, _) =>
        Future.failed(RoleChecksumNotValid)
    }
  }
}
