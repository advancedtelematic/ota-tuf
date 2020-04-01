package com.advancedtelematic.libtuf.http

import java.io.{FileInputStream, InputStream}
import java.net.URI
import java.nio.file.Path

import scala.concurrent.duration._
import com.advancedtelematic.libats.data.DataType.ValidChecksum
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.ClientDataType.{DelegatedRoleName, RootRole, TargetsRole}
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.TufDataType.{KeyId, SignedPayload, TargetFilename, TufKeyPair}
import com.advancedtelematic.libtuf.http.CliHttpClient.CliHttpBackend
import com.advancedtelematic.libtuf.http.TufServerHttpClient.{RoleChecksumNotValid, RoleNotFound, TargetsResponse}
import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import org.slf4j.LoggerFactory
import sttp.client
import sttp.client._
import sttp.model.Uri

import scala.concurrent.{ExecutionContext, Future, _}
import scala.util.control.NoStackTrace

object TufServerHttpClient {
  case class TargetsResponse(targets: SignedPayload[TargetsRole], checksum: Option[Refined[String, ValidChecksum]])

  case object RoleChecksumNotValid extends Exception("could not overwrite targets, trying to update an older version of role. Did you run `targets pull` ?") with NoStackTrace

  case class RoleNotFound(msg: String) extends Exception(s"role not found: $msg") with NoStackTrace
}

trait TufServerClient {
  def root(version: Option[Int] = None): Future[SignedPayload[RootRole]]

  def fetchKeyPair(keyId: KeyId): Future[TufKeyPair]

  def deleteKey(keyId: KeyId): Future[Unit]

  def pushSignedRoot(signedRoot: SignedPayload[RootRole]): Future[Unit]
}

trait ReposerverClient extends TufServerClient {
  def pushDelegation(name: DelegatedRoleName, delegation: SignedPayload[TargetsRole]): Future[Unit]

  def pullDelegation(name: DelegatedRoleName): Future[SignedPayload[TargetsRole]]

  def targets(): Future[TargetsResponse]

  def pushTargets(role: SignedPayload[TargetsRole], previousChecksum: Option[Refined[String, ValidChecksum]]): Future[Unit]

  def uploadTarget(targetFilename: TargetFilename, inputPath: Path, timeout: Duration): Future[Unit]
}

trait DirectorClient extends TufServerClient

abstract class TufServerHttpClient(uri: URI, httpBackend: CliHttpBackend)
                                  (implicit ec: ExecutionContext) extends CliHttpClient(httpBackend) {

  protected def uriPath: String

  protected def apiUri(path: String): Uri =
    Uri.apply(URI.create(uri.toString + uriPath + path))

  def root(version: Option[Int] = None): Future[SignedPayload[RootRole]] = {
    val filename = version match {
      case Some(v) => v + ".root.json"
      case None => "root.json"
    }

    val req = http.get(apiUri(filename))

    execHttp[SignedPayload[RootRole]](req) {
      case (404, error) => Future.failed(RoleNotFound(error.description))
    }.map(_.body)
  }

  def pushSignedRoot(signedRoot: SignedPayload[RootRole]): Future[Unit] = {
    val req = http.post(apiUri("root"))
    execJsonHttp[Unit, SignedPayload[RootRole]](req, signedRoot)()
  }

  def fetchKeyPair(keyId: KeyId): Future[TufKeyPair]

  def deleteKey(keyId: KeyId): Future[Unit]
}

class ReposerverHttpClient(uri: URI, httpBackend: CliHttpBackend)(implicit ec: ExecutionContext)
  extends TufServerHttpClient(uri, httpBackend) with ReposerverClient {

  private val _log = LoggerFactory.getLogger(this.getClass)

  protected def uriPath: String = "/api/v1/user_repo/"

  def fetchKeyPair(keyId: KeyId): Future[TufKeyPair] = {
    val req = http.get(apiUri("root/private_keys/" + keyId.value))
    execHttp[TufKeyPair](req)().map(_.body)
  }

  def deleteKey(keyId: KeyId): Future[Unit] = {
    val req = http.delete(apiUri("root/private_keys/" + keyId.value))
    execHttp[Unit](req)().map(_.body)
  }

  def targets(): Future[TargetsResponse] = {
    val req = http.get(apiUri("targets.json"))
    execHttp[SignedPayload[TargetsRole]](req)().map { response =>
      val checksumO = response.header("x-ats-role-checksum").flatMap { v => refineV[ValidChecksum](v).toOption }
      TargetsResponse(response.body, checksumO)
    }
  }

  def pushTargets(role: SignedPayload[TargetsRole], previousChecksum: Option[Refined[String, ValidChecksum]]): Future[Unit] = {
    val put = http.put(apiUri("targets"))
    val req = previousChecksum.map(e => put.header("x-ats-role-checksum", e.value)).getOrElse(put)

    execJsonHttp[Unit, SignedPayload[TargetsRole]](req, role) {
      case (412, errorRepr) if errorRepr.code.code == "role_checksum_mismatch" =>
        Future.failed(RoleChecksumNotValid)
      case (428, _) =>
        Future.failed(RoleChecksumNotValid)
    }
  }

  override def pushDelegation(name: DelegatedRoleName, delegation: SignedPayload[TargetsRole]): Future[Unit] = {
    val req = http.put(apiUri(s"delegations/${name.value}.json"))
    execJsonHttp[Unit, SignedPayload[TargetsRole]](req, delegation)()
  }

  override def pullDelegation(name: DelegatedRoleName): Future[SignedPayload[TargetsRole]] = {
    val req = http.get(apiUri(s"delegations/${name.value}.json"))
    execHttp[SignedPayload[TargetsRole]](req)().map(_.body)
  }

  override def uploadTarget(targetFilename: TargetFilename, inputPath: Path, timeout: Duration): Future[Unit] = {
    val req = basicRequest.put(apiUri(s"uploads/" + targetFilename.value))
      .body(inputPath)
      .readTimeout(timeout)
      .followRedirects(true)
      .response(asByteArrayAlways)

    val httpF = execHttp[Unit](req)().map(_ => ())

    _log.info("Uploading file, this may take a while")

    def wait(): Future[Unit] = {
      if(httpF.isCompleted) {
        println("")
        httpF
      } else {
        Future {
          blocking {
            print(".")
            Thread.sleep(1000)
          }
        }.flatMap(_ => wait())
      }
    }

    wait()
  }
}

class DirectorHttpClient(uri: URI, httpBackend: CliHttpBackend)
                        (implicit ec: ExecutionContext) extends TufServerHttpClient(uri, httpBackend) with DirectorClient {

  // assumes talking to the Director through the API gateway
  protected def uriPath: String = "/api/v1/director/admin/repo/"

  def fetchKeyPair(keyId: KeyId): Future[TufKeyPair] = {
    val req = http.get(apiUri("private_keys/" + keyId.value))
    execHttp[TufKeyPair](req)().map(_.body)
  }

  def deleteKey(keyId: KeyId): Future[Unit] = {
    val req = http.delete(apiUri("private_keys/" + keyId.value))
    execHttp[Unit](req)().map(_.body)
  }
}
