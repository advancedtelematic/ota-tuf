package com.advancedtelematic.ota_tuf.vault

import cats.syntax.show._
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpRequest, HttpResponse, Uri}
import akka.http.scaladsl.model.Uri.Path._
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.HttpMethods._
import akka.stream.Materializer
import com.advancedtelematic.ota_tuf.data.DataType.{Key, KeyId}
import com.advancedtelematic.ota_tuf.data.KeyType.KeyType
import com.advancedtelematic.ota_tuf.vault.VaultClient.VaultKey
import io.circe.Encoder

import scala.concurrent.Future
import scala.util.control.NoStackTrace
import io.circe.generic.semiauto._
import io.circe.syntax._

trait VaultClient {
  def createKey(key: VaultKey): Future[Unit]
}

object VaultClient {
  import KeyId._

  case class VaultKey(id: KeyId, keyType: KeyType, publicKey: String, privateKey: String)

  object VaultKey {
    implicit val encoder: Encoder[VaultKey] = deriveEncoder[VaultKey]
  }

  def apply(host: Uri, token: String)(implicit system: ActorSystem, mat: Materializer): VaultClient = new VaultClientImpl(host, token)
}

class VaultClientImpl(vaultHost: Uri, token: String)(implicit system: ActorSystem, mat: Materializer) extends VaultClient {
  import VaultKey._
  import system.dispatcher

  private val _http = Http()

  private val mountPath = Empty / "v1" / "ota-tuf" / "keys"

  case class VaultError(msg: String) extends Throwable(msg) with NoStackTrace

  override def createKey(key: VaultKey): Future[Unit] = {
    val req = HttpRequest(POST, vaultHost.withPath(mountPath / key.id.show))
      .withEntity(key.asJson.noSpaces)
    execute(req)
  }

  private def execute(request: HttpRequest): Future[Unit] = {
    val authRequest = request.addHeader(RawHeader("X-Vault-Token", token))

    _http.singleRequest(authRequest).flatMap {
      case HttpResponse(status, _, _, _) if status.isSuccess() =>
        Future.successful(())
      case r =>
        Future.failed(VaultError(s"Unexpected response from vault: $r"))
    }
  }
}
