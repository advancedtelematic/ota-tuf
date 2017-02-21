package com.advancedtelematic.tuf.keyserver.vault

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.Uri.Path._
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, Unmarshaller}
import akka.stream.Materializer
import com.advancedtelematic.libtuf.data.TufDataType.KeyId
import com.advancedtelematic.libtuf.data.TufDataType.KeyType.KeyType
import com.advancedtelematic.tuf.keyserver.vault.VaultClient.VaultKey
import io.circe.{Decoder, Encoder, HCursor}

import scala.concurrent.Future
import scala.util.control.NoStackTrace
import io.circe.generic.semiauto._
import io.circe.syntax._

import scala.reflect.ClassTag

trait VaultClient {
  def createKey(key: VaultKey): Future[Unit]

  def findKey(keyId: KeyId): Future[VaultKey]
}

object VaultClient {
  import com.advancedtelematic.libats.codecs.AkkaCirce._

  case class VaultKey(id: KeyId, keyType: KeyType, publicKey: String, privateKey: String)

  object VaultKey {
    implicit val encoder: Encoder[VaultKey] = deriveEncoder[VaultKey]
    implicit val decoder: Decoder[VaultKey] = deriveDecoder[VaultKey]
  }

  def apply(host: Uri, token: String, mount: String)(implicit system: ActorSystem, mat: Materializer): VaultClient =
    new VaultHttpClient(host, token, mount)
}

class VaultHttpClient(vaultHost: Uri, token: String, mount: String)(implicit system: ActorSystem, mat: Materializer) extends VaultClient {
  import VaultKey._
  import system.dispatcher

  private val _http = Http()

  private val mountPath = Empty / "v1" / mount

  case class VaultError(msg: String) extends Throwable(msg) with NoStackTrace

  override def createKey(key: VaultKey): Future[Unit] = {
    val req = HttpRequest(POST, vaultHost.withPath(mountPath / key.id.get))
      .withEntity(key.asJson.noSpaces)
    execute[Unit](req)
  }

  override def findKey(keyId: KeyId): Future[VaultKey] = {
    val req = HttpRequest(GET, vaultHost.withPath(mountPath / keyId.get))
    execute[VaultKey](req)
  }

  private def execute[T](request: HttpRequest)
                        (implicit ct: ClassTag[T], um: FromEntityUnmarshaller[T]): Future[T] = {
    val authRequest = request.addHeader(RawHeader("X-Vault-Token", token))

    _http.singleRequest(authRequest).flatMap {
      case r @ HttpResponse(status, _, _, _) if status.isSuccess() =>
        um(r.entity)
      case r =>
        Future.failed(VaultError(s"Unexpected response from vault: $r"))
    }
  }

  implicit private def vaultResponseUnmarshaller[T](implicit dec: Decoder[T], ct: ClassTag[T]): FromEntityUnmarshaller[T] = {
    if(ct.runtimeClass == classOf[Unit])
      Unmarshaller.strict(_ => ().asInstanceOf[T])
    else
      Unmarshaller
        .stringUnmarshaller
        .forContentTypes(ContentTypes.`application/json`)
        .map { data =>
          io.circe.parser
            .parse(data)
            .map(j => HCursor.fromCursor(j.cursor).downField("data"))
            .flatMap(_.as[T])
            .valueOr(throw _)
        }
  }
}
