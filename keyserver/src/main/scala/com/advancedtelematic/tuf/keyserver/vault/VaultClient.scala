package com.advancedtelematic.tuf.keyserver.vault

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.Uri.Path._
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model.Uri.Path
import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, Unmarshaller}
import akka.stream.Materializer
import com.advancedtelematic.libtuf.data.TufDataType.{EdKeyType, EdTufKey, KeyId, KeyType, RSATufPrivateKey, TufKey, TufKeyPair, TufPrivateKey}
import com.advancedtelematic.tuf.keyserver.vault.VaultClient.{VaultKey, VaultKeyNotFound}
import io.circe.{Decoder, Encoder, HCursor}

import scala.concurrent.Future
import scala.reflect.ClassTag
import scala.util.control.NoStackTrace
import scala.util.Try
import io.circe.generic.semiauto._
import io.circe.syntax._
import cats.syntax.either._
import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.data.TufCodecs
import cats.syntax.functor._
import org.bouncycastle.util.encoders.{Base64, Hex}

trait VaultClient {
  def createKey(key: VaultKey): Future[Unit]

  def findKey(keyId: KeyId): Future[VaultKey]

  def deleteKey(keyId: KeyId): Future[Unit]

  def renewToken(): Future[Unit]
}

object VaultClient {
  import com.advancedtelematic.libats.codecs.CirceCodecs._
  import TufCodecs._

  case class VaultKey(id: KeyId, keyType: KeyType, publicKey: TufKey, privateKey: TufPrivateKey) {
    def toTufKeyPair: Try[TufKeyPair] = keyType.crypto.castToKeyPair(publicKey, privateKey)
  }

  case object VaultKeyNotFound extends Exception("vault key not found") with NoStackTrace

  object VaultKey {
    implicit val encoder: Encoder[VaultKey] = deriveEncoder[VaultKey]

    private val legacyPrivateKeyDecoder = Decoder[String].emapTry { str ⇒
      TufCrypto.parsePrivatePem(str).map(RSATufPrivateKey)
    }

    private def legacyPublicKeyDecoder(keyType: KeyType) = Decoder[String].emapTry { str ⇒
      keyType.crypto.parsePublic(str)
    }

    private def legacyPemPublicKeyDecoder(keyType: KeyType) = Decoder[String].emapTry { str ⇒
      TufCrypto.parsePublicPem(str).map(keyVal => keyType.crypto.convert(keyVal))
    }

    private def lenientPublicKeyDecoder(keyType: KeyType): Decoder[TufKey] = TufCodecs.tufKeyDecoder or legacyPublicKeyDecoder(keyType).widen or legacyPemPublicKeyDecoder(keyType).widen

    private val lenientPrivateKeyDecoder: Decoder[TufPrivateKey] = TufCodecs.tufPrivateKeyDecoder or legacyPrivateKeyDecoder.widen

    implicit val decoder: Decoder[VaultKey] = Decoder.instance { cursor ⇒
      for {
        keyId ← cursor.downField("id").as[KeyId]
        keyType ← cursor.downField("keyType").as[KeyType]
        publicKey ← cursor.downField("publicKey").as[TufKey](lenientPublicKeyDecoder(keyType))
        privateKey ← cursor.downField("privateKey").as[TufPrivateKey](lenientPrivateKeyDecoder)
      } yield VaultKey(keyId, keyType, publicKey, privateKey)
    }
  }

  def apply(host: Uri, token: String, mount: Path)(implicit system: ActorSystem, mat: Materializer): VaultClient =
    new VaultHttpClient(host, token, mount)
}

class VaultHttpClient(vaultHost: Uri, token: String, mount: Path)(implicit system: ActorSystem, mat: Materializer) extends VaultClient {
  import VaultKey._
  import system.dispatcher

  private val _http = Http()

  private val mountPath = Empty / "v1" ++ Slash(mount)

  case class VaultError(msg: String) extends Exception(msg) with NoStackTrace

  override def createKey(key: VaultKey): Future[Unit] = {
    val req = HttpRequest(POST, vaultHost.withPath(mountPath / key.id.value))
      .withEntity(key.asJson.noSpaces)
    execute[Unit](req)
  }

  override def findKey(keyId: KeyId): Future[VaultKey] = {
    val req = HttpRequest(GET, vaultHost.withPath(mountPath / keyId.value))
    execute[VaultKey](req)
  }

  override def deleteKey(keyId: KeyId): Future[Unit] = {
    val req = HttpRequest(DELETE, vaultHost.withPath(mountPath / keyId.value))
    execute[Unit](req)
  }

  override def renewToken(): Future[Unit] = {
    val req = HttpRequest(POST, vaultHost.withPath(Path("/v1") / "auth" / "token" / "renew-self"))
    execute[Unit](req)
  }

  private def execute[T](request: HttpRequest)(implicit um: FromEntityUnmarshaller[T]): Future[T] = {
    val authRequest = request.addHeader(RawHeader("X-Vault-Token", token))

    _http.singleRequest(authRequest).flatMap {
      case r @ HttpResponse(status, _, _, _) if status.isSuccess() =>
        um(r.entity)
      case HttpResponse(StatusCodes.NotFound, _, _, _) =>
        Future.failed(VaultKeyNotFound)
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
            .map(j => HCursor.fromJson(j).downField("data"))
            .flatMap(_.as[T])
            .valueOr(throw _)
        }
  }
}
