package com.advancedtelematic.libtuf.data

import cats.syntax.either._
import akka.http.scaladsl.model.Uri
import io.circe.syntax._
import cats.syntax.either._
import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.data.TufDataType._
import io.circe._
import cats.syntax.functor._

import scala.util.Try

object TufCodecs {
  import io.circe.generic.semiauto._
  import com.advancedtelematic.libats.codecs.AkkaCirce._

  implicit val uriEncoder: Encoder[Uri] = Encoder[String].contramap(_.toString)
  implicit val uriDecoder: Decoder[Uri] = Decoder[String].map(Uri.apply)

  implicit val signatureEncoder: Encoder[Signature] = deriveEncoder
  implicit val signatureDecoder: Decoder[Signature] = deriveDecoder

  implicit val clientSignatureEncoder: Encoder[ClientSignature] = deriveEncoder
  implicit val clientSignatureDecoder: Decoder[ClientSignature] = deriveDecoder

  implicit def signedPayloadEncoder[T : Encoder]: Encoder[SignedPayload[T]] = deriveEncoder
  implicit def signedPayloadDecoder[T : Encoder : Decoder]: Decoder[SignedPayload[T]] = deriveDecoder

  implicit val checkSumEncoder: Encoder[Checksum] = deriveEncoder
  implicit val checkSumDecoder: Decoder[Checksum] = deriveDecoder

  implicit val rsaKeyTypeEncoder: Encoder[RsaKeyType.type] = Encoder[String].contramap(_ ⇒ "RSA")
  implicit val rsaKeyTypeDecoder: Decoder[RsaKeyType.type] = Decoder[String].emap(str ⇒ Either.cond(str == "RSA", RsaKeyType, "RsaKeyType"))

  implicit val edKeyTypeEncoder: Encoder[EdKeyType.type] = Encoder[String].contramap(_ ⇒ "ED25519")
  implicit val edKeyTypeDecoder: Decoder[EdKeyType.type] = Decoder[String].emap(str ⇒ Either.cond(str == "ED25519", EdKeyType, "EdcKeyType"))

  implicit val keyTypeDecoder: Decoder[KeyType] = List[Decoder[KeyType]](rsaKeyTypeDecoder.widen, edKeyTypeDecoder.widen).reduceLeft(_ or _)

  implicit val keyTypeEncoder: Encoder[KeyType] = Encoder.instance {
    case RsaKeyType ⇒ RsaKeyType.asJson
    case EdKeyType ⇒ EdKeyType.asJson
  }

  implicit val tufKeyEncoder: Encoder[TufKey] = Encoder.instance {
    case key @ RSATufKey(_) ⇒
      Json.obj("keyval" →
        Json.obj("public" -> RsaKeyType.crypto.encode(key)), "keytype" → RsaKeyType.asJson)
    case key @ EdTufKey(_) ⇒
      Json.obj("keyval" →
        Json.obj("public" -> EdKeyType.crypto.encode(key)), "keytype" → EdKeyType.asJson)
  }

  implicit val tufPrivateKeyEncoder: Encoder[TufPrivateKey] = Encoder.instance {
    case key @ RSATufPrivateKey(_) ⇒
      Json.obj("keyval" →
        Json.obj("private" -> RsaKeyType.crypto.encode(key)), "keytype" → RsaKeyType.asJson)
    case key @ EdTufPrivateKey(_) ⇒
      Json.obj("keyval" →
        Json.obj("private" -> EdKeyType.crypto.encode(key)), "keytype" → EdKeyType.asJson)
  }

  private def tufKeyDecoder[T](field: String, decodeFn: (KeyType, String) ⇒ Try[T]): Decoder[T] = Decoder.instance { cursor ⇒
    for {
      keyType ← cursor.downField("keytype").as[KeyType]
      keyVal ← cursor.downField("keyval").downField(field).as[String]
      key ← Either.fromTry(decodeFn(keyType, keyVal)).leftMap(ex => DecodingFailure(ex.getMessage, cursor.history))
    } yield key
  }

  implicit val tufKeyDecoder: Decoder[TufKey] = tufKeyDecoder("public", TufCrypto.parsePublic)

  implicit val tufPrivateKeyDecoder: Decoder[TufPrivateKey] = tufKeyDecoder("private", TufCrypto.parsePrivate)
}
