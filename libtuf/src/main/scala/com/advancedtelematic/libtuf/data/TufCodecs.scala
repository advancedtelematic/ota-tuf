package com.advancedtelematic.libtuf.data

import cats.syntax.either._
import io.circe.syntax._
import cats.syntax.either._
import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.data.TufDataType._
import io.circe._
import cats.syntax.functor._
import com.advancedtelematic.libtuf.data.TufDataType.SignatureMethod.SignatureMethod

import scala.util.Try

object TufCodecs {
  import io.circe.generic.semiauto._
  import com.advancedtelematic.libats.codecs.CirceCodecs._

  implicit val signatureMethodEncoder: Encoder[SignatureMethod] = Encoder.enumEncoder(SignatureMethod)

  implicit val signatureMethodLenientDecoder: Decoder[SignatureMethod] = Decoder.decodeString.flatMap {
    case "rsassa-pss" => Decoder.const(SignatureMethod.RSASSA_PSS_SHA256)
    case _ => Decoder.enumDecoder(SignatureMethod)
  }

  implicit val signatureEncoder: Encoder[Signature] = deriveEncoder
  implicit val signatureDecoder: Decoder[Signature] = deriveDecoder

  implicit val clientSignatureEncoder: Encoder[ClientSignature] = deriveEncoder
  implicit val clientSignatureDecoder: Decoder[ClientSignature] = deriveDecoder

  implicit def signedPayloadEncoder[T : Encoder]: Encoder[SignedPayload[T]] = deriveEncoder
  implicit def signedPayloadDecoder[T : Encoder : Decoder]: Decoder[SignedPayload[T]] = deriveDecoder

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
      Json.obj("keyval" → Json.obj("public" -> RsaKeyType.crypto.encode(key)),
               "keytype" → RsaKeyType.asJson)
    case key @ EdTufKey(_) ⇒
      Json.obj("keyval" → Json.obj("public" -> EdKeyType.crypto.encode(key)),
               "keytype" → EdKeyType.asJson)
  }

  implicit val tufPrivateKeyEncoder: Encoder[TufPrivateKey] = Encoder.instance {
    case key @ RSATufPrivateKey(_) ⇒
      Json.obj("keyval" → Json.obj("private" -> RsaKeyType.crypto.encode(key)),
               "keytype" → RsaKeyType.asJson)
    case key @ EdTufPrivateKey(_) ⇒
      Json.obj("keyval" → Json.obj("private" -> EdKeyType.crypto.encode(key)),
               "keytype" → EdKeyType.asJson)
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

  implicit val tufKeyPairEncoder: Encoder[TufKeyPair] = Encoder.instance {
    case RSATufKeyPair(pub, priv) =>
      Json.obj("keytype" -> RsaKeyType.asJson,
               "keyval" -> Json.obj("public" -> RsaKeyType.crypto.encode(pub),
                                    "private" -> RsaKeyType.crypto.encode(priv)))
    case EdTufKeyPair(pub, priv) =>
      Json.obj("keytype" -> EdKeyType.asJson,
               "keyval" -> Json.obj("public" -> EdKeyType.crypto.encode(pub),
                                    "private" -> EdKeyType.crypto.encode(priv)))
  }

  implicit val tufKeyPairDecoder: Decoder[TufKeyPair] = Decoder.instance { cursor =>
    for {
      keyType <- cursor.downField("keytype").as[KeyType]
      keyVal = cursor.downField("keyval")
      pubkeyStr <- keyVal.downField("public").as[String]
      privkeyStr <- keyVal.downField("private").as[String]
      pair <- Either.fromTry(keyType.crypto.parseKeyPair(pubkeyStr, privkeyStr))
                  .leftMap(ex => DecodingFailure(ex.getMessage, cursor.history))
    } yield pair
  }
}
