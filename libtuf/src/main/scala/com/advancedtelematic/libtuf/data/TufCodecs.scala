package com.advancedtelematic.libtuf.data

import java.security.{PrivateKey, PublicKey}

import akka.http.scaladsl.model.Uri
import cats.syntax.either._
import cats.syntax.show._
import com.advancedtelematic.libtuf.crypt.RsaKeyPair
import com.advancedtelematic.libtuf.crypt.RsaKeyPair._
import com.advancedtelematic.libtuf.data.TufDataType._
import io.circe._

object TufCodecs {
  import com.advancedtelematic.libtuf.data.RefinedStringEncoding._
  import io.circe.generic.semiauto._
  import com.advancedtelematic.libats.codecs.AkkaCirce._

  implicit val uriEncoder: Encoder[Uri] = Encoder[String].contramap(_.toString)
  implicit val uriDecoder: Decoder[Uri] = Decoder[String].map(Uri.apply)

  implicit val privateKeyEncoder: Encoder[PrivateKey] = Encoder.instance { privateKey =>
    Json.obj("private" -> Json.fromString(privateKey.show))
  }

  implicit val privateKeyDecoder: Decoder[PrivateKey] = Decoder.instance { cursor =>
    val aCursor = cursor.downField("private")

    aCursor
      .as[String]
      .flatMap { str =>
        Either
          .fromTry(RsaKeyPair.parseKeyPair(str))
          .map(_.getPrivate)
          .leftMap(ex => DecodingFailure(ex.getMessage, aCursor.history))
      }
  }

  implicit val publicKeyEncoder: Encoder[PublicKey] = Encoder.instance { publicKey =>
    Json.obj("public" -> Json.fromString(publicKey.show))
  }

  implicit val publicKeyDecoder: Decoder[PublicKey] = Decoder.instance { cursor =>
    val aCursor = cursor.downField("public")

    aCursor
      .as[String]
      .flatMap { str =>
        Either
          .fromTry(RsaKeyPair.parsePublic(str))
          .leftMap(ex => DecodingFailure(ex.getMessage, aCursor.history))
      }
  }

  implicit val signatureEncoder: Encoder[Signature] = deriveEncoder
  implicit val signatureDecoder: Decoder[Signature] = deriveDecoder

  implicit val clientSignatureEncoder: Encoder[ClientSignature] = deriveEncoder
  implicit val clientSignatureDecoder: Decoder[ClientSignature] = deriveDecoder

  implicit def signedPayloadEncoder[T : Encoder]: Encoder[SignedPayload[T]] = deriveEncoder
  implicit def signedPayloadDecoder[T : Encoder : Decoder]: Decoder[SignedPayload[T]] = deriveDecoder

  implicit val checkSumEncoder: Encoder[Checksum] = deriveEncoder
  implicit val checkSumDecoder: Decoder[Checksum] = deriveDecoder
}
