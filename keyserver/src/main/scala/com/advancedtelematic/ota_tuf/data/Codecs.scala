package com.advancedtelematic.ota_tuf.data

import java.security.PublicKey

import com.advancedtelematic.libtuf.crypt.RsaKeyPair._
import cats.syntax.show.toShowOps
import akka.http.scaladsl.model.Uri
import cats.data.Xor
import com.advancedtelematic.libtuf.crypt.RsaKeyPair
import com.advancedtelematic.libtuf.data.ClientDataType._
import com.advancedtelematic.libtuf.data.TufDataType.HashMethod.HashMethod
import com.advancedtelematic.libtuf.data.TufDataType._
import com.advancedtelematic.libtuf.data.RepoClientDataType._
import io.circe._

import scala.util.Try

object Codecs {
  import com.advancedtelematic.libtuf.data.RefinedStringEncoding._
  import io.circe.generic.semiauto._

  // refinedMapEncoder and refinedMapDecoder are broken in sota CirceInstances
  // they encode Map[Refined[T, P], V] as an array of tuples
  // so we need to custom import what we need here.
  import org.genivi.sota.marshalling.CirceInstances.{dateTimeDecoder, dateTimeEncoder, refinedDecoder, refinedEncoder}

  implicit val uriEncoder: Encoder[Uri] = Encoder[String].contramap(_.toString)
  implicit val uriDecoder: Decoder[Uri] = Decoder[String].map(Uri.apply)

  implicit val publicKeyEncoder: Encoder[PublicKey] = Encoder.instance { publicKey =>
    Json.obj("public" -> Json.fromString(publicKey.show))
  }
  implicit val publicKeyDecoder: Decoder[PublicKey] = Decoder.instance { cursor =>
    val aCursor = cursor.downField("public")

    aCursor
      .as[String]
      .flatMap { str =>
        Xor
          .fromTry(RsaKeyPair.parsePublic(str))
          .leftMap(ex => DecodingFailure(ex.getMessage, aCursor.history))
      }
  }

  implicit val signatureEncoder: Encoder[Signature] = deriveEncoder
  implicit val signatureDecoder: Decoder[Signature] = deriveDecoder

  implicit val roleKeyEncoder: Encoder[RoleKeys] = deriveEncoder
  implicit val roleKeyDecoder: Decoder[RoleKeys] = deriveDecoder

  implicit val clientKeyEncoder: Encoder[ClientKey] = deriveEncoder

  implicit val rootRoleEncoder: Encoder[RootRole] = deriveEncoder
  implicit val rootRoleDecoder: Decoder[RootRole] = deriveDecoder

  implicit val clientKeyDecoder: Decoder[ClientKey] = deriveDecoder

  implicit val clientSignatureEncoder: Encoder[ClientSignature] = deriveEncoder
  implicit val clientSignatureDecoder: Decoder[ClientSignature] = deriveDecoder

  implicit def signedPayloadEncoder[T : Encoder]: Encoder[SignedPayload[T]] = deriveEncoder
  implicit def signedPayloadDecoder[T : Encoder : Decoder]: Decoder[SignedPayload[T]] = deriveDecoder

  // *** Repo Service Codecs ***

  implicit val checkSumEncoder: Encoder[Checksum] = deriveEncoder
  implicit val checkSumDecoder: Decoder[Checksum] = deriveDecoder

  implicit val targetsRoleEncoder: Encoder[TargetsRole] = deriveEncoder
  implicit val targetsRoleDecoder: Decoder[TargetsRole] = deriveDecoder

  implicit val clientTargetItemEncoder: Encoder[ClientTargetItem] = deriveEncoder
  implicit val clientTargetItemDecoder: Decoder[ClientTargetItem] = deriveDecoder

  implicit val hashMethodKeyEncoder: KeyEncoder[HashMethod] = KeyEncoder[String].contramap(_.toString)
  implicit val hashMethodKeyDecoder: KeyDecoder[HashMethod] = KeyDecoder.instance { value =>
    Try(HashMethod.withName(value)).toOption
  }

  implicit val metaItemEncoder: Encoder[MetaItem] = deriveEncoder
  implicit val metaItemDecoder: Decoder[MetaItem] = deriveDecoder

  implicit val snapshotRoleEncoder: Encoder[SnapshotRole] = deriveEncoder
  implicit val snapshotRoleDecoder: Decoder[SnapshotRole] = deriveDecoder

  implicit val timestampRoleEncoder: Encoder[TimestampRole] = deriveEncoder
  implicit val timestampRoleDecoder: Decoder[TimestampRole] = deriveDecoder
}


