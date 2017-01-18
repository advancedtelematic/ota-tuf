package com.advancedtelematic.ota_tuf.data

import java.security.PublicKey

import akka.http.scaladsl.model.Uri
import cats.data.Xor
import com.advancedtelematic.ota_tuf.data.DataType.Signature
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}
import com.advancedtelematic.ota_tuf.crypt.RsaKeyPair.keyShow
import cats.syntax.show.toShowOps
import com.advancedtelematic.ota_tuf.crypt.RsaKeyPair
import com.advancedtelematic.ota_tuf.data.ClientDataType._
import com.advancedtelematic.ota_tuf.data.RepoClientDataType.{ClientTargetItem, MetaItem, SnapshotRole, TargetsRole, TimestampRole}
import com.advancedtelematic.ota_tuf.data.RepositoryDataType.HashMethod.HashMethod
import com.advancedtelematic.ota_tuf.data.RepositoryDataType.{Checksum, HashMethod, TargetItem}
import eu.timepit.refined
import eu.timepit.refined.api.{Refined, Validate}

import scala.util.Try

object Codecs {
  import io.circe.generic.semiauto._
  import RefinedStringEncoding._

  // refinedMapEncoder and refinedMapDecoder are broken in sota CirceInstances
  // they encode Map[Refined[T, P], V] as an array of tuples
  // so we need to custom import what we need here.
  import org.genivi.sota.marshalling.CirceInstances.{refinedDecoder, refinedEncoder}
  import org.genivi.sota.marshalling.CirceInstances.{dateTimeDecoder, dateTimeEncoder}

  implicit val uriEncoder: Encoder[Uri] = Encoder[String].contramap(_.toString)
  implicit val uriDecoder: Decoder[Uri] = Decoder[String].map(Uri.apply)

  implicit val publicKeyEncoder: Encoder[PublicKey] = Encoder[String].contramap(_.show)
  implicit val publicKeyDecoder: Decoder[PublicKey] = Decoder[String].emap { str =>
    Xor.fromTry(RsaKeyPair.parsePublic(str)).leftMap(_.getMessage)
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

object RefinedStringEncoding {
  implicit def refinedKeyEncoder[P]
  (implicit strKeyEncoder: KeyEncoder[String]): KeyEncoder[Refined[String, P]] =
    strKeyEncoder.contramap(_.get)

  implicit def refinedKeyDecoder[P]
  (implicit p: Validate.Plain[String, P]): KeyDecoder[Refined[String, P]] =
    KeyDecoder.instance[Refined[String, P]] { s =>
      refined.refineV[P](s).right.toOption
    }
}
