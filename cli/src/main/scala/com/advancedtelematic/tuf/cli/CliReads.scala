package com.advancedtelematic.tuf.cli

import java.nio.file.{Path, Paths}
import java.time.Instant

import com.advancedtelematic.libtuf.data.TufDataType.{Ed25519KeyType, KeyType, RsaKeyType, TargetFormat}
import eu.timepit.refined
import eu.timepit.refined.api.{Refined, Validate}
import scopt.Read
import shapeless._
import cats.syntax.either._
import com.advancedtelematic.libtuf.data.TufDataType.TargetFormat.TargetFormat
import com.advancedtelematic.libtuf.data.ValidatedString.{ValidatedString, ValidatedStringValidation}
import com.advancedtelematic.tuf.cli.DataType._
import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax._

import scala.util.Try
import cats.syntax.functor._

object CliCodecs {
  import io.circe.generic.extras.semiauto._
  import com.advancedtelematic.libats.codecs.CirceUri._
  import io.circe.generic.extras.Configuration

  implicit val config: Configuration = Configuration.default.withDefaults

  implicit val authConfigDecoder: Decoder[OAuthConfig] = deriveDecoder
  implicit val authConfigEncoder: Encoder[OAuthConfig] = deriveEncoder

  implicit val mutualTlsConfigEncoder: Encoder[MutualTlsConfig] = deriveEncoder
  implicit val mutualTlsConfigDecoder: Decoder[MutualTlsConfig] = deriveDecoder

  implicit val cliAuthEncoder: Encoder[CliAuth] = Encoder.instance {
    case oauth: OAuthConfig => oauth.asJson
    case tls: MutualTlsConfig => tls.asJson
  }
  implicit val cliAuthDecoder: Decoder[CliAuth] = authConfigDecoder.widen or mutualTlsConfigDecoder.widen

  implicit val pathEncoder: Encoder[Path] = Encoder.encodeString.contramap(_.toString)
  implicit val pathDecoder: Decoder[Path] = Decoder.decodeString.emapTry(s => Try(Paths.get(s)))

  implicit val treehubConfigDecoder = Decoder.instance { d =>
    for {
      noAuth <- d.downField("no_auth").as[Option[Boolean]]
      oauth <- d.downField("oauth2").as[Option[OAuthConfig]]
      ostree <- d.downField("ostree").as[Option[Json]]
    } yield TreehubConfig(oauth, noAuth.getOrElse(false), ostree.getOrElse(Json.obj()))
  }

  implicit val treehubConfigEncoder = deriveEncoder[TreehubConfig]

  implicit val repoServerTypeEncoder = deriveEnumerationEncoder[TufServerType]
  implicit val repoServerTypeDecoder = deriveEnumerationDecoder[TufServerType]

  implicit val repoConfigEncoder = deriveEncoder[RepoConfig]
  implicit val repoConfigDecoder = deriveDecoder[RepoConfig]
}

object CliReads {
  implicit def refinedRead[P](implicit v: Validate.Plain[String, P]): Read[Refined[String, P]] = Read.stringRead.map { str =>
    refined.refineV[P](str).valueOr(p => throw new IllegalArgumentException(s"Invalid value: $p"))
  }

  implicit val keyTypeRead: Read[KeyType] = Read.reads {
    case "ed25519" => Ed25519KeyType
    case "rsa" => RsaKeyType
    case str => throw new IllegalArgumentException(s"Invalid keytype: $str valid: (ed25519, rsa)")
  }

  implicit def anyvalRead[T <: AnyVal](implicit gen: Generic.Aux[T, String :: HNil]): Read[T] = Read.stringRead.map { str =>
    gen.from(str :: HNil)
  }

  implicit def validatedStringRead[W <: ValidatedString](implicit validation: ValidatedStringValidation[W]): Read[W] = Read.stringRead.map { str =>
    validation(str).valueOr(err => throw new IllegalArgumentException(err.toList.mkString(",")))
  }

  implicit val pathRead: Read[Path] = Read.fileRead.map(_.toPath)

  implicit val instantRead: Read[Instant] = Read.stringRead.map(Instant.parse)

  implicit val targetFormatRead: Read[TargetFormat] = Read.stringRead.map(_.toUpperCase).map(TargetFormat.withName)

  implicit val repoServerTypeRead: Read[TufServerType] = Read.stringRead.map(_.toLowerCase()).map {
    case "reposerver" => RepoServer
    case "director" => Director
    case str => throw new IllegalArgumentException(s"Invalid repo server type: $str valid: reposerver or director")
  }

  implicit def seqToListRead[T : Read]: Read[List[T]] = Read.seqRead[T].map(_.toList)
}
