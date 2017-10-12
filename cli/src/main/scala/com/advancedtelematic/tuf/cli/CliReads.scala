package com.advancedtelematic.tuf.cli

import java.nio.file.Path
import java.time.Instant

import com.advancedtelematic.libtuf.data.TufDataType.{EdKeyType, KeyType, RsaKeyType}
import eu.timepit.refined
import eu.timepit.refined.api.{Refined, Validate}
import scopt.Read
import shapeless._
import cats.syntax.either._
import com.advancedtelematic.tuf.cli.DataType.AuthConfig


object CliCodecs {
  import io.circe.generic.semiauto._
  import com.advancedtelematic.libats.codecs.CirceUri._

  implicit val authConfigDecoder = deriveDecoder[AuthConfig]
  implicit val authConfigEncoder = deriveEncoder[AuthConfig]
}

object CliReads {
  implicit def refinedRead[P](implicit v: Validate.Plain[String, P]): Read[Refined[String, P]] = Read.stringRead.map { str =>
    refined.refineV[P](str).valueOr(p => throw new IllegalArgumentException(s"Invalid value: $p"))
  }

  implicit val keyTypeRead: Read[KeyType] = Read.reads {
    case "ec" => EdKeyType
    case "rsa" => RsaKeyType
    case str => throw new IllegalArgumentException(s"Invalid keytype: $str valid: (ec, rsa)")
  }

  implicit def anyvalRead[T <: AnyVal](implicit gen: Generic.Aux[T, String :: HNil]): Read[T] = Read.stringRead.map { str =>
    gen.from(str :: HNil)
  }

  implicit val pathRead: Read[Path] = Read.fileRead.map(_.toPath)

  implicit val instantRead: Read[Instant] = Read.stringRead.map(Instant.parse)
}
