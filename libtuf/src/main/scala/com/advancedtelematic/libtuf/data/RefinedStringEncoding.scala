package com.advancedtelematic.libtuf.data

import eu.timepit.refined
import eu.timepit.refined.api.{Refined, Validate}
import io.circe.{KeyDecoder, KeyEncoder}

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
