package com.advancedtelematic.ota_tuf.data

import eu.timepit.refined
import eu.timepit.refined.api.{Refined, Validate}
import org.genivi.sota.marshalling.RefinementError

import scala.util.{Failure, Success, Try}

object RefinedUtils {
  def refineTry[T, P](value: T)(implicit ev: Validate[T, P]): Try[Refined[T, P]] = {
    refined.refineV(value) match {
      case Left(err) => Failure(RefinementError(value, err))
      case Right(v) => Success(v)
    }
  }
}
