package com.advancedtelematic.libtuf.data

import com.advancedtelematic.libats.codecs.RefinementError
import eu.timepit.refined
import eu.timepit.refined.api.{Refined, Validate}
import scala.util.{Failure, Success, Try}

object RefinedUtils {
  def refineTry[T, P](value: T)(implicit ev: Validate[T, P]): Try[Refined[T, P]] = {
    refined.refineV(value) match {
      case Left(err) => Failure(RefinementError(value, err))
      case Right(v) => Success(v)
    }
  }
}
