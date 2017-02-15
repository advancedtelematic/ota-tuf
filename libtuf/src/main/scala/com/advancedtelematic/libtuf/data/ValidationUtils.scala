package com.advancedtelematic.libtuf.data

import eu.timepit.refined.api.Validate

object ValidationUtils {
  def validHex(length: Long, str: String): Boolean = {
    str.length == length && str.forall(h => ('0' to '9').contains(h) || ('a' to 'f').contains(h))
  }

  def validHexValidation[T](v: T, length: Int): Validate.Plain[String, T] =
    Validate.fromPredicate(
      hash => validHex(length, hash),
      hash => s"$hash is not a $length hex string",
      v
    )
}
