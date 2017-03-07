package com.advancedtelematic.libtuf.data

import eu.timepit.refined.api.Validate

object ValidationUtils {

  def validBase64(str: String): Boolean= {
    val validChars = ('0' to '9') ++ ('a' to 'z') ++ ('A' to 'Z') ++ List('+', '/', '=')
    str.forall(validChars.contains)
  }

  def validBase64Validation[T](v: T): Validate.Plain[String, T] =
    Validate.fromPredicate(
      value => validBase64(value),
      hash => s"$hash is not a base64 string",
      v
    )

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
