package com.advancedtelematic.libtuf.crypt

import com.advancedtelematic.libtuf.data.TufDataType.{SignedPayload, TufKey}
import io.circe.Encoder
import org.slf4j.LoggerFactory


object SignedPayloadSignatureOps  {

  private val _log = LoggerFactory.getLogger(this.getClass)

  implicit class SignedPayloadSignatureOps[T : Encoder](value: SignedPayload[T]) {
    def isValidFor(tufKey: TufKey): Boolean =
      value.signatures.exists { sig =>
        TufCrypto.isValid(sig, tufKey, value.json)
      }
  }
}
