package com.advancedtelematic.libtuf.data

import java.security.PublicKey

import com.advancedtelematic.libtuf.data.TufDataType.KeyType.KeyType
import com.advancedtelematic.libtuf.data.TufDataType.SignatureMethod.SignatureMethod
import com.advancedtelematic.libtuf.data.TufDataType.{KeyId, Signature, ValidSignature}
import eu.timepit.refined.api.Refined
import io.circe.Encoder

object ClientDataType {
  case class ClientSignature(keyid: KeyId, method: SignatureMethod, sig: Refined[String, ValidSignature]) {
    def toSignature: Signature = Signature(sig, method)
  }

  implicit class SignatureToClientSignatureOps(value: Signature) {
    def toClient(keyId: KeyId): ClientSignature =
      ClientSignature(keyId, value.method, value.hex)
  }

  case class ClientKey(keytype: KeyType, keyval: PublicKey)

  case class SignedPayload[T : Encoder](signatures: Seq[ClientSignature], signed: T)

  case class RootRole(keys: Map[KeyId, ClientKey],
                      roles: Map[String, RoleKeys],
                      version: Int,
                      _type: String = "Root")

  case class RoleKeys(keyids: Seq[KeyId], threshold: Int)
}
