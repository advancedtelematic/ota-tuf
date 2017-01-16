package com.advancedtelematic.ota_tuf.crypt

import java.io.StringWriter
import java.security._
import cats.syntax.contravariant._
import cats.Show
import org.bouncycastle.openssl.jcajce.JcaPEMWriter

object RsaKeyPair {
  def generate(size: Int = 512): KeyPair = {
    val keyGen = KeyPairGenerator.getInstance("RSA", "BC")
    keyGen.initialize(size, new SecureRandom())
    keyGen.generateKeyPair()
  }

  private implicit val keyShow: Show[java.security.Key] = Show.show { privateKey ⇒
    val pemStrWriter = new StringWriter()
    val jcaPEMWriter = new JcaPEMWriter(pemStrWriter)
    jcaPEMWriter.writeObject(privateKey)
    jcaPEMWriter.flush()
    pemStrWriter.toString
  }

  implicit val rsaPrivateKeyShow: Show[PrivateKey] = keyShow.contramap(identity)

  implicit val rsaPublicKeyShow: Show[PublicKey] = keyShow.contramap(identity)
}
