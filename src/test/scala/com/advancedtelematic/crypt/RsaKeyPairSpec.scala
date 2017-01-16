package com.advancedtelematic.crypt

import com.advancedtelematic.ota_tuf.crypt.RsaKeyPair
import com.advancedtelematic.util.OtaTufSpec
import cats.syntax.show._
import RsaKeyPair._

class RsaKeyPairSpec extends OtaTufSpec {
  test("generates a small key")  {
    val generated = RsaKeyPair.generate(size = 512)

    generated.getPrivate.show should include("BEGIN RSA PRIVATE KEY")
  }
}
