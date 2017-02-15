package com.advancedtelematic.ota_tuf.crypt

import com.advancedtelematic.util.OtaTufSpec
import cats.syntax.show._
import com.advancedtelematic.libtuf.crypt.RsaKeyPair
import com.advancedtelematic.libtuf.crypt.RsaKeyPair._

class RsaKeyPairSpec extends OtaTufSpec {
  test("generates a small key")  {
    val generated = RsaKeyPair.generate(size = 512)

    generated.getPrivate.show should include("BEGIN RSA PRIVATE KEY")
  }
}
