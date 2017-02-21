package com.advancedtelematic.tuf.keyserver.crypt

import com.advancedtelematic.tuf.util.TufKeyserverSpec
import cats.syntax.show._
import com.advancedtelematic.libtuf.crypt.RsaKeyPair
import com.advancedtelematic.libtuf.crypt.RsaKeyPair._

class RsaKeyPairSpec extends TufKeyserverSpec {
  test("generates a small key")  {
    val generated = RsaKeyPair.generate(size = 512)

    generated.getPrivate.show should include("BEGIN RSA PRIVATE KEY")
  }
}
