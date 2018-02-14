package com.advancedtelematic.tuf.cli

import java.nio.file.Files
import java.nio.file.attribute.PosixFilePermission
import PosixFilePermission._
import com.advancedtelematic.libtuf.data.TufDataType.{Ed25519KeyType, Ed25519TufKey, Ed25519TufPrivateKey}
import com.advancedtelematic.tuf.cli.DataType.KeyName
import com.advancedtelematic.tuf.cli.repo.CliKeyStorage
import scala.collection.JavaConverters._

class CliKeyStorageSpec extends CliSpec {
  val tempDir = Files.createTempDirectory("tuf-keys")

  lazy val subject = new CliKeyStorage(tempDir)

  test("generates a key") {
    val keyName = KeyName("test-key")
    subject.genKeys(keyName, Ed25519KeyType, 256)

    val (pub, priv) = subject.readKeyPair(keyName).get

    pub shouldBe a[Ed25519TufKey]
    priv shouldBe a[Ed25519TufPrivateKey]
  }

  test("writes keys with limited permissions") {
    val keyName = KeyName("test-key-02")
    subject.genKeys(keyName, Ed25519KeyType, 256)

    val perms = Files.getPosixFilePermissions(tempDir.resolve("keys").resolve(keyName.value + ".sec"))
    perms.asScala shouldBe Set(OWNER_READ, OWNER_WRITE)
  }

  test("creates key directory with limited permissions") {
    val keyName = KeyName("test-key-02")
    subject.genKeys(keyName, Ed25519KeyType, 256)

    val perms = Files.getPosixFilePermissions(tempDir.resolve("keys"))
    perms.asScala shouldBe Set(OWNER_READ, OWNER_WRITE, OWNER_EXECUTE)
  }

}


