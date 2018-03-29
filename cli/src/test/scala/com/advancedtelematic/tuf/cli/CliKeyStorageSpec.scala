package com.advancedtelematic.tuf.cli

import java.nio.file.Files
import java.nio.file.attribute.PosixFilePermission
import PosixFilePermission._

import com.advancedtelematic.libtuf.data.TufDataType.{Ed25519KeyType, Ed25519TufKey, Ed25519TufPrivateKey, KeyType, RSATufKey, RSATufPrivateKey, RsaKeyType}
import com.advancedtelematic.tuf.cli.DataType.KeyName
import com.advancedtelematic.tuf.cli.repo.CliKeyStorage

import scala.collection.JavaConverters._
import scala.reflect.ClassTag

class CliKeyStorageSpec extends CliSpec {
  val tempDir = Files.createTempDirectory("tuf-keys")

  lazy val subject = new CliKeyStorage(tempDir)

  def keySpecific[T <: KeyType, Pub <: T#Pub : ClassTag, Priv <: T#Priv : ClassTag](keyType: KeyType, name: String): Unit = {
    test("generates a key " + name) {
      val keyName = KeyName("test-key")
      subject.genKeys(keyName, keyType)

      val (pub, priv) = subject.readKeyPair(keyName).get

      pub shouldBe a[Pub]
      priv shouldBe a[Priv]
    }

    test("writes keys with limited permissions " + name) {
      val keyName = KeyName("test-key-02")
      subject.genKeys(keyName, keyType)

      val perms = Files.getPosixFilePermissions(tempDir.resolve("keys").resolve(keyName.value + ".sec"))
      perms.asScala shouldBe Set(OWNER_READ, OWNER_WRITE)
    }

    test("creates key directory with limited permissions "  + name) {
      val keyName = KeyName("test-key-02")
      subject.genKeys(keyName, keyType)

      val perms = Files.getPosixFilePermissions(tempDir.resolve("keys"))
      perms.asScala shouldBe Set(OWNER_READ, OWNER_WRITE, OWNER_EXECUTE)
    }
  }

  testsFor(keySpecific[RsaKeyType.type, RSATufKey, RSATufPrivateKey](RsaKeyType, "RSA"))
  testsFor(keySpecific[Ed25519KeyType.type, Ed25519TufKey, Ed25519TufPrivateKey](Ed25519KeyType, "Ed25519"))
}
