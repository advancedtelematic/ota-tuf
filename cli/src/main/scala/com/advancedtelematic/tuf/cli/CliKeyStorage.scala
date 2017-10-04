package com.advancedtelematic.tuf.cli

import java.nio.file.{Files, Path}

import com.advancedtelematic.libtuf.data.TufDataType.{KeyType, TufKey, TufPrivateKey}
import com.advancedtelematic.tuf.cli.DataType.KeyName
import org.slf4j.LoggerFactory
import cats.syntax.either._
import scala.util.Try

class CliKeyStorage(repo: Path) {
  import com.advancedtelematic.libtuf.data.TufCodecs._
  import io.circe.jawn._
  import io.circe.syntax._

  private lazy val log = LoggerFactory.getLogger(this.getClass)

  private implicit class KeyNamePath(v: KeyName) {
    def publicKeyPath = repo.resolve("keys").resolve(v.value + ".pub")

    def privateKeyPath = repo.resolve("keys").resolve(v.value + ".sec")
  }

  private def writePublic(keyName: KeyName, tufKey: TufKey): Try[Unit] = Try {
    Files.write(keyName.publicKeyPath, tufKey.asJson.spaces2.getBytes)
  }

  private def writePrivate(keyName: KeyName, tufKey: TufPrivateKey): Try[Unit] = Try {
    Files.write(keyName.privateKeyPath, tufKey.asJson.spaces2.getBytes)
  }

  def writeKeys(name: KeyName, pub: TufKey, priv: TufPrivateKey): Try[Unit] = for {
    _ <- Try(Files.createDirectories(repo.resolve("keys")))
    _ <- writePublic(name, pub)
    _ <- writePrivate(name, priv)
    _ = log.info(s"Saved keys to $repo/{${repo.relativize(name.privateKeyPath)}, ${repo.relativize(name.publicKeyPath)}}")
  } yield ()

  def genKeys(name: KeyName, keyType: KeyType, keySize: Int): Try[(TufKey, TufPrivateKey)] = {
    val (pub, priv) = keyType.crypto.generateKeyPair(keySize)
    writeKeys(name, pub, priv).map(_ => pub -> priv)
  }

  def readPrivateKey(keyName: KeyName): Try[TufPrivateKey] = {
    parseFile(keyName.privateKeyPath.toFile).flatMap(_.as[TufPrivateKey]).toTry
  }

  def readPublicKey(keyName: KeyName): Try[TufKey] =
    parseFile(keyName.publicKeyPath.toFile).flatMap(_.as[TufKey]).toTry

  def readKeyPair(keyName: KeyName): Try[(TufKey, TufPrivateKey)] = for {
    pub <- readPublicKey(keyName)
    priv <- readPrivateKey(keyName)
  } yield (pub, priv)
}
