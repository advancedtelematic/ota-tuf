package com.advancedtelematic.tuf.cli.repo

import java.nio.file.attribute.{PosixFilePermission, PosixFilePermissions}
import java.nio.file.{FileAlreadyExistsException, Files, Path, Paths}
import java.util
import PosixFilePermission._
import scala.collection.JavaConversions._
import cats.syntax.either._
import com.advancedtelematic.libtuf.data.TufDataType.{KeyType, TufKey, TufKeyPair, TufPrivateKey}
import com.advancedtelematic.tuf.cli.DataType.KeyName
import org.slf4j.LoggerFactory

import scala.util.Try

class CliKeyStorage(repo: Path) {
  import com.advancedtelematic.libtuf.data.TufCodecs._
  import io.circe.jawn._
  import io.circe.syntax._

  private lazy val log = LoggerFactory.getLogger(this.getClass)

  private lazy val SECRET_KEY_PERMISSIONS = util.EnumSet.of(OWNER_READ, OWNER_WRITE)

  implicit private class KeyNamePath(v: KeyName) {
    def publicKeyPath: Path = repo.resolve("keys").resolve(v.publicKeyName)

    def privateKeyPath: Path = repo.resolve("keys").resolve(v.privateKeyName)
  }

  private def writePublic(keyName: KeyName, tufKey: TufKey): Try[Unit] = Try {
    Files.write(keyName.publicKeyPath, tufKey.asJson.spaces2.getBytes)
  }

  private def writePrivate(keyName: KeyName, tufKey: TufPrivateKey): Try[Unit] = Try {
    try Files.createFile(keyName.privateKeyPath, PosixFilePermissions.asFileAttribute(SECRET_KEY_PERMISSIONS))
    catch { case _: FileAlreadyExistsException => () }

    Files.write(keyName.privateKeyPath, tufKey.asJson.spaces2.getBytes)
  }

  def writeKeys(name: KeyName, pair: TufKeyPair): Try[Unit] =
    writeKeys(name, pair.pubkey, pair.privkey)

  private def ensureKeysDirCreated(): Try[Unit] = Try {
    val perms = PosixFilePermissions.asFileAttribute(SECRET_KEY_PERMISSIONS + OWNER_EXECUTE)
    Files.createDirectories(repo.resolve("keys"), perms)
  }

  def writeKeys(name: KeyName, pub: TufKey, priv: TufPrivateKey): Try[Unit] = {
    assert(pub.keytype == priv.keytype)

    for {
      _ <- ensureKeysDirCreated()
      _ <- writePublic(name, pub)
      _ <- writePrivate(name, priv)
      _ = log.info(s"Saved keys to $repo/{${repo.relativize(name.privateKeyPath)}, ${repo.relativize(name.publicKeyPath)}}")
    } yield ()
  }

  def genKeys(name: KeyName, keyType: KeyType, keySize: Int): Try[TufKeyPair] = {
    val pair = keyType.crypto.generateKeyPair(keySize)
    writeKeys(name, pair).map(_ => pair)
  }

  def readPrivateKey(keyName: KeyName): Try[TufPrivateKey] =
    parseFile(keyName.privateKeyPath.toFile).flatMap(_.as[TufPrivateKey]).toTry

  def readPublicKey(keyName: KeyName): Try[TufKey] = {
    parseFile(keyName.publicKeyPath.toFile).flatMap(_.as[TufKey]).toTry
  }

  def readKeyPair(keyName: KeyName): Try[(TufKey, TufPrivateKey)] = for {
    pub <- readPublicKey(keyName)
    priv <- readPrivateKey(keyName)
  } yield (pub, priv)
}
