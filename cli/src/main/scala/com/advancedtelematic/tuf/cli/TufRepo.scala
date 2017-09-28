package com.advancedtelematic.tuf.cli

import java.nio.file.{Files, Path, Paths}
import java.time.{Instant, Period}
import java.time.temporal.ChronoUnit

import akka.actor.ActorSystem
import akka.http.scaladsl.model.Uri
import akka.stream.Materializer
import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.data.ClientDataType.{ClientTargetItem, RoleKeys, RoleTypeToMetaPathOp, RootRole, TargetCustom, TargetsRole, VersionedRole}
import com.advancedtelematic.libtuf.data.TufDataType.{HardwareIdentifier, KeyId, KeyType, RoleType, SignedPayload, TargetFormat, TargetName, TargetVersion, TufKey, TufPrivateKey}
import com.advancedtelematic.tuf.cli.DataType.{AuthConfig, KeyName, RepoName}
import com.advancedtelematic.tuf.cli.client.{AuthPlusClient, UserReposerverHtttpClient}
import io.circe.{Decoder, Encoder}
import org.slf4j.LoggerFactory
import TryToFuture._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
import cats.syntax.either._
import com.advancedtelematic.libats.messaging_datatype.DataType.{HashMethod, TargetFilename, ValidChecksum, ValidTargetFilename}
import io.circe.syntax._
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.libtuf.reposerver.UserReposerverClient
import eu.timepit.refined.refineV
import eu.timepit.refined.api.Refined

class TufRepo(val name: RepoName, val repoPath: Path)(implicit ec: ExecutionContext) {
  import CliCodecs._
  import io.circe.jawn._

  lazy val storage = new CliKeyStorage(repoPath)

  private lazy val log = LoggerFactory.getLogger(this.getClass)

  private lazy val rolesPath = repoPath.resolve("roles")

  val DEFAULT_EXPIRE_TIME = Period.ofDays(365)

  def init(credentialsPath: Path): Try[Unit] = {
    for {
      _ <- Either.catchNonFatal(Files.createDirectories(repoPath))
      json <- parseFile(credentialsPath.toFile)
      authConfig <- json.as[AuthConfig](authConfigDecoder.prepare(_.downField("oauth2")))
      _ <- Either.catchNonFatal(Files.write(repoPath.resolve("auth.json"), authConfig.asJson.noSpaces.getBytes))
    } yield ()
  }.toTry

  def initTargets(version: Int, expires: Instant): Try[Path] = {
    val emptyTargets = TargetsRole(expires, Map.empty, version)
    writeUnsignedRole(emptyTargets)
  }

  import cats.syntax.option._

  def addTarget(name: TargetName, version: TargetVersion, length: Int, checksum: Refined[String, ValidChecksum], hardwareIds: List[HardwareIdentifier], url: Uri): Try[Path] = {
    for {
      targetsRole <- readUnsignedRole[TargetsRole](RoleType.TARGETS)
      targetFilename <- refineV[ValidTargetFilename](s"${name.value}-${version.value}").leftMap(s => new IllegalArgumentException(s)).toTry
      newTargetRole = {
        val custom = TargetCustom(name, version, hardwareIds, TargetFormat.BINARY.some, url.some)
        val clientHashes = Map(HashMethod.SHA256 -> checksum)
        val newTarget = ClientTargetItem(clientHashes, length, custom.asJson.some)

        targetsRole.copy(targets = targetsRole.targets + (targetFilename -> newTarget))
      }
      path <- writeUnsignedRole(newTargetRole)
    } yield path
  }

  def pushTargets(reposerverClient: UserReposerverClient): Future[SignedPayload[TargetsRole]] = {
    readSignedRole[TargetsRole](RoleType.TARGETS).toFuture.flatMap { targets =>
      log.info(s"pushing ${targets.asJson.spaces2}")
      reposerverClient.pushTargets(targets).map(_ => targets)
    }
  }

  def signTargets(targetsKey: KeyName): Try[Path] =
    for {
      (pub, priv) <- storage.readKeyPair(targetsKey)
      unsigned <- readUnsignedRole[TargetsRole](RoleType.TARGETS) // TODO: Why do we need both?
      sig = TufCrypto.signPayload(priv, unsigned).toClient(pub.id)
      signedRole = SignedPayload(Seq(sig), unsigned)
      path <- writeSignedRole(signedRole)
    } yield path

  private def deleteOrReadKey(reposerverClient: UserReposerverClient, keyName: KeyName, keyId: KeyId): Future[TufPrivateKey] = {
    storage.readPrivateKey(keyName).toFuture.recoverWith { case _ =>
      log.info(s"Could not read old private key locally, fetching/deleting from server")
      reposerverClient.deleteKey(keyId)
    }
  }

  private def readUnsignedRole[T <: VersionedRole : Decoder](roleType: RoleType): Try[T] = {
    val path = rolesPath.resolve("unsigned").resolve(roleType.toMetaPath.value)
    parseFile(path.toFile).flatMap(_.as[T]).toTry
  }

  private def readSignedRole[T <: VersionedRole](roleType: RoleType)(implicit ev: Decoder[SignedPayload[T]]): Try[SignedPayload[T]] = {
    val path = rolesPath.resolve(roleType.toMetaPath.value)
    parseFile(path.toFile).flatMap(_.as[SignedPayload[T]]).toTry
  }

  private def writeUnsignedRole[T <: VersionedRole : Encoder](role: T): Try[Path] =
    writeRole(rolesPath.resolve("unsigned"), role.roleType, role)

  private def writeSignedRole [T <: VersionedRole : Encoder](signedPayload: SignedPayload[T]): Try[Path] =
    writeRole(rolesPath, signedPayload.signed.roleType, signedPayload)

  private def writeRole[T : Encoder](path: Path, roleType: RoleType, payload: T): Try[Path] = Try {
    Files.createDirectories(path)
    val rolePath = path.resolve(roleType.toMetaPath.value)
    Files.write(rolePath, payload.asJson.spaces2.getBytes)
    rolePath
  }

  def genKeys(name: KeyName, keyType: KeyType, keySize: Int): Try[(TufKey, TufPrivateKey)] =
    storage.genKeys(name, keyType, keySize)

  def rotateRoot(repoClient: UserReposerverClient,
                 newRootName: KeyName,
                 oldRootName: KeyName,
                 newTargetsName: KeyName,
                 oldKeyId: Option[KeyId]): Future[SignedPayload[RootRole]] = {
    for {
      (newRootPubKey, newRootPrivKey) <- storage.readKeyPair(newRootName).toFuture
      (newTargetsPubKey, _) <- storage.readKeyPair(newTargetsName).toFuture
      oldRootRole <- repoClient.root().map(_.signed)
      oldRootPubKeyId = oldKeyId.getOrElse(oldRootRole.roles(RoleType.ROOT).keyids.last)
      oldRootPubKey = oldRootRole.keys(oldRootPubKeyId)
      oldRootPrivKey <- deleteOrReadKey(repoClient, oldRootName, oldRootPubKeyId)
      _ <- storage.writeKeys(oldRootName, oldRootPubKey, oldRootPrivKey).toFuture
      newKeySet = oldRootRole.keys ++ Map(newRootPubKey.id -> newRootPubKey, newTargetsPubKey.id -> newTargetsPubKey)
      newRootRoleKeys = RoleKeys(Seq(newRootPubKey.id), threshold = 1)
      newTargetsRoleKeys = RoleKeys(Seq(newTargetsPubKey.id), threshold = 1)
      newRootRoleMap = oldRootRole.roles ++ Map(RoleType.ROOT -> newRootRoleKeys, RoleType.TARGETS -> newTargetsRoleKeys)
      newExpireTime = oldRootRole.expires.plus(DEFAULT_EXPIRE_TIME)
      newRootRole = oldRootRole.copy(keys = newKeySet, roles = newRootRoleMap, version = oldRootRole.version + 1, newExpireTime)
      newRootSignature = TufCrypto.signPayload(newRootPrivKey, newRootRole).toClient(newRootPubKey.id)
      newRootClientOldSignature = TufCrypto.signPayload(oldRootPrivKey, newRootRole).toClient(oldRootPubKeyId)
      newSignedRoot = SignedPayload(Seq(newRootSignature, newRootClientOldSignature), newRootRole)
      _ = log.debug(s"pushing ${newSignedRoot.asJson.spaces2}")
      _ <- repoClient.pushSignedRoot(newSignedRoot)
      _ <- writeSignedRole(newSignedRoot).toFuture
    } yield newSignedRoot
  }

  def pushTargetsKey(reposerver: UserReposerverClient, keyName: KeyName): Future[TufKey] = {
    storage.readPublicKey(keyName).toFuture.flatMap(reposerver.pushTargetsKey)
  }

  def authConfig(): Try[AuthConfig] = {
    parseFile(repoPath.resolve("auth.json").toFile)
      .flatMap(_.as[AuthConfig])
      .toTry
  }
}

