package com.advancedtelematic.tuf.cli.repo

import java.io._
import java.net.URI
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}
import java.time.{Instant, Period}

import cats.data.Validated.{Invalid, Valid}
import cats.syntax.either._
import cats.syntax.option._
import com.advancedtelematic.libats.data.DataType.{HashMethod, ValidChecksum}
import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.ClientDataType.{ClientTargetItem, ETag, MetaPath, RoleKeys, RootRole, TargetCustom, TargetsRole, TufRole, TufRoleOps}
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.TufDataType.TargetFormat.TargetFormat
import com.advancedtelematic.libtuf.data.TufDataType.{HardwareIdentifier, KeyId, KeyType, RoleType, SignedPayload, TargetName, TargetVersion, TufKey, TufKeyPair, TufPrivateKey, ValidTargetFilename}
import com.advancedtelematic.libtuf.reposerver.UserReposerverClient
import com.advancedtelematic.tuf.cli.DataType.{AuthConfig, KeyName, RepoConfig, RepoName}
import com.advancedtelematic.tuf.cli.repo.TufRepo.{EtagsNotFound, TargetsPullError}
import com.advancedtelematic.tuf.cli.{CliCodecs, CliUtil}
import eu.timepit.refined.api.Refined
import eu.timepit.refined.refineV
import io.circe.jawn.parseFile
import io.circe.syntax._
import io.circe.{Decoder, Encoder}
import org.slf4j.LoggerFactory
import com.advancedtelematic.tuf.cli.TryToFuture._
import com.advancedtelematic.libtuf.data.ClientDataType.TufRole._
import com.advancedtelematic.libtuf.reposerver.UserReposerverClient.TargetsResponse

import java.nio.file.attribute.PosixFilePermission._
import scala.collection.JavaConverters._
import scala.concurrent.{ExecutionContext, Future}
import scala.util
import scala.util.control.NoStackTrace
import scala.util.{Failure, Success, Try}

object TufRepo {
  import CliCodecs._

  case object EtagsNotFound extends Exception(
    "Could not find targets etags file. You need this to push a new targets file. Etags can be obtained using the pull command"
  ) with NoStackTrace

  case class TreehubConfigError(msg: String) extends Exception(msg) with NoStackTrace

  case class MissingCredentialsZipFile(filename: String) extends Exception(
    s"Missing file from credentials.zip: $filename"
  ) with NoStackTrace

  case class RepoAlreadyInitialized(path: Path) extends Exception(s"Repository at $path was already initialized") with NoStackTrace

  case class TargetsPullError(msg: String) extends Exception(msg) with NoStackTrace

  protected [repo] def readConfigFile(repoPath: Path): Try[RepoConfig] =
    Try { new FileInputStream(repoPath.resolve("config.json").toFile) }
      .flatMap { is => CliUtil.readJsonFrom[RepoConfig](is) }

  protected [repo] def writeConfigFile(repoPath: Path, repoUri: URI, authConfig: Option[AuthConfig]): Try[RepoConfig] = Try {
    val repoConfig = RepoConfig(repoUri, authConfig)
    Files.write(repoPath.resolve("config.json"), repoConfig.asJson.spaces2.getBytes)
    repoConfig
  }
}

class TufRepo(val name: RepoName, val repoPath: Path)(implicit ec: ExecutionContext) {
  import CliCodecs._

  protected[repo] lazy val keyStorage = new CliKeyStorage(repoPath)

  private lazy val log = LoggerFactory.getLogger(this.getClass)

  private lazy val rolesPath = repoPath.resolve("roles")

  private val DEFAULT_EXPIRE_TIME = Period.ofDays(365)

  def initTargets(version: Int, expires: Instant): Try[Path] = {
    val emptyTargets = TargetsRole(expires, Map.empty, version)
    writeUnsignedRole(emptyTargets)
  }

  def initRepoDirs(): Try[Unit] = Try {
    val perms = PosixFilePermissions.asFileAttribute(java.util.EnumSet.of(OWNER_READ, OWNER_WRITE, OWNER_EXECUTE))

    Files.createDirectory(repoPath, perms)
    Files.createDirectory(rolesPath, perms)
    Files.createDirectory(rolesPath.resolve("unsigned"), perms)

    def checkPerms(path: Path): Unit = {
      val currentPerms = Files.getPosixFilePermissions(path)
      if(currentPerms.asScala != Set(OWNER_READ, OWNER_WRITE, OWNER_EXECUTE))
        log.warn(s"Permissions for $path are too open")
    }

    checkPerms(repoPath)
    checkPerms(rolesPath)
    checkPerms(rolesPath.resolve("unsigned"))
  }

  def addTarget(name: TargetName, version: TargetVersion, length: Int, checksum: Refined[String, ValidChecksum],
                hardwareIds: List[HardwareIdentifier], url: URI, format: TargetFormat): Try[Path] = {
    for {
      targetsRole <- readUnsignedRole[TargetsRole]
      targetFilename <- refineV[ValidTargetFilename](s"${name.value}-${version.value}").leftMap(s => new IllegalArgumentException(s)).toTry
      newTargetRole = {
        val custom = TargetCustom(name, version, hardwareIds, format.some, url.some)
        val clientHashes = Map(HashMethod.SHA256 -> checksum)
        val newTarget = ClientTargetItem(clientHashes, length, custom.asJson.some)

        targetsRole.copy(targets = targetsRole.targets + (targetFilename -> newTarget))
      }
      path <- writeUnsignedRole(newTargetRole)
    } yield path
  }

  private def writeTargets(targets: SignedPayload[TargetsRole], etag: ETag): Try[Unit] =
    writeUnsignedRole(targets.signed).flatMap(_ => writeEtag(etag))

  private def ensureTargetsPulled(reposerverClient: UserReposerverClient, rootRole: RootRole): Future[Unit] = {
    Future.fromTry {
      readUnsignedRole[TargetsRole].map(_ => ())
    }.recoverWith {
      case _ =>
        log.warn("unsigned targets not available locally, pulling targets from server")
        pullTargets(reposerverClient, rootRole).map(_ => ())
    }
  }

  def pullTargets(reposerverClient: UserReposerverClient, rootRole: RootRole): Future[SignedPayload[TargetsRole]] =
    reposerverClient.targets().flatMap {
      case TargetsResponse(targets, etag) =>
        val roleValidation = TufCrypto.payloadSignatureIsValid(rootRole, targets)

        roleValidation match {
          case Valid(_) if etag.isDefined => writeTargets(targets, etag.get).map(_ => targets).toFuture
          case Valid(_) => Future.failed(TargetsPullError("Did not receive valid etag from reposerver"))
          case Invalid(s) => Future.failed(TargetsPullError(s.toList.mkString(", ")))
        }
    }

  def pushTargets(reposerverClient: UserReposerverClient): Future[SignedPayload[TargetsRole]] =
    readSignedRole[TargetsRole].toFuture.flatMap { targets =>
      log.debug(s"pushing ${targets.asJson.spaces2}")

      for {
        etag <- readEtag[TargetsRole].toFuture
        _ <- reposerverClient.pushTargets(targets, etag.some)
      } yield targets
    }

  def signTargets(targetsKey: KeyName, version: Option[Int] = None): Try[Path] =
    for {
      (pub, priv) <- keyStorage.readKeyPair(targetsKey)
      unsigned <- readUnsignedRole[TargetsRole]
      newUnsigned = unsigned.copy(version = version.getOrElse(unsigned.version + 1))
      sig = TufCrypto.signPayload(priv, newUnsigned).toClient(pub.id)
      signedRole = SignedPayload(Seq(sig), newUnsigned)
      _ <- writeUnsignedRole(signedRole.signed)
      path <- writeSignedRole(signedRole)
    } yield path

  private def deleteOrReadKey(reposerverClient: UserReposerverClient, keyName: KeyName, keyId: KeyId): Future[TufPrivateKey] = {
    keyStorage.readPrivateKey(keyName).toFuture.recoverWith { case _ =>
      log.info(s"Could not read old private key locally, fetching before deleting from server")

      for {
        keyPair ← reposerverClient.fetchKeyPair(keyId)
        _ ← reposerverClient.deleteKey(keyId)
      } yield keyPair.privkey
    }
  }

  def readUnsignedRole[T : Decoder](implicit ev: TufRole[T]): Try[T] = {
    val path = rolesPath.resolve("unsigned").resolve(ev.toMetaPath.value)
    parseFile(path.toFile).flatMap(_.as[T]).toTry
  }

  def readSignedRole[T](implicit decoder: Decoder[SignedPayload[T]], ev: TufRole[T]): Try[SignedPayload[T]] = {
    val path = rolesPath.resolve(ev.toMetaPath.value)
    parseFile(path.toFile).flatMap(_.as[SignedPayload[T]]).toTry
  }

  private def readEtag[T](implicit ev: TufRole[T]): Try[ETag] = Try {
    val lines = Files.readAllLines(rolesPath.resolve(ev.toETagPath)).asScala
    assert(lines.tail.isEmpty)
    ETag(lines.head)
  }.recoverWith {
    case _: FileNotFoundException => Failure(EtagsNotFound)
  }

  private def writeEtag(etag: ETag): Try[Unit] = Try {
    Files.write(rolesPath.resolve(TufRole.targetsTufRole.toETagPath), etag.value.getBytes)
  }

  private def writeUnsignedRole[T : TufRole : Encoder](role: T): Try[Path] =
    writeRole(rolesPath.resolve("unsigned"), role.toMetaPath, role)

  def writeSignedRole[T : TufRole : Encoder](signedPayload: SignedPayload[T]): Try[Path] =
    writeRole(rolesPath, signedPayload.signed.toMetaPath, signedPayload)

  private def writeRole[T : Encoder](path: Path, metaPath: MetaPath, payload: T): Try[Path] = Try {
    val rolePath = path.resolve(metaPath.value)
    Files.write(rolePath, payload.asJson.spaces2.getBytes)
    rolePath
  }

  def genKeys(name: KeyName, keyType: KeyType, keySize: Int): Try[TufKeyPair] =
    keyStorage.genKeys(name, keyType, keySize)

  def rotateRoot(repoClient: UserReposerverClient,
                 newRootName: KeyName,
                 oldRootName: KeyName,
                 newTargetsName: KeyName,
                 oldKeyId: Option[KeyId]): Future[SignedPayload[RootRole]] = {
    for {
      (newRootPubKey, newRootPrivKey) <- keyStorage.readKeyPair(newRootName).toFuture
      (newTargetsPubKey, _) <- keyStorage.readKeyPair(newTargetsName).toFuture
      oldRootRole <- repoClient.root().map(_.signed)
      _ <- ensureTargetsPulled(repoClient, oldRootRole)
      oldRootPubKeyId = oldKeyId.getOrElse(oldRootRole.roles(RoleType.ROOT).keyids.last)
      oldTargetsKeyIds = oldRootRole.roles(RoleType.TARGETS).keyids
      oldRootPubKey = oldRootRole.keys(oldRootPubKeyId)
      oldRootPrivKey <- deleteOrReadKey(repoClient, oldRootName, oldRootPubKeyId)
      _ <- keyStorage.writeKeys(oldRootName, oldRootPubKey, oldRootPrivKey).toFuture
      newKeySet = (oldRootRole.keys -- (oldTargetsKeyIds :+ oldRootPubKeyId)) ++ Map(newRootPubKey.id -> newRootPubKey, newTargetsPubKey.id -> newTargetsPubKey)
      newRootRoleKeys = RoleKeys(Seq(newRootPubKey.id), threshold = 1)
      newTargetsRoleKeys = RoleKeys(Seq(newTargetsPubKey.id), threshold = 1)
      newRootRoleMap = oldRootRole.roles ++ Map(RoleType.ROOT -> newRootRoleKeys, RoleType.TARGETS -> newTargetsRoleKeys)
      newExpireTime = oldRootRole.expires.plus(DEFAULT_EXPIRE_TIME)
      newRootRole = oldRootRole.copy(keys = newKeySet, roles = newRootRoleMap, version = oldRootRole.version + 1, newExpireTime)
      newRootSignature = TufCrypto.signPayload(newRootPrivKey, newRootRole).toClient(newRootPubKey.id)
      newRootOldSignature = TufCrypto.signPayload(oldRootPrivKey, newRootRole).toClient(oldRootPubKeyId)
      newSignedRoot = SignedPayload(Seq(newRootSignature, newRootOldSignature), newRootRole)
      _ = log.debug(s"pushing ${newSignedRoot.asJson.spaces2}")
      _ <- repoClient.pushSignedRoot(newSignedRoot)
      _ <- writeSignedRole(newSignedRoot).toFuture
    } yield newSignedRoot
  }

  def pushTargetsKey(reposerver: UserReposerverClient, keyName: KeyName): Future[TufKey] = {
    keyStorage.readPublicKey(keyName).toFuture.flatMap(reposerver.pushTargetsKey)
  }

  def authConfig: Try[Option[AuthConfig]] = TufRepo.readConfigFile(repoPath).map(_.auth)

  def repoServerUri: Try[URI] = TufRepo.readConfigFile(repoPath).map(_.reposerver)
}
