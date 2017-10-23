package com.advancedtelematic.tuf.cli

import java.io.{File, FileInputStream, FileOutputStream, InputStream}
import java.net.URI
import java.nio.file.{Files, Path}
import java.time.{Instant, Period}
import java.util.zip.{ZipEntry, ZipFile, ZipOutputStream}

import cats.syntax.either._
import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.data.ClientDataType.{ClientTargetItem, ETag, RoleKeys, RoleTypeToMetaPathOp, RootRole, TargetCustom, TargetsRole, VersionedRole}
import com.advancedtelematic.libtuf.data.TufDataType.{ClientSignature, HardwareIdentifier, KeyId, KeyType, RoleType, SignedPayload, TargetFormat, TargetName, TargetVersion, TufKey, TufPrivateKey, ValidTargetFilename}
import com.advancedtelematic.libats.data.DataType.{HashMethod, ValidChecksum}
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.libtuf.reposerver.UserReposerverClient
import com.advancedtelematic.tuf.cli.DataType.{AuthConfig, KeyName, RepoName}
import TryToFuture._
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, ValidatedNel}
import eu.timepit.refined.refineV
import eu.timepit.refined.api.Refined
import io.circe.{Decoder, Encoder, Json}
import io.circe.jawn.{parse, parseFile}
import io.circe.syntax._
import org.slf4j.LoggerFactory

import scala.collection.JavaConverters._
import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source
import scala.util.{Failure, Success, Try}

class TufRepo(val name: RepoName, val repoPath: Path)(implicit ec: ExecutionContext) {

  import CliCodecs._

  lazy val storage = new CliKeyStorage(repoPath)

  private lazy val log = LoggerFactory.getLogger(this.getClass)

  private lazy val rolesPath = repoPath.resolve("roles")

  val DEFAULT_EXPIRE_TIME = Period.ofDays(365)

  val zipTargetKeyName = KeyName("targets")

  def initFromAuthJson(authJson: File): Try[Unit] =
    for {
      json <- parseFile(authJson).toTry
      authConfig <- json.as[AuthConfig](authConfigDecoder.prepare(_.downField("oauth2"))).toTry
      _ <- Try(Files.write(repoPath.resolve("auth.json"), authConfig.asJson.noSpaces.getBytes))
    } yield ()

  def initFromZip(path: Path, keyName: KeyName): Try[Unit] = {
    import storage.KeyNamePath

    // copy whole ZIP file into repo
    Files.copy(path, repoPath.resolve("credentials.zip"))

    val zipFile = new ZipFile(path.toFile)

    val either = for {
      is <- Either.catchNonFatal(zipFile.getInputStream(zipFile.getEntry("treehub.json")))
      treehubJson <- parse(Source.fromInputStream(is).mkString)
      authConfig <- treehubJson.as[AuthConfig](authConfigDecoder.prepare(_.downField("oauth2")))
      _ <- Either.catchNonFatal(Files.write(repoPath.resolve("auth.json"), authConfig.asJson.noSpaces.getBytes))

      pubis <- Either.catchNonFatal(zipFile.getInputStream(zipFile.getEntry(zipTargetKeyName.publicKeyName)))
      _ <- Either.catchNonFatal(Files.copy(pubis, keyName.publicKeyPath))
      privis <- Either.catchNonFatal(zipFile.getInputStream(zipFile.getEntry(zipTargetKeyName.privateKeyName)))
      _ <- Either.catchNonFatal(Files.copy(privis, keyName.privateKeyPath))
    } yield ()

    zipFile.close()

    either.toTry
  }

  def init(credentialsPath: Path, keyName: KeyName): Try[Unit] = {

    Try(Files.createDirectories(repoPath.resolve("keys"))).flatMap { _ =>

      Try(credentialsPath.getFileName.toString).flatMap { name =>
        if (name.endsWith(".json")) {
          initFromAuthJson(credentialsPath.toFile)
        } else if (name.endsWith(".zip")) {
          initFromZip(credentialsPath, keyName)
        } else {
          Failure(new Exception("unknown file extension"))
        }
      }
    }
  }

  def initTargets(version: Int, expires: Instant): Try[Path] = {
    val emptyTargets = TargetsRole(expires, Map.empty, version)
    writeUnsignedRole(emptyTargets)
  }

  import cats.syntax.option._

  def addTarget(name: TargetName, version: TargetVersion, length: Int, checksum: Refined[String, ValidChecksum], hardwareIds: List[HardwareIdentifier], url: URI): Try[Path] = {
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

  def verifyTargets(targets: SignedPayload[TargetsRole], rootName: KeyName): ValidatedNel[String, SignedPayload[TargetsRole]] =
    storage.readPublicKey(rootName).flatMap { publicRootKey =>
      readUnsignedRole[RootRole](RoleType.ROOT)
    } match {
      case Success(rootRole) =>
        TufCrypto.payloadSignatureIsValid(rootRole, targets)
      case Failure(t) =>
        Invalid(NonEmptyList.of(t.getMessage))
    }

  def saveTargets(targets: SignedPayload[TargetsRole], etag: ETag): Try[Unit] =
    writeSignedRole(targets).flatMap { _ =>
      writeEtag(etag)
    }

  def pullTargets(reposerverClient: UserReposerverClient, rootName: KeyName): Future[Unit] =
    reposerverClient.targets().flatMap { case (targets, etag) =>
      verifyTargets(targets, rootName) match {
        case Valid(_) => saveTargets(targets, etag).toFuture
        case Invalid(s) => Future.failed(new Exception(s"Error pulling targets: ${s.toList.mkString(", ")}"))
      }
    }

  def pushTargets(reposerverClient: UserReposerverClient): Future[SignedPayload[TargetsRole]] =
    readSignedRole[TargetsRole](RoleType.TARGETS).toFuture.flatMap { targets =>
      log.info(s"pushing ${targets.asJson.spaces2}")
      val etag = readEtag[TargetsRole](RoleType.TARGETS)
      log.info(s"etag: $etag")
      reposerverClient.pushTargets(targets, etag.toOption).map(_ => targets)
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

  private def readEtag[T <: VersionedRole](roleType: RoleType): Try[ETag] =
    Try {
      val lines = Source.fromFile(rolesPath.resolve(roleType.toETagPath).toFile).getLines().toTraversable
      assert(lines.tail.isEmpty)
      ETag(lines.head)
    }

  private def writeEtag(etag: ETag): Try[Unit] = Try {
    Files.write(rolesPath.resolve(RoleType.TARGETS.toETagPath), Seq(etag.value).asJava)
  }

  private def writeUnsignedRole[T <: VersionedRole : Encoder](role: T): Try[Path] =
    writeRole(rolesPath.resolve("unsigned"), role.roleType, role)

  private def writeSignedRole[T <: VersionedRole : Encoder](signedPayload: SignedPayload[T]): Try[Path] =
    writeRole(rolesPath, signedPayload.signed.roleType, signedPayload)

  private def writeRole[T: Encoder](path: Path, roleType: RoleType, payload: T): Try[Path] = Try {
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

  def authConfig(): Try[AuthConfig] =
    parseFile(repoPath.resolve("auth.json").toFile)
      .flatMap(_.as[AuthConfig])
      .toTry

  private def toByteArray(is: InputStream) =
    Stream.continually(is.read).takeWhile((-1).!=).map(_.toByte).toArray

  def export(targetKey: KeyName, exportPath: Path): Try[Unit] = {
    import scala.collection.JavaConversions._
    import storage.KeyNamePath

    Try {

      val zipExportStream = new ZipOutputStream(new FileOutputStream(exportPath.toFile))

      try {

        val sourceZip = new ZipFile(repoPath.resolve("credentials.zip").toFile)

        sourceZip.entries().foreach { zipEntry =>
          val is = sourceZip.getInputStream(zipEntry)

          if (zipEntry.getName == "treehub.json") {
            zipExportStream.putNextEntry(new ZipEntry("treehub.json"))
            parse(Source.fromInputStream(is).mkString).map { oldTreehubJson =>
              val newTreehubJson = oldTreehubJson.deepMerge(Json.obj("oauth2" -> authConfig().get.asJson))
              zipExportStream.write(newTreehubJson.spaces2.getBytes)
            }.valueOr(throw _)
          } else if (zipEntry.getName != zipTargetKeyName.publicKeyName
                  && zipEntry.getName != zipTargetKeyName.privateKeyName) {
            // copy other files over
            zipExportStream.putNextEntry(zipEntry)
            zipExportStream.write(toByteArray(is))
          }

          zipExportStream.closeEntry()
        }

        zipExportStream.putNextEntry(new ZipEntry(zipTargetKeyName.publicKeyName))
        val pub = new FileInputStream(targetKey.publicKeyPath.toFile)
        zipExportStream.write(toByteArray(pub))
        zipExportStream.putNextEntry(new ZipEntry(zipTargetKeyName.privateKeyName))
        val priv = new FileInputStream(targetKey.privateKeyPath.toFile)
        zipExportStream.write(toByteArray(priv))

        sourceZip.close()

      } finally {
        zipExportStream.close()
      }
    }
  }
}
