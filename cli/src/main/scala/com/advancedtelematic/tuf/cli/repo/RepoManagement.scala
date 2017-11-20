package com.advancedtelematic.tuf.cli.repo

import java.io._
import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipFile, ZipOutputStream}

import cats.implicits._
import com.advancedtelematic.libtuf.data.ClientDataType.TufRoleOps
import com.advancedtelematic.libtuf.data.ClientDataType.{RootRole, TufRole}
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.ClientDataType.TufRole._
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.TufDataType.{SignedPayload, TufKey, TufPrivateKey}
import com.advancedtelematic.tuf.cli.DataType.{AuthConfig, KeyName, RepoName}
import com.advancedtelematic.tuf.cli.repo.TufRepo.UnknownInitFile
import com.advancedtelematic.tuf.cli.CliCodecs._
import io.circe.jawn._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import org.slf4j.LoggerFactory
import scala.collection.JavaConversions._
import scala.concurrent.ExecutionContext
import scala.io.Source
import scala.util.{Failure, Success, Try}

object RepoManagement {
  private val zipTargetKeyName = KeyName("targets")

  private lazy val _log = LoggerFactory.getLogger(this.getClass)

  def initialize(repoName: RepoName, repoPath: Path, initFilePath: Path)(implicit ec: ExecutionContext): Try[TufRepo] = Try {
    Files.createDirectories(repoPath.resolve("keys"))
    initFilePath.getFileName.toString
  }.flatMap { name =>
    if (name.endsWith(".json")) {
      JsonRepoInitialization.init(repoName, repoPath, initFilePath)
    } else if (name.endsWith(".zip")) {
      ZipRepoInitialization.init(repoName, repoPath, zipTargetKeyName, initFilePath)
    } else {
      Failure(UnknownInitFile(initFilePath))
    }
  }

  def export(repo: TufRepo, targetKey: KeyName, exportPath: Path): Try[Unit] = {
    def copyAuth(src: ZipFile, dest: ZipOutputStream): Try[Unit] = {
      for {
        _ <- Try(dest.putNextEntry(new ZipEntry("treehub.json")))
        oldTreehubJson <- repo.authConfig().map(_.asJson)
        newTreehubJson = oldTreehubJson.deepMerge(Json.obj("oauth2" -> oldTreehubJson.asJson))
        _ <- Try(dest.write(newTreehubJson.spaces2.getBytes))
        _ <- Try(dest.closeEntry())
      } yield ()
    }

    def copyEntries(src: ZipFile, dest: ZipOutputStream): Try[Unit] = {
      val entries = src.entries().map { zipEntry =>
        val is = src.getInputStream(zipEntry)

        val copyTry =
          if (
            zipEntry.getName != "treehub.json" &&
              zipEntry.getName != zipTargetKeyName.publicKeyName &&
              zipEntry.getName != zipTargetKeyName.privateKeyName) {
            Try {
              dest.putNextEntry(zipEntry)
              dest.write(toByteArray(is))
            }
          } else {
            Success(())
          }

        copyTry.flatMap { _ => Try(dest.closeEntry()) }
      }

      entries.toList.sequenceU.map(_ => ())
    }

    def copyKeyPair(pubKey: TufKey, privKey: TufPrivateKey, dest: ZipOutputStream): Try[Unit] = Try {
      dest.putNextEntry(new ZipEntry(zipTargetKeyName.publicKeyName))
      dest.write(pubKey.asJson.spaces2.getBytes())
      dest.closeEntry()

      dest.putNextEntry(new ZipEntry(zipTargetKeyName.privateKeyName))
      dest.write(privKey.asJson.spaces2.getBytes())
      dest.closeEntry()
    }

    def copyRole[T : TufRole : Encoder : Decoder](repo: TufRepo, dest: ZipOutputStream): Try[Unit] = {
      repo.readSignedRole[T].map { role =>
        dest.putNextEntry(new ZipEntry(role.signed.toMetaPath.value))
        dest.write(role.asJson.spaces2.getBytes)
        dest.closeEntry()
      }
    }

    def ensureSourceZipExists(repoPath: Path): Try[ZipFile] = {
      val zipPath = repo.repoPath.resolve("credentials.zip")

      Try(new ZipFile(zipPath.toFile)).recoverWith {
        case _: FileNotFoundException =>
          _log.info("zip file not found in repo, creating empty zip file")

          Try {
            val zos = new ZipOutputStream(new FileOutputStream(zipPath.toFile))
            zos.close()
            new ZipFile(zipPath.toFile)
          }
      }
    }

    Try(new ZipOutputStream(new FileOutputStream(exportPath.toFile))).flatMap { zipExportStream ⇒
      ensureSourceZipExists(repo.repoPath).flatMap { sourceZip =>
        val t = for {
          (pubKey, privKey) <- repo.keyStorage.readKeyPair(targetKey)
          _ <- copyAuth(sourceZip, zipExportStream)
          _ ← copyEntries(sourceZip, zipExportStream)
          _ ← copyKeyPair(pubKey, privKey, zipExportStream)
          _ <- copyRole[RootRole](repo, zipExportStream).recover {
            case ex =>
              _log.warn(s"Could not copy RootRole: ${ex.getMessage}")
          }
        } yield ()

        Try(sourceZip.close())

        Try(zipExportStream.close())

        t
      }
    }
  }

  private def toByteArray(is: InputStream): Array[Byte] = {
    val baos = new ByteArrayOutputStream()
    Stream.continually(is.read).takeWhile(_ != -1).foreach(baos.write)
    baos.toByteArray
  }
}

protected object JsonRepoInitialization {
  def init(repoName: RepoName, repoPath: Path, initFilePath: Path)(implicit ec: ExecutionContext): Try[TufRepo] = {
    for {
      json <- parseFile(initFilePath.toFile).toTry
      authConfig <- json.as[AuthConfig](authConfigDecoder.prepare(_.downField("oauth2"))).toTry
      _ <- Try(Files.write(repoPath.resolve("auth.json"), authConfig.asJson.noSpaces.getBytes))
    } yield new TufRepo(repoName, repoPath)
  }
}

protected object ZipRepoInitialization {
  import com.advancedtelematic.tuf.cli.CliCodecs._

  private lazy val _log = LoggerFactory.getLogger(this.getClass)

  def init(repoName: RepoName, repoPath: Path, zipTargetKeyName: KeyName, initFilePath: Path)(implicit ec: ExecutionContext): Try[TufRepo] = {
    val tufRepo = new TufRepo(repoName, repoPath)

    // copy whole ZIP file into repo
    Files.copy(initFilePath, repoPath.resolve("credentials.zip"))

    def writeAuthFile(src: ZipFile): Try[Unit] = for {
      is <- Try(src.getInputStream(src.getEntry("treehub.json")))
      decoder = authConfigDecoder.prepare(_.downField("oauth2"))
      authConfig ← readJsonFrom[AuthConfig](is)(decoder)
      _ ← Try(Files.write(repoPath.resolve("auth.json"), authConfig.asJson.noSpaces.getBytes))
    } yield ()

    def writeTargetKeys(src: ZipFile): Try[Unit] = for {
      pubKeyIs <- Try(src.getInputStream(src.getEntry(zipTargetKeyName.publicKeyName)))
      pubKey <- readJsonFrom[TufKey](pubKeyIs)

      privateKeyIs <- Try(src.getInputStream(src.getEntry(zipTargetKeyName.privateKeyName)))
      privKey <- readJsonFrom[TufPrivateKey](privateKeyIs)

      _ <- tufRepo.keyStorage.writeKeys(zipTargetKeyName, pubKey, privKey)
    } yield ()

    def writeRoot(src: ZipFile): Try[Unit] = for {
      rootIs <- Try(src.getInputStream(src.getEntry(TufRole.rootTufRole.toMetaPath.value)))
      rootRole <- readJsonFrom[SignedPayload[RootRole]](rootIs)
      _ <- tufRepo.writeSignedRole(rootRole)
    } yield ()

    for {
      src ← Try(new ZipFile(initFilePath.toFile))
      _ <- writeAuthFile(src)
      _ <- writeRoot(src).recover { case ex =>
        _log.warn(s"Could not read/write root.json from credentials zip file: ${ex.getMessage}. Continuing.")
      }
      _ <- writeTargetKeys(src).recover { case ex =>
        _log.warn(s"Could not read/write target keys from credentials zip file: ${ex.getMessage}. Continuing.")
      }
      _ = Try(src.close())
    } yield tufRepo
  }

  private def readJsonFrom[T](is: InputStream)(implicit decoder: Decoder[T]): Try[T] = {
    parse(Source.fromInputStream(is).mkString).flatMap(_.as[T](decoder)).toTry
  }
}
