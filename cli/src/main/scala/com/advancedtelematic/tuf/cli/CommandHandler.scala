package com.advancedtelematic.tuf.cli

import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.ClientDataType.RootRole
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.http.TufServerClient
import com.advancedtelematic.tuf.cli.Commands._
import com.advancedtelematic.tuf.cli.TryToFuture._
import com.advancedtelematic.tuf.cli.repo._
import io.circe.syntax._
import org.slf4j.LoggerFactory
import cats.implicits._

import scala.concurrent.{ExecutionContext, Future}

object CommandHandler {
  private lazy val log = LoggerFactory.getLogger(this.getClass)

  def handle[S <: TufServerClient](tufRepo: TufRepo[S],
                                   userKeyStorage: CliKeyStorage,
                                   repoServer: => Future[S])(config: Config)
                                  (implicit ec: ExecutionContext): Future[Unit] = config.command match {
    case Help =>
      Cli.parser.showUsage()
      Future.successful(())

    case InitRepo =>
      RepoManagement.initialize(config.repoType.get, config.repoName, tufRepo.repoPath, config.credentialsPath, config.reposerverUrl)
        .map(_ => log.info(s"Finished init for ${config.repoName.value} using ${config.credentialsPath}"))
        .toFuture

    case MoveOffline =>
      repoServer.flatMap { client =>
        tufRepo.moveRootOffline(client,
          config.rootKey,
          config.oldRootKey,
          config.oldKeyId,
          config.keyNames.headOption)
          .map(_ => log.info(s"root keys moved offline, root.json saved to ${tufRepo.repoPath}"))
      }

    case GetTargets =>
      repoServer.flatMap { client =>
        tufRepo.pullTargets(client).map { targetsResponse => log.info(targetsResponse.asJson.spaces2) }
      }

    case InitTargets =>
      tufRepo
        .initTargets(config.version.get, config.expires)
        .map(p => log.info(s"Wrote empty targets to $p"))
        .toFuture

    case AddTarget =>
      tufRepo
        .addTarget(config.targetName.get,
          config.targetVersion.get,
          config.length,
          config.checksum.get,
          config.hardwareIds,
          config.targetUri,
          config.targetFormat)
        .map(p => log.info(s"added target to $p"))
        .toFuture

    case DeleteTarget =>
      tufRepo
        .initTargets(config.version.get, config.expires)
        .map(p => log.info(s"Wrote empty targets to $p"))
        .toFuture

    case SignTargets =>
      tufRepo
        .signTargets(config.keyNames, config.version)
        .map(p => log.info(s"signed targets.json to $p"))
        .toFuture

    case PullTargets =>
      for {
        client <- repoServer
        rootRole <- tufRepo.readSignedRole[RootRole].toFuture
        _ <- tufRepo.pullVerifyTargets(client, rootRole.signed)
      } yield
        log.info("Pulled targets")

    case PushTargets =>
      repoServer
        .flatMap(tufRepo.pushTargets)
        .map(_ => log.info("Pushed targets"))

    case VerifyRoot =>
      CliUtil.verifyRootFile(config.inputPath).map(_ => ())

    case GenRepoKeys =>
      tufRepo.genKeys(config.rootKey, config.keyType, config.keySize).toFuture.map(_ => ())

    case PullRoot =>
      repoServer.flatMap(client => tufRepo.pullRoot(client, config.force))
        .map(_ => log.info("Pulled root.json"))

    case PushRoot =>
      repoServer.flatMap(client => tufRepo.pushRoot(client))
        .map(_ => log.info("Pushed root.json"))

    case SignRoot =>
      tufRepo.signRoot(config.keyNames).toFuture
        .map(p => log.info(s"signed root.json saved to $p"))

    case AddRootKey =>
      tufRepo.addRootKeys(config.keyNames).toFuture
        .map(p => log.info(s"keys added to unsigned root.json saved to $p"))

    case RemoveRootKey =>
      tufRepo.keyIdsByName(config.keyNames).flatMap { keyIds =>
        tufRepo.removeRootKeys(config.keyIds ++ keyIds)
      }.map(p => log.info(s"keys removed from unsigned root.json saved to $p")).toFuture

    case ExportRepository =>
      RepoManagement.export(tufRepo, config.keyNames.head, config.exportPath).toFuture

    case GenUserKey =>
      config.keyNames.map { keyName =>
        userKeyStorage.genKeys(keyName, config.keyType, config.keySize)
      }.sequence_.toFuture

    case CreateDelegation =>
      Delegations.writeNew(config.exportPath).toFuture

    case SignDelegation =>
      config.keyNames
        .map(userKeyStorage.readKeyPair).sequence
        .flatMap { keyPairs =>
          Delegations.signPayload(keyPairs, config.inputPath, config.exportPath)
        }.toFuture
  }
}
