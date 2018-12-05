package com.advancedtelematic.tuf.cli

import cats.implicits._
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.ClientDataType.RootRole
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.http.{ReposerverClient, TufServerClient}
import com.advancedtelematic.tuf.cli.CliConfigOptionOps._
import com.advancedtelematic.tuf.cli.Commands._
import com.advancedtelematic.tuf.cli.TryToFuture._
import com.advancedtelematic.tuf.cli.repo._
import io.circe.syntax._
import org.slf4j.LoggerFactory

import scala.concurrent.{ExecutionContext, Future}

object CommandHandler {
  private lazy val log = LoggerFactory.getLogger(this.getClass)

  def handle[S <: TufServerClient](tufRepo: => TufRepo[S],
                                   repoServer: => Future[S],
                                   delegationsServer: => Future[ReposerverClient],
                                   userKeyStorage: CliKeyStorage,
                                   config: Config)
                                  (implicit ec: ExecutionContext): Future[Unit] = config.command match {
    case Help =>
      Cli.parser.showUsage()
      Future.successful(())

    case InitRepo =>
      RepoManagement.initialize(config.repoType.valueOrConfigError, tufRepo.repoPath, config.credentialsPath, config.reposerverUrl)
        .map(_ => log.info(s"Finished init for ${config.repoName.valueOrConfigError.value} using ${config.credentialsPath}"))
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
        .initTargets(config.version.valueOrConfigError, config.expires)
        .map(p => log.info(s"Wrote empty targets to $p"))
        .toFuture

    case AddTarget =>
      tufRepo
        .addTarget(config.targetName.valueOrConfigError,
          config.targetVersion.valueOrConfigError,
          config.length,
          config.checksum.valueOrConfigError,
          config.hardwareIds,
          config.targetUri,
          config.targetFormat)
        .map(p => log.info(s"added target to $p"))
        .toFuture

    case DeleteTarget =>
      tufRepo
        .initTargets(config.version.valueOrConfigError, config.expires)
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
      CliUtil.verifyRootFile(config.inputPath.valueOrConfigError).map(_ => ())

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
      RepoManagement.export(tufRepo, config.keyNames.head, config.outputPath.valueOrConfigError).toFuture

    case GenUserKey =>
      config.keyNames.map { keyName =>
        userKeyStorage.genKeys(keyName, config.keyType, config.keySize)
      }.sequence_.toFuture

    case CreateDelegation =>
      Delegations.writeNew(config.outputPath.streamOrStdout).toFuture

    case SignDelegation =>
      config.keyNames
        .map(userKeyStorage.readKeyPair).sequence
        .flatMap { keyPairs =>
          Delegations.signPayload(keyPairs, config.inputPath.valueOrConfigError, config.outputPath.streamOrStdout)
        }.toFuture

    case PushDelegation =>
      delegationsServer.flatMap { server =>
        Delegations.push(server, config.delegationName, config.inputPath.valueOrConfigError)
      }

    case AddTargetDelegation =>
      config.keyPaths.map(CliKeyStorage.readPublicKey).sequence.flatMap { keys =>
        tufRepo.addTargetDelegation(config.delegationName, keys, config.delegatedPaths, threshold = 1)
      }.map(_ => ()).toFuture

    case PullDelegation =>
      delegationsServer.flatMap { server =>
        Delegations.pull(server, config.delegationName, config.outputPath.streamOrStdout).map(_ => ())
      }
  }
}
