package com.advancedtelematic.tuf.cli

import java.nio.file.Files

import com.advancedtelematic.libtuf.data.ClientDataType.RootRole
import com.advancedtelematic.libtuf.reposerver.{UserDirectorHttpClient, UserReposerverClient, UserTufServerClient}
import com.advancedtelematic.tuf.cli.Commands._
import com.advancedtelematic.tuf.cli.DataType.{Director, RepoServer}
import com.advancedtelematic.tuf.cli.repo.{DirectorRepo, RepoManagement, RepoServerRepo, TufRepo}
import org.slf4j.LoggerFactory
import TryToFuture._
import io.circe.syntax._
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.ClientCodecs._

import scala.concurrent.{ExecutionContext, Future}

case class MissingTargetKeyname() extends Exception

abstract class Executor[C <: UserTufServerClient, R <: TufRepo](config: Config, repoServer: Future[C], tufRepo: R)
                                                               (implicit val ec: ExecutionContext) {

  protected lazy val log = LoggerFactory.getLogger(this.getClass)

  Files.createDirectories(config.home)

  lazy val repoPath = config.home.resolve(config.repoName.value)

  def dispatch: PartialFunction[Command, Future[Unit]] = {
    case GenKeys =>
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

    case Export =>
      RepoManagement.export(tufRepo, config.keyNames.head, config.exportPath).toFuture

    case VerifyRoot =>
      CliUtil.verifyRootFile(config.inputPath).map(_ => ())

    case Help =>
      Cli.parser.showUsage()
      Future.successful(())
  }
}

class ReposerverExecutor(config: Config, repoServer: Future[UserReposerverClient], tufRepo: RepoServerRepo)
  extends Executor[UserReposerverClient, RepoServerRepo](config, repoServer, tufRepo)(ExecutionContext.Implicits.global) {

  val reposerverDispatch: PartialFunction[Command, Future[Unit]] = {
    case InitRepo =>
      RepoManagement.initialize(RepoServer, config.repoName, repoPath, config.credentialsPath, config.reposerverUrl)
        .map(_ => log.info(s"Finished init for ${config.repoName.value} using ${config.credentialsPath}"))
        .toFuture

    case MoveOffline =>
      if (config.keyNames.isEmpty) {
        Future.failed(MissingTargetKeyname())
      } else {
        repoServer
          .flatMap { client =>
            tufRepo.moveRootOffline(client,
              config.rootKey,
              config.oldRootKey,
              config.keyNames.head,
              config.oldKeyId)
          }
          .map(_ => log.info(s"root keys are offline, root.json saved to $repoPath"))
      }
    case GetTargets =>
      repoServer
        .flatMap(_.targets())
        .map { targetsResponse => log.info(targetsResponse.targets.asJson.spaces2) }

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
        .deleteTarget(config.targetFilename.get)
        .map(p => log.info(s"deleted target ${config.targetFilename.get} in $p"))
        .toFuture

    case SignTargets =>
      tufRepo
        .signTargets(config.keyNames, config.version)
        .map(p => log.info(s"signed targets.json to $p"))
        .toFuture

    case PullTargets =>
      repoServer.zip(tufRepo.readSignedRole[RootRole].toFuture)
        .flatMap { case (r, rootRole) => tufRepo.pullTargets(r, rootRole.signed) }
        .map(_ => log.info("Pulled targets"))

    case PushTargets =>
      repoServer
        .flatMap(tufRepo.pushTargets)
        .map(_ => log.info("Pushed targets"))
  }

  override val dispatch: PartialFunction[Command, Future[Unit]] = reposerverDispatch orElse super.dispatch

}

class DirectorExecutor(config: Config, repoServer: Future[UserDirectorHttpClient], tufRepo: DirectorRepo)
  extends Executor[UserDirectorHttpClient, DirectorRepo](config, repoServer, tufRepo)(ExecutionContext.Implicits.global) {

  val directorDispatch: PartialFunction[Command, Future[Unit]] = {
    case InitRepo =>
      RepoManagement.initialize(Director, config.repoName, repoPath, config.credentialsPath, config.reposerverUrl)
        .map(_ => log.info(s"Finished init for ${config.repoName.value} using ${config.credentialsPath}"))
        .toFuture

    case MoveOffline =>
      repoServer
        .flatMap { client =>
          tufRepo.moveRootOffline(client,
            config.rootKey,
            config.oldRootKey,
            config.oldKeyId)
        }
        .map(_ => log.info(s"root keys are offline, root.json saved to $repoPath"))
  }

  override val dispatch: PartialFunction[Command, Future[Unit]] = directorDispatch orElse super.dispatch orElse {
    case _ => Future.successful(log.error("Command not applicable for director!"))
  }

}

