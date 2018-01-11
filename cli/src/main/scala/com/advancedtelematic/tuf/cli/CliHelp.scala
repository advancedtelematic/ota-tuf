package com.advancedtelematic.tuf.cli

import com.advancedtelematic.tuf.cli.repo.TufRepo.TargetsPullError
import org.slf4j.LoggerFactory

import scala.concurrent.Future

object CliHelp {
  lazy val log = LoggerFactory.getLogger(this.getClass)

  val showError: PartialFunction[Throwable, Throwable] = {
    case ex =>
      log.info(ex.getMessage)
      ex
  }

  private val explainError: PartialFunction[Throwable, Throwable] = showError andThen {
    case ex: TargetsPullError =>
      log.info(
        s"""
          |Common causes for this error include:
          |
          |- You are trying to pull a version of targets.json signed with keys no longer present on root.json.
          |  This happens if you rotate root.json and try to pull targets.json without first pushing a targets.json signed with the new targets.keys.
          |
          |  Sign a targets.json with the keys used to rotate root.json, push it to the server and try again.
          |
          |- The targets public key present in the credentials.zip file used to initialize this repository is not the same key the server is using as a targets key.
          |  Did you already rotate a root.json with different keys and you are trying to rotate it again using old keys?
          |  Did you remove the public key currently associated to the targets role?
          |
          |  If the targets key is still online on the server, you can try downloading a fresh credentials.zip, otherwise you will need to add this public key manually to this local tuf repository.
          |
          |- The Server could not return a valid etag when pulling targets.
          |""".stripMargin)
      ex
    case ex => log.error("", ex)
      ex
  }

  val explainErrorHandler: PartialFunction[Throwable, Future[Unit]] = {
    case ex if explainError.isDefinedAt(ex) =>
      Future.failed(explainError(ex))
    case ex =>
      Future.failed(ex)
  }
}
