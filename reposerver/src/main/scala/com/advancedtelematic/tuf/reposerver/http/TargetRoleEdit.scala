package com.advancedtelematic.tuf.reposerver.http

import akka.actor.Scheduler
import com.advancedtelematic.libtuf.data.TufDataType.{JsonSignedPayload, RepoId, TargetFilename, TargetFormat}
import com.advancedtelematic.libtuf_server.repo.server.SignedRoleGeneration
import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType.TargetItem
import com.advancedtelematic.tuf.reposerver.db.{FilenameCommentRepository, TargetItemRepositorySupport}

import scala.concurrent.{ExecutionContext, Future}
import slick.jdbc.MySQLProfile.api._

class TargetRoleEdit(signedRoleGeneration: SignedRoleGeneration)
                    (implicit val db: Database, val ec: ExecutionContext, val scheduler: Scheduler)
  extends TargetItemRepositorySupport with FilenameCommentRepository.Support {

  def addTargetItem(targetItem: TargetItem): Future[JsonSignedPayload] = for {
    _ <- targetItemRepo.persist(targetItem)
    json <- signedRoleGeneration.regenerateAllSignedRoles(targetItem.repoId)
  } yield json

  def deleteTargetItem(repoId: RepoId, filename: TargetFilename): Future[Unit] = for {
    _ <- signedRoleGeneration.ensureTargetsCanBeSigned(repoId)
    _ <- targetItemRepo.deleteItemAndComments(filenameCommentRepo)(repoId, filename)
    _ <- signedRoleGeneration.regenerateAllSignedRoles(repoId)
  } yield ()

  def deleteOsTreeTargets(repoId: RepoId): Future[Seq[TargetItem]] = for {
    _             <- signedRoleGeneration.ensureTargetsCanBeSigned(repoId)
    osTreeTargets <- deleteOsTreeItemAndComments(repoId)
    _             <- signedRoleGeneration.regenerateAllSignedRoles(repoId)
  } yield osTreeTargets

  private def deleteOsTreeItemAndComments(repoId: RepoId): Future[Seq[TargetItem]] = for {
    targetItems   <- targetItemRepo.findFor(repoId)
    osTreeTargets = targetItems.filter(_.custom.exists(_.targetFormat.exists(_ == TargetFormat.OSTREE)))
    _             <- targetItemRepo.deleteItemsAndComments(filenameCommentRepo)(repoId, osTreeTargets.map(_.filename).toSet)
  } yield osTreeTargets
}
