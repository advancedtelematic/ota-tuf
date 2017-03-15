package com.advancedtelematic.tuf.keyserver.roles

import akka.http.scaladsl.util.FastFuture
import com.advancedtelematic.libtuf.data.ClientDataType.RootRole
import com.advancedtelematic.libtuf.data.TufDataType.{RepoId, SignedPayload}
import com.advancedtelematic.tuf.keyserver.db.SignedRootRolesSupport
import slick.driver.MySQLDriver.api._
import scala.concurrent.{ExecutionContext, Future}

class RootRoleCache(roleSignFn: RepoId => Future[SignedPayload[RootRole]])
                   (implicit val db: Database, val ec: ExecutionContext) extends SignedRootRolesSupport {

  import scala.async.Async._

  def findCached(repoId: RepoId): Future[SignedPayload[RootRole]] =
    signedRootRolesRepo.find(repoId).flatMap {
      case Some(signedPayload) => FastFuture.successful(signedPayload)
      case None => roleSignFn(repoId).flatMap(persistGenerated(repoId))
    }

  def ensureCached[T](repoId: RepoId)(fn: => Future[T]): Future[T] =
    findCached(repoId).flatMap(_ => fn)

  def execAndUpdate[T](repoId: RepoId)(fn: => Future[T]): Future[T] =
    async {
      val result = await(fn)
      val updatedRootRole = await(roleSignFn(repoId))
      await(persistGenerated(repoId)(updatedRootRole))
      result
    }

  private def persistGenerated(repoId: RepoId)(signedRoot: SignedPayload[RootRole]): Future[SignedPayload[RootRole]] = {
    signedRootRolesRepo.addSigned(repoId, signedRoot).map(_ => signedRoot)
  }
}
