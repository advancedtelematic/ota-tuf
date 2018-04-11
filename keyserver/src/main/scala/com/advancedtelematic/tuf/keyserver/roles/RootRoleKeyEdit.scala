package com.advancedtelematic.tuf.keyserver.roles

import akka.http.scaladsl.util.FastFuture
import com.advancedtelematic.libtuf.data.TufDataType.{KeyId, RepoId, TufKeyPair}
import com.advancedtelematic.tuf.keyserver.db.{KeyRepository, KeyRepositorySupport}
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.ClientCodecs._

import scala.async.Async.{async, await}
import scala.concurrent.{ExecutionContext, Future}
import com.advancedtelematic.tuf.keyserver.db.KeyRepository.KeyNotFound
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import slick.jdbc.MySQLProfile.api._
import cats.implicits._
import cats.instances.map
import com.advancedtelematic.libtuf.data.RootManipulationOps._

class RootRoleKeyEdit()
                      (implicit val db: Database, val ec: ExecutionContext)
  extends KeyRepositorySupport {
  val roleSigning = new RoleSigning()
  val signedRootRole = new SignedRootRoles()

  def deletePrivateKey(repoId: RepoId, keyId: KeyId): Future[Unit] = for {
    _ <- ensureIsRepoKey(repoId, keyId)
    _ <- keyRepo.delete(keyId)
  } yield ()

  def findAllKeyPairs2(repoId: RepoId, roleType: RoleType): Future[Seq[TufKeyPair]] = async {
    val root = await(signedRootRole.find(repoId)).signed
    val targetKeyIds = root.roleKeys(roleType).map(_.id)

    val keys = await {
      keyRepo.findAll(targetKeyIds).map(_.map(_.toTufKeyPair))
    }

    await { keys.toList.traverse(Future.fromTry) }
  }

  def findAllKeyPairs(repoId: RepoId, roleType: RoleType): Future[Seq[TufKeyPair]] =
    for {
      rootRole <- signedRootRole.find(repoId)
      targetKeyIds = rootRole.signed.roleKeys(roleType).map(_.id)
      dbKeys <- keyRepo.findAll(targetKeyIds)
      keyPairsT = dbKeys.map(_.toTufKeyPair)
      keyPairs <- Future.fromTry(keyPairsT.toList.sequence)
    } yield keyPairs

  def findKeyPair(repoId: RepoId, keyId: KeyId): Future[TufKeyPair] = {
    for {
      _ <- ensureIsRepoKey(repoId, keyId)
      key <- keyRepo.find(keyId)
      keyPair ← Future.fromTry(key.toTufKeyPair)
    } yield keyPair
  }

  private def ensureIsRepoKey(repoId: RepoId, keyId: KeyId): Future[KeyId] = async {
    val publicKey = await(keyRepo.repoKeys(repoId)).find(_.id == keyId)
    publicKey.map(_.id).getOrElse(throw KeyNotFound)
  }
}
