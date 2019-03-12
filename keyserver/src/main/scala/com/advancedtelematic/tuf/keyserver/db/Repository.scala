package com.advancedtelematic.tuf.keyserver.db

import com.advancedtelematic.libtuf.data.TufDataType._
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType._
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.KeyGenRequestStatus
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.KeyGenRequestStatus.KeyGenRequestStatus
import com.advancedtelematic.libats.http.Errors.{EntityAlreadyExists, MissingEntity}
import com.advancedtelematic.libats.slick.codecs.SlickRefined._
import com.advancedtelematic.libtuf.data.ClientDataType.RootRole
import com.advancedtelematic.libats.slick.db.SlickUUIDKey._
import com.advancedtelematic.libats.slick.db.SlickExtensions._
import slick.jdbc.MySQLProfile.api._

import scala.concurrent.{ExecutionContext, Future}
import SlickMappings._

trait DatabaseSupport {
  implicit val ec: ExecutionContext
  implicit val db: Database
}

trait KeyGenRequestSupport extends DatabaseSupport {
  lazy val keyGenRepo = new KeyGenRequestRepository()
}

protected [db] class KeyGenRequestRepository()(implicit db: Database, ec: ExecutionContext) {
  import Schema.keyGenRequests

  val KeyGenRequestNotFound = MissingEntity[KeyGenRequest]()

  def persist(keyGenRequest: KeyGenRequest): Future[KeyGenRequest] = {
    db.run(persistAction(keyGenRequest))
  }

  def persistAll(reqs: Seq[KeyGenRequest]): Future[Seq[KeyGenRequest]] =
    db.run {
      DBIO.sequence {
        reqs.map(persistAction)
      }.transactionally
    }


  def persistGenerated(keyGenRequest: KeyGenRequest,
                       keys: Seq[Key],
                       keyRepository: KeyRepository): Future[KeyGenRequest] = {
    val dbIO = for {
      _ <- setStatusAction(keyGenRequest.id, KeyGenRequestStatus.GENERATED)
      _ <- keyRepository.persistAllAction(keys)
    } yield keyGenRequest.copy(status = KeyGenRequestStatus.GENERATED)

    db.run(dbIO.transactionally)
  }

  def setStatus(genId: KeyGenId, status: KeyGenRequestStatus, cause: Option[Throwable] = None): Future[KeyGenId] = {
    val causeRepr = cause.map(t => s"${t.getClass.getSimpleName}|${t.getMessage}").getOrElse("")
    db.run(setStatusAction(genId, status, causeRepr))
  }

  def setStatusAll(genIds: Seq[KeyGenId], status: KeyGenRequestStatus): Future[Seq[KeyGenId]] =
    db.run(DBIO.sequence(genIds.map(genId => setStatusAction(genId, status))).transactionally)

  def findPending(limit: Int = 1024): Future[Seq[KeyGenRequest]] =
    db.run(keyGenRequests.filter(_.status === KeyGenRequestStatus.REQUESTED).take(limit).result)

  def find(genId: KeyGenId): Future[KeyGenRequest] = {
    db.run(keyGenRequests.filter(_.id === genId).result.failIfNotSingle(KeyGenRequestNotFound))
  }

  def findBy(repoId: RepoId): Future[Seq[KeyGenRequest]] =
    db.run {
      keyGenRequests
        .filter(_.repoId === repoId)
        .result
    }

  protected [db] def setStatusAction(id: KeyGenId, status: KeyGenRequestStatus, cause: String = ""): DBIO[KeyGenId] =
    keyGenRequests
      .filter(_.id === id)
      .map(r => (r.status, r.description))
      .update(status, cause)
      .handleSingleUpdateError(KeyGenRequestNotFound)
      .map(_ => id)


  protected [db] def persistAction(keyGenRequest: KeyGenRequest): DBIO[KeyGenRequest] = {
    (keyGenRequests += keyGenRequest)
      .handleIntegrityErrors(EntityAlreadyExists[KeyGenRequest]())
      .map(_ => keyGenRequest)
  }
}

trait KeyRepositorySupport extends DatabaseSupport {
  lazy val keyRepo = new KeyRepository()
}

object KeyRepository {
  val KeyNotFound = MissingEntity[Key]()
}

protected [db] class KeyRepository()(implicit db: Database, ec: ExecutionContext) {
  import Schema.keys
  import KeyRepository._
  import com.advancedtelematic.libats.slick.db.SlickPipeToUnit.pipeToUnit

  def persist(key: Key): Future[Unit] = db.run(persistAction(key))

  protected [db] def deleteRepoKeys(repoId: RepoId, keysToDelete: Set[KeyId]): DBIO[Unit] =
    keys.filter(_.repoId === repoId).filter(_.id.inSet(keysToDelete)).delete.map(_ => ())

  protected [db] def keepOnlyKeys(repoId: RepoId, keysToKeep: Set[KeyId]): DBIO[Unit] =
    keys.filter(_.repoId === repoId).filterNot(_.id.inSet(keysToKeep)).delete.map(_ => ())

  def find(keyId: KeyId): Future[Key] =
    findAll(Seq(keyId)).map(_.head)

  def findAll(keyIds: Seq[KeyId]): Future[Seq[Key]] =
    db.run(keys.filter(_.id.inSet(keyIds)).result.failIfEmpty(KeyNotFound))

  protected [db] def persistAllAction(keys: Seq[Key]): DBIO[Unit] =
    DBIO.sequence(keys.map(Schema.keys.insertOrUpdate))

  protected [db] def persistAction(key: Key): DBIO[Unit] = {
    Schema.keys.insertOrUpdate(key).map(_ => ())
  }

  def repoKeys(repoId: RepoId): Future[Seq[Key]] = db.run {
    Schema.keys.filter(_.repoId === repoId).result
  }

  def delete(keyId: KeyId): Future[Unit] = db.run {
    keys.filter(_.id === keyId).delete.map(_ => ())
  }
}


trait SignedRootRoleSupport extends DatabaseSupport {
  lazy val signedRootRoleRepo = new SignedRootRoleRepository()
}

object SignedRootRoleRepository {
  val MissingSignedRole = MissingEntity[RootRole]
  val RootRoleExists = EntityAlreadyExists[RootRole]
}

protected[db] class SignedRootRoleRepository()(implicit db: Database, ec: ExecutionContext) {
  import Schema.signedRootRoles
  import SignedRootRoleRepository.{MissingSignedRole, RootRoleExists}

  def persist(signedRootRole: SignedRootRole): Future[Unit] = db.run {
    persistAction(signedRootRole)
  }

  def persistAndKeepRepoKeys(keyRepository: KeyRepository)(signedRootRole: SignedRootRole, keysToKeep: Set[KeyId]): Future[Unit] = db.run {
    keyRepository.keepOnlyKeys(signedRootRole.repoId, keysToKeep).andThen(persistAction(signedRootRole).transactionally)
  }

  protected [db] def persistAction(signedRootRole: SignedRootRole): DBIO[Unit] = {
    (signedRootRoles += signedRootRole)
      .handleIntegrityErrors(RootRoleExists)
      .map(_ => ())
  }

  def nextVersion(repoId: RepoId): Future[Int] = db.run {
    signedRootRoles
      .filter(_.repoId === repoId)
      .sortBy(_.version.desc)
      .map(_.version)
      .result
      .headOption
      .map(_.getOrElse(0) + 1)
  }

  def findLatest(repoId: RepoId): Future[SignedRootRole] =
    db.run {
      signedRootRoles
        .filter(_.repoId === repoId)
        .sortBy(_.version.desc)
        .take(1)
        .result
        .failIfNotSingle(MissingSignedRole)
    }

  def findByVersion(repoId: RepoId, version: Int): Future[SignedRootRole] =
    db.run {
      signedRootRoles
        .filter(_.repoId === repoId)
        .filter(_.version === version)
        .sortBy(_.version.desc)
        .result
        .failIfNotSingle(MissingSignedRole)
    }
}
