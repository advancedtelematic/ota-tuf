package com.advancedtelematic.tuf.keyserver.db

import java.time.Instant

import com.advancedtelematic.libtuf.data.TufDataType._
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
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
import com.advancedtelematic.libtuf_server.data.TufSlickMappings._
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

  protected [db] def deleteRepoKeys(repoId: RepoId): DBIO[Unit] =
    keys.filter(_.repoId === repoId).delete.map(_ => ())

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
  import Schema.signedPayloadRootRoleMapper
  import Schema.signedRootRoles
  import SignedRootRoleRepository.{MissingSignedRole, RootRoleExists}

  def persist(repoId: RepoId, signedPayload: SignedPayload[RootRole]): Future[Unit] = db.run {
    persistAction(repoId, signedPayload)
  }

  def persistAndDeleteRepoKeys(keyRepository: KeyRepository)(repoId: RepoId, signedPayload: SignedPayload[RootRole]): Future[Unit] = db.run {
    keyRepository.deleteRepoKeys(repoId).andThen(persistAction(repoId, signedPayload).transactionally)
  }
  protected [db] def persistAction(repoId: RepoId, signedPayload: SignedPayload[RootRole]): DBIO[Unit] = {
    val expires = signedPayload.signed.expires

    val io = (signedRootRoles += ((repoId, expires, signedPayload.signed.version, signedPayload)))
      .handleIntegrityErrors(RootRoleExists)

    io.map(_ => ())
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

  def findLatest(repoId: RepoId): Future[SignedPayload[RootRole]] =
    db.run {
      signedRootRoles
        .filter(_.repoId === repoId)
        .sortBy(_.version.desc)
        .take(1)
        .map(_.signedPayload)
        .result
        .failIfNotSingle(MissingSignedRole)
    }

  def findByVersion(repoId: RepoId, version: Int): Future[SignedPayload[RootRole]] =
    db.run {
      signedRootRoles
        .filter(_.repoId === repoId)
        .filter(_.version === version)
        .sortBy(_.version.desc)
        .map(_.signedPayload)
        .result
        .failIfNotSingle(MissingSignedRole)
    }
}
