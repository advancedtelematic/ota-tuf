package com.advancedtelematic.tuf.keyserver.db

import java.time.Instant

import cats.data.NonEmptyList
import com.advancedtelematic.libtuf.data.TufDataType._
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType._
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.KeyGenRequestStatus
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.KeyGenRequestStatus.KeyGenRequestStatus
import com.advancedtelematic.libats.http.Errors.{EntityAlreadyExists, MissingEntity}
import slick.driver.MySQLDriver.api._
import com.advancedtelematic.libats.codecs.SlickRefined._
import com.advancedtelematic.libtuf.data.ClientDataType.RootRole
import io.circe.syntax._

import scala.concurrent.ExecutionContext


trait DatabaseSupport {
  implicit val ec: ExecutionContext
  implicit val db: Database
}

import scala.concurrent.{ExecutionContext, Future}

trait KeyGenRequestSupport extends DatabaseSupport {
  lazy val keyGenRepo = new KeyGenRequestRepository()
}

protected [db] class KeyGenRequestRepository()(implicit db: Database, ec: ExecutionContext) {

  import com.advancedtelematic.libats.db.SlickExtensions._
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
                       key: Key,
                       role: Role,
                       keyRepository: KeyRepository,
                       roleRepository: RoleRepository): Future[KeyGenRequest] = {
    val dbIO = for {
      _ <- roleRepository.persistAction(role)
      _ <- setStatusAction(keyGenRequest.id, KeyGenRequestStatus.GENERATED)
      _ <- keyRepository.persistAction(key)
    } yield keyGenRequest.copy(status = KeyGenRequestStatus.GENERATED)

    db.run(dbIO.transactionally)
  }

  def setStatus(genId: KeyGenId, status: KeyGenRequestStatus): Future[KeyGenId] =
    db.run(setStatusAction(genId, status))

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

  protected [db] def setStatusAction(id: KeyGenId, status: KeyGenRequestStatus): DBIO[KeyGenId] =
    keyGenRequests
      .filter(_.id === id)
      .map(_.status)
      .update(status)
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
  import com.advancedtelematic.libats.db.SlickExtensions._
  import com.advancedtelematic.libats.db.SlickPipeToUnit.pipeToUnit

  import Schema.{keys, roles}
  import KeyRepository._


  protected[db] def persist(key: Key): Future[Unit] = db.run(persistAction(key))

  def find(keyId: KeyId): Future[Key] =
    db.run(keys.filter(_.id === keyId).result.failIfNotSingle(KeyNotFound))

  def repoKeys(repoId: RepoId, roleType: RoleType): Future[Seq[Key]] =
    db.run {
      roles.join(keys).on(_.id === _.roleId)
        .filter(_._1.repoId === repoId)
        .filter(_._1.roleType === roleType)
        .map(_._2)
        .result
    }


  def keysFor(repoId: RepoId): Future[Seq[Key]] =
    db.run {
      roles.join(keys).on(_.id === _.roleId)
        .filter(_._1.repoId === repoId)
        .map(_._2)
        .result
    }

  def repoKeysByRole(repoId: RepoId): Future[Map[RoleType, (Role, Seq[Key])]] = {
    db.run {
      val repoKeys = roles
        .filter(_.repoId === repoId)
        .join(keys).on(_.id === _.roleId)
        .result

      repoKeys.map { roleKeys =>
        roleKeys
          .groupBy(_._1.roleType)
          .filter { case (_, values) => values.nonEmpty }
          .mapValues { keysSeq =>
            (keysSeq.head._1, keysSeq.map(_._2))
          }
      }
    }
  }

  def replaceRootKeys(repoId: RepoId, keys: NonEmptyList[Key]): Future[Unit] = db.run {
    val rolesQ =
      Schema.roles
        .filter(_.repoId === repoId)
        .filter(_.roleType === RoleType.ROOT)
        .map(_.id)

    val deleteIO =
      Schema.keys.filter(_.roleId.in(rolesQ)).delete

    deleteIO.andThen(Schema.keys ++= keys.toList).transactionally
  }

  protected [db] def persistAction(key: Key): DBIO[Unit] = {
    Schema.keys.insertOrUpdate(key).map(_ => ())
  }
}

trait RoleRepositorySupport extends DatabaseSupport {
  lazy val roleRepo = new RoleRepository()
}

protected [db] class RoleRepository()(implicit db: Database, ec: ExecutionContext) {

  import com.advancedtelematic.libats.db.SlickExtensions._

  def findByType(repoId: RepoId, roleType: RoleType): Future[Role] = db.run {
    Schema.roles
      .filter(_.repoId === repoId)
      .filter(_.roleType === roleType)
      .result
      .failIfNotSingle(MissingEntity[Role])
  }

  def persist(role: Role): Future[Role] =
    db.run(persistAction(role))

  protected [db] def persistAction(role: Role): DBIO[Role] =
      (Schema.roles += role).map(_ => role)
}

trait RootRoleCacheSupport extends DatabaseSupport {
  lazy val rootRoleCacheRepo = new RootRoleCacheRepository()
}

protected[db] class RootRoleCacheRepository()(implicit db: Database, ec: ExecutionContext) {

  import com.advancedtelematic.libats.db.SlickExtensions._
  import com.advancedtelematic.libtuf.data.TufCodecs._
  import com.advancedtelematic.libtuf.data.ClientCodecs._
  import Schema.signedPayloadRootRoleMapper
  import Schema.rootRoleCaches

  def addCached(repoId: RepoId, signedPayload: SignedPayload[RootRole]): Future[Unit] = {
    val expires = signedPayload.signed.expires

    val io = rootRoleCaches
      .insertOrUpdate((repoId, expires, signedPayload))

    db.run(io).map(_ => ())
  }

  def findCached(repoId: RepoId): Future[Option[SignedPayload[RootRole]]] =
    db.run {
      rootRoleCaches
        .filter(_.expiresAt > Instant.now())
        .filter(_.repoId === repoId)
        .map(_.signedPayload)
        .result
        .headOption
    }
}
