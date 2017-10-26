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
                       role: Role,
                       keyRepository: KeyRepository,
                       roleRepository: RoleRepository): Future[KeyGenRequest] = {
    val dbIO = for {
      _ <- roleRepository.persistAction(role)
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
  import Schema.{keys, roles}
  import KeyRepository._
  import com.advancedtelematic.libats.slick.db.SlickPipeToUnit.pipeToUnit

  def persist(key: Key): Future[Unit] = db.run(persistAction(key))

  def find(keyId: KeyId): Future[Key] =
    db.run(keys.filter(_.id === keyId).result.failIfNotSingle(KeyNotFound))

  def findAll(keyIds: Seq[KeyId]): Future[Seq[Key]] =
    db.run(keys.filter(_.id.inSet(keyIds)).result)

  def repoKeysForRole(repoId: RepoId, roleType: RoleType): Future[Seq[Key]] =
    db.run {
      roles.join(keys).on(_.id === _.roleId)
        .filter(_._1.repoId === repoId)
        .filter(_._1.roleType === roleType)
        .map(_._2)
        .result
    }

  def repoKeys(repoId: RepoId): Future[Seq[Key]] =
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

  protected [db] def persistAllAction(keys: Seq[Key]): DBIO[Unit] =
    DBIO.sequence(keys.map(Schema.keys.insertOrUpdate))

  protected [db] def persistAction(key: Key): DBIO[Unit] = {
    Schema.keys.insertOrUpdate(key).map(_ => ())
  }
}

trait RoleRepositorySupport extends DatabaseSupport {
  lazy val roleRepo = new RoleRepository()
}

protected [db] class RoleRepository()(implicit db: Database, ec: ExecutionContext) {
  def persist(role: Role): Future[Role] =
    db.run(persistAction(role))

  def find(repoId: RepoId, roleType: RoleType): Future[Role] = db.run {
    Schema.roles
      .filter(_.repoId === repoId)
      .filter(_.roleType === roleType)
      .result
      .failIfNotSingle(MissingEntity[Role])
  }

  def update(role: Role): Future[Role] = {
    db.run(Schema.roles.filter(_.id === role.id).update(role).map(_ => role))
  }

  protected [db] def persistAction(role: Role): DBIO[Role] =
      (Schema.roles += role).map(_ => role)
}

trait SignedRootRoleSupport extends DatabaseSupport {
  lazy val signedRootRoleRepo = new SignedRootRoleRepository()
}

object SignedRootRoleRepository {
  val MissingSignedRole = MissingEntity[SignedPayload[RootRole]]
}

protected[db] class SignedRootRoleRepository()(implicit db: Database, ec: ExecutionContext) {
  import Schema.signedPayloadRootRoleMapper
  import Schema.signedRootRoles
  import SignedRootRoleRepository.MissingSignedRole

  def persist(repoId: RepoId, signedPayload: SignedPayload[RootRole]): Future[Unit] = {
    val expires = signedPayload.signed.expires

    val io = signedRootRoles
      .insertOrUpdate((repoId, expires, signedPayload.signed.version, signedPayload))

    db.run(io).map(_ => ())
  }

  def storeKeys(repoId: RepoId, rootRole: RootRole): Future[Unit] = {
    def cleanKeys(roleIds: Set[RoleId]): DBIO[_] =
      Schema.keys
        .filter(_.roleId.inSet(roleIds))
        .delete

    def addKeys(newRoot: RootRole, existingRoles: Map[RoleType, RoleId]): DBIO[_] = {
      val newKeys = existingRoles.flatMap { case (roleType, roleId) =>
        newRoot.roles(roleType).keyids.map { keyid =>
          val tuf = newRoot.keys(keyid)
          Key(keyid, roleId, tuf.keytype, tuf.keyval)
        }
      }

      Schema.keys ++= newKeys
    }

    val existingRolesIO: DBIO[Map[RoleType, RoleId]] =
      Schema.roles
        .filter(_.repoId === repoId)
        .map(role => role.roleType -> role.id)
        .result
        .map(_.toMap)

    val act = for {
      existingRoles <- existingRolesIO
      _ <- cleanKeys(existingRoles.values.toSet)
      _ <- addKeys(rootRole, existingRoles)
    } yield ()

    db.run(act.transactionally)
  }

  def nextVersion(repoId: RepoId): Future[Int] = db.run {
    signedRootRoles
      .filter(_.repoId === repoId)
      .map(_.version)
      .result
      .headOption
      .map(_.getOrElse(0) + 1)
  }

  def find(repoId: RepoId): Future[SignedPayload[RootRole]] =
    db.run {
      signedRootRoles
        .filter(_.expiresAt > Instant.now())
        .filter(_.repoId === repoId)
        .map(_.signedPayload)
        .result
        .failIfNotSingle(MissingSignedRole)
    }
}
