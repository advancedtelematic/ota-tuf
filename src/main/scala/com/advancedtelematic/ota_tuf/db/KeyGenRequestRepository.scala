package com.advancedtelematic.ota_tuf.db

import com.advancedtelematic.ota_tuf.data.DataType.{Key, KeyGenRequest, KeyId}
import com.advancedtelematic.ota_tuf.data.KeyGenRequestStatus
import org.genivi.sota.http.Errors.{EntityAlreadyExists, MissingEntity}
import slick.driver.MySQLDriver.api._
import KeyId._
import com.advancedtelematic.ota_tuf.data.KeyGenRequestStatus.KeyGenRequestStatus

import scala.concurrent.{ExecutionContext, Future}

trait KeyGenRequestSupport {
  def keyGenRepo(implicit db: Database, ec: ExecutionContext) = new KeyGenRequestRepository()
}

protected class KeyGenRequestRepository()(implicit db: Database, ec: ExecutionContext) {
  import com.advancedtelematic.ota_tuf.data.DataType._
  import org.genivi.sota.db.SlickExtensions._
  import org.genivi.sota.db.Operators._

  val keyGenRequestNotFound = MissingEntity(classOf[KeyGenRequest])

  def persist(keyGenRequest: KeyGenRequest): Future[KeyGenRequest] = {
    db.run(persistAction(keyGenRequest))
  }

  def persistGenerated(keyGenRequest: KeyGenRequest, key: Key, keyRepository: KeyRepository): Future[KeyGenRequest] = {
    val dbIO = for {
      newKgr ← persistAction(keyGenRequest.copy(status = KeyGenRequestStatus.GENERATED))
      _ ← keyRepository.persistAction(key)
    } yield newKgr

    db.run(dbIO.transactionally)
  }

  def setStatus(keyId: KeyId, status: KeyGenRequestStatus): Future[KeyId] =
    db.run {
      Schema.keyGenRequests
        .filter(_.id === keyId)
        .map(_.status)
        .update(status)
        .map(_ => keyId)
    }

  def findPending(limit: Int = 1024): Future[Seq[KeyGenRequest]] =
    db.run(Schema.keyGenRequests.filter(_.status === KeyGenRequestStatus.REQUESTED).take(limit).result)

  def find(keyId: KeyId): Future[KeyGenRequest] = {
    db.run(Schema.keyGenRequests.filter(_.id === keyId).result.failIfNotSingle(keyGenRequestNotFound))
  }

  protected [db] def persistAction(keyGenRequest: KeyGenRequest): DBIO[KeyGenRequest] = {
    Schema.keyGenRequests.insertOrUpdate(keyGenRequest).map(_ => keyGenRequest)
  }
}

trait KeyRepositorySupport {
  def keyRepo(implicit db: Database, ec: ExecutionContext) = new KeyRepository()
}

class KeyRepository()(implicit db: Database, ec: ExecutionContext) {
  import org.genivi.sota.db.SlickExtensions._

  val KeyNotFound = MissingEntity(classOf[Key])

  protected[db] def persist(key: Key): Future[Unit] = db.run(persistAction(key))

  def find(keyId: KeyId): Future[Key] =
    db.run(Schema.keys.filter(_.id === keyId).result.failIfNotSingle(KeyNotFound))

  protected [db] def persistAction(key: Key): DBIO[Unit] = {
    Schema.keys.insertOrUpdate(key).map(_ ⇒ ())
  }
}
