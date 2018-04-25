package com.advancedtelematic.tuf.keyserver.db

import akka.stream.Materializer
import akka.Done
import akka.stream.scaladsl.Source
import com.advancedtelematic.libats.slick.codecs.SlickRefined
import com.advancedtelematic.libats.slick.db.SlickEncryptedColumn.EncryptedColumn
import com.advancedtelematic.libtuf.data.TufDataType.{KeyId, TufKeyPair, ValidKeyId}
import com.advancedtelematic.libtuf_server.data.TufSlickMappings._
import scala.concurrent.{ExecutionContext, Future}
import slick.jdbc.MySQLProfile.api._
import com.advancedtelematic.tuf.keyserver.vault.VaultClient
import org.slf4j.LoggerFactory
import slick.jdbc.GetResult
import com.advancedtelematic.libats.slick.codecs.SlickRefined._
import eu.timepit.refined.api.Refined

class MovePrivateKeysFromVaultMigration(vaultClient: VaultClient)(implicit mat: Materializer, val ec: ExecutionContext, val db: Database)
  extends KeyRepositorySupport {

  private val logger = LoggerFactory.getLogger(this.getClass)

  private lazy val query = sql"""select key_id from `keys` where private_key is null""".as[KeyId]

  private lazy val keyIdSource = Source.fromPublisher(db.stream(query))

  private def fetchPrivateKey(keyId: KeyId): Future[TufKeyPair] =
    vaultClient.findKey(keyId).flatMap(key => Future.fromTry(key.toTufKeyPair))

  private def updateDbKey(keyPair: TufKeyPair): Future[TufKeyPair] = {
    val q = Schema.keys.filter(_.id === keyPair.pubkey.id).map(_.privateKey).update(EncryptedColumn(keyPair.privkey))
    db.run(q).map(_ => keyPair)
  }

  private def deleteDbKey(keyId: KeyId): Future[KeyId] =
    db.run(Schema.keys.filter(_.id === keyId).delete.map(_ => keyId))

  private def fetchUpdate(keyId: KeyId): Future[Either[(KeyId, Throwable), TufKeyPair]] = {
    fetchPrivateKey(keyId)
      .flatMap(updateDbKey)
      .map(Right.apply)
      .recoverWith {
        case VaultClient.VaultResourceNotFound =>
          logger.info(s"key $keyId is offline, deleting from local db")
          deleteDbKey(keyId).map(_ => Left((keyId, VaultClient.VaultResourceNotFound)))

        case ex => Future.successful(Left((keyId, ex)))
      }
  }

  def migrate: Future[Done] = {
    val updateStream = keyIdSource.mapAsync(5)(fetchUpdate)

    updateStream.runForeach {
      case Right(keyPair) =>
        logger.info(s"Updated ${keyPair.pubkey.id} from vault")
      case Left((_, VaultClient.VaultResourceNotFound)) =>
        logger.debug("key was offline")
      case Left((keyid, ex)) =>
        logger.warn(s"Could not update key id: $keyid", ex)
    }
  }

  implicit private val getRowResult: GetResult[KeyId] = slick.jdbc.GetResult { r =>
    SlickRefined.refinedMappedType[String, ValidKeyId, Refined].getValue(r.rs, 1)
  }
}
