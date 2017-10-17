package com.advancedtelematic.tuf.keyserver.http

import akka.http.scaladsl.util.FastFuture
import cats.data.ValidatedNel
import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.libtuf.data.TufDataType._
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.Key
import com.advancedtelematic.libtuf.data.TufDataType.RepoId
import com.advancedtelematic.tuf.keyserver.db.KeyRepositorySupport
import com.advancedtelematic.tuf.keyserver.vault.VaultClient
import io.circe.Encoder

import scala.concurrent.{ExecutionContext, Future}
import slick.jdbc.MySQLProfile.api._

class RoleSigning(vaultClient: VaultClient)(implicit val db: Database, val ec: ExecutionContext)
  extends KeyRepositorySupport {

  def signFor[T : Encoder](repoId: RepoId, roleType: RoleType, payload: T): Future[SignedPayload[T]] = {
    val roleKeys = keyRepo.repoKeysForRole(repoId, roleType)

    roleKeys.flatMap {
      case Nil =>
        FastFuture.failed(Errors.RoleKeysNotFound)
      case keys =>
        signAll(payload, keys)
    }.recoverWith {
      case VaultClient.VaultKeyNotFound => Future.failed(Errors.PrivateKeysNotFound)
    }
  }

  def signAll[T : Encoder](payload: T, keys: Seq[Key]): Future[SignedPayload[T]] = {
    Future.sequence {
      keys.map(signForClient(payload))
    }.map { signatures =>
      SignedPayload(signatures, payload)
    }
  }

  def signForClient[T : Encoder](payload: T)(key: Key): Future[ClientSignature] = {
    fetchPrivateKey(key).map { privateKey =>
      val signature = TufCrypto.signPayload(privateKey, payload)
      ClientSignature(key.id, signature.method, signature.sig)
    }
  }

  def verifySignatures[T : Encoder](repoId: RepoId, signedPayload: SignedPayload[T]): Future[ValidatedNel[String, List[ClientSignature]]] = {
    val publicKeysF = keyRepo.repoKeysForRole(repoId, RoleType.ROOT)

    publicKeysF.map { publicKeys =>
      TufCrypto.verifySignatures(signedPayload, publicKeys.view.map(key => key.toTufKey))
    }
  }

  private def fetchPrivateKey(key: Key): Future[TufPrivateKey] =
    vaultClient.findKey(key.id).map(_.privateKey)
}
