package com.advancedtelematic.tuf.keyserver.http

import java.security.{PrivateKey, PublicKey}

import akka.http.scaladsl.util.FastFuture
import cats.data.NonEmptyList
import com.advancedtelematic.libtuf.crypt.RsaKeyPair
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.libtuf.data.TufDataType._
import com.advancedtelematic.libtuf.data.TufDataType.RepoId
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.{Key, TufPrivateKey}
import com.advancedtelematic.tuf.keyserver.db.KeyRepositorySupport
import com.advancedtelematic.tuf.keyserver.vault.VaultClient
import io.circe.Encoder

import scala.concurrent.{ExecutionContext, Future}
import slick.driver.MySQLDriver.api._
import io.circe.syntax._

object RoleSigning {
  import com.advancedtelematic.libtuf.crypt.CanonicalJson._

  def isValid[T : Encoder](value: T, signature: ClientSignature, publicKey: PublicKey): Boolean = {
    val sig = Signature(signature.sig, signature.method)
    RsaKeyPair.isValid(publicKey, sig, value.asJson.canonical.getBytes)
  }
}

class PrivateKeysFetch(vaultClient: VaultClient)(implicit val db: Database, val ec: ExecutionContext)
  extends KeyRepositorySupport {


  def clientKeys(repoId: RepoId, roleType: RoleType): Future[NonEmptyList[TufPrivateKey]] = {
    val roleKeys = keyRepo.repoKeys(repoId, roleType)

    roleKeys
      .flatMap(fetchPrivateKeys)
      .map(keys => NonEmptyList.fromList(keys))
      .flatMap {
        case None =>
          FastFuture.failed(Errors.RoleKeysNotFound)
        case Some(keys) =>
          FastFuture.successful(keys)
      }
  }

  def fetchPrivateKeys(keys: Seq[Key]): Future[List[TufPrivateKey]] = {
    import cats.syntax.list._
    import cats.implicits._

    keys.map(fetchPrivateKey).toList.sequence
  }

  private def fetchPrivateKey(key: Key): Future[TufPrivateKey] =
    vaultClient.findKey(key.id).flatMap { vaultKey =>
      Future.fromTry {
        RsaKeyPair.parseKeyPair(vaultKey.privateKey).map(_.getPrivate).map { privateKey =>
          TufPrivateKey(key.id, KeyType.RSA, privateKey)
        }
      }
    }
}

class RoleSigning(implicit val db: Database, val ec: ExecutionContext)
  extends KeyRepositorySupport {

  import com.advancedtelematic.libtuf.crypt.CanonicalJson._

  def signAll[T : Encoder](payload: T, keys: NonEmptyList[TufPrivateKey]): SignedPayload[T] = {
    val signatures = keys.map(signForClient(payload)).toList
    SignedPayload(signatures, payload)
  }

  def signForClient[T : Encoder](payload: T)(key: TufPrivateKey): ClientSignature = {
    val signature = calculateSignature(payload, key.value)
    ClientSignature(key.id, signature.method, signature.sig)
  }

  private def calculateSignature[T : Encoder](payload: T, privateKey: PrivateKey): Signature = {
    val bytes = payload.asJson.canonical.getBytes
    RsaKeyPair.sign(privateKey, bytes)
  }
}
