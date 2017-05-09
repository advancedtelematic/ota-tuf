package com.advancedtelematic.tuf.keyserver.http

import java.security.{PrivateKey, PublicKey}

import akka.http.scaladsl.util.FastFuture
import com.advancedtelematic.libtuf.crypt.RsaKeyPair
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.libtuf.data.TufDataType._
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.Key
import com.advancedtelematic.libtuf.data.TufDataType.RepoId
import com.advancedtelematic.tuf.keyserver.db.KeyRepositorySupport
import com.advancedtelematic.tuf.keyserver.vault.VaultClient
import io.circe.{Encoder, Json, JsonObject}

import scala.concurrent.{ExecutionContext, Future}
import slick.jdbc.MySQLProfile.api._
import io.circe.syntax._

object RoleSigning {
  import com.advancedtelematic.libtuf.crypt.CanonicalJson._

  def isValid[T : Encoder](value: T, signature: ClientSignature, publicKey: PublicKey): Boolean = {
    val sig = Signature(signature.sig, signature.method)
    RsaKeyPair.isValid(publicKey, sig, value.asJson.canonical.getBytes)
  }
}

class RoleSigning(vaultClient: VaultClient)(implicit val db: Database, val ec: ExecutionContext)
  extends KeyRepositorySupport {

  import com.advancedtelematic.libtuf.crypt.CanonicalJson._

  def signFor[T : Encoder](repoId: RepoId, roleType: RoleType, payload: T): Future[SignedPayload[T]] = {
    val roleKeys = keyRepo.repoKeys(repoId, roleType)

    roleKeys.flatMap {
      case Nil =>
        FastFuture.failed(Errors.RoleKeysNotFound)
      case keys =>
        signAll(payload, keys)
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
      val signature = calculateSignature(payload, privateKey)
      ClientSignature(key.id, signature.method, signature.sig)
    }
  }

  private def calculateSignature[T : Encoder](payload: T, privateKey: PrivateKey): Signature = {
    val bytes = payload.asJson.canonical.getBytes
    RsaKeyPair.sign(privateKey, bytes)
  }

  private def fetchPrivateKey(key: Key): Future[PrivateKey] =
    vaultClient.findKey(key.id).flatMap { vaultKey =>
      Future.fromTry(RsaKeyPair.parseKeyPair(vaultKey.privateKey).map(_.getPrivate))
    }
}
