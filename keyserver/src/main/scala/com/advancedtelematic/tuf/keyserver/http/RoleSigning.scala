package com.advancedtelematic.tuf.keyserver.http

import java.security.{PrivateKey, PublicKey}

import akka.http.scaladsl.util.FastFuture
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, ValidatedNel}
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
import io.circe.syntax._
import cats.implicits._

class RoleSigning(vaultClient: VaultClient)(implicit val db: Database, val ec: ExecutionContext)
  extends KeyRepositorySupport {

  import com.advancedtelematic.libtuf.crypt.CanonicalJson._

  def signFor[T : Encoder](repoId: RepoId, roleType: RoleType, payload: T): Future[SignedPayload[T]] = {
    val roleKeys = keyRepo.repoKeysForRole(repoId, roleType)

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
      val signature = calculateSignature(payload, key, privateKey)
      ClientSignature(key.id, signature.method, signature.sig)
    }
  }

  // TODO: DUplication
  def signatureIsValid[T : Encoder](repoId: RepoId, signedPayload: SignedPayload[T]): Future[ValidatedNel[String, List[ClientSignature]]] = {
    val publicKeysF = keyRepo.repoKeysForRole(repoId, RoleType.ROOT)
    val sigsByKeyId = signedPayload.signatures.map(s => s.keyid -> s).toMap

    publicKeysF.map { publicKeys =>
      publicKeys.par.map { key =>
        sigsByKeyId.get(key.id) match {
          case Some(sig) =>
            if (TufCrypto.isValid(signedPayload.signed, sig, key.publicKey))
              Valid(sig)
            else
              Invalid(NonEmptyList.of(s"Invalid signature for key ${sig.keyid}"))
          case None =>
            Invalid(NonEmptyList.of(s"payload not signed with key ${key.id}"))
        }
      }.toList.sequenceU
    }
  }

  private def calculateSignature[T : Encoder](payload: T, key: Key, privateKey: PrivateKey): Signature = {
    val bytes = payload.asJson.canonical.getBytes
    TufCrypto.sign(key.keyType, privateKey, bytes)
  }

  private def fetchPrivateKey(key: Key): Future[PrivateKey] =
    vaultClient.findKey(key.id).map(_.privateKey.keyval)
}
