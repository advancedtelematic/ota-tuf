package com.advancedtelematic.tuf.keyserver.http

import akka.http.scaladsl.util.FastFuture
import cats.data.ValidatedNel
import cats.data.Validated.Valid
import cats.implicits._
import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.ClientDataType.RootRole
import com.advancedtelematic.libtuf.data.TufDataType._
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.libtuf.data.TufDataType.RepoId
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.{Key, Role}
import com.advancedtelematic.tuf.keyserver.db.{KeyRepositorySupport, RoleRepositorySupport}
import com.advancedtelematic.tuf.keyserver.vault.VaultClient
import io.circe.Encoder

import scala.concurrent.{ExecutionContext, Future}
import slick.jdbc.MySQLProfile.api._

class RoleSigning(vaultClient: VaultClient)(implicit val db: Database, val ec: ExecutionContext)
    extends KeyRepositorySupport
    with RoleRepositorySupport {

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

  def thresholdsIsSatisfied(oldThreshold: Int, oldRootKeyids: Set[KeyId],
                           newSignedRoot: SignedPayload[RootRole]): ValidatedNel[String, Int] = {

    def check(version: String, threshold: Int, keyIds: Set[KeyId]): ValidatedNel[String, Unit] = {
      val signaturesSatisfied = newSignedRoot.signatures.map(_.keyid).toSet.intersect(keyIds).size

      if (signaturesSatisfied >= threshold)
        Valid(())
      else
        s"Only $signaturesSatisfied signatures present from $version, need at least $threshold".invalidNel
    }

    val newCheck = newSignedRoot.signed.roles.get(RoleType.ROOT)
      .toValidNel(s"New root.json doesn't provide a root role")
      .andThen { roleKeys => check("new root", roleKeys.threshold, roleKeys.keyids.toSet).map(_ => roleKeys)
    }

    val newThresholdsAreOkay = newSignedRoot.signed.roles.map { case (role, roleKeys) =>
      if (roleKeys.threshold >= 1)
        Valid(())
      else
        s"New root.json specifies a threshold of ${roleKeys.threshold} for $role role, which is not greater than 0".invalidNel
    }.toList.sequenceU.map(_ => ())

    check("previous root", oldThreshold, oldRootKeyids)
      .combine(newThresholdsAreOkay)
      .product(newCheck).map(_._2.threshold)
  }

  def newRootIsValid(repoId: RepoId, newSignedRoot: SignedPayload[RootRole]): Future[ValidatedNel[String, Role]] =
    roleRepo.find(repoId, RoleType.ROOT).flatMap { oldRole =>
      keyRepo.repoKeysForRole(repoId, RoleType.ROOT).map { oldPublicKeys =>
        val oldThreshold = oldRole.threshold
        val oldSignedRoot = oldPublicKeys.map(k => k.id -> k.toTufKey).toMap
        val oldRootKeyids = oldPublicKeys.map(_.id).toSet

        val publicKeys = oldSignedRoot ++ newSignedRoot.signed.keys

        TufCrypto.verifySignatures(newSignedRoot, publicKeys)
          .product(thresholdsIsSatisfied(oldThreshold, oldRootKeyids, newSignedRoot))
          .map{case (_, newThreshold) => oldRole.copy(threshold = newThreshold)}
      }
    }

  private def fetchPrivateKey(key: Key): Future[TufPrivateKey] =
    vaultClient.findKey(key.id).map(_.privateKey)
}
