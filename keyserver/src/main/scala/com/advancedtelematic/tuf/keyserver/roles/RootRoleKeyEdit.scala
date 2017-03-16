package com.advancedtelematic.tuf.keyserver.roles

import java.security.PrivateKey

import cats.syntax.traverse._
import cats.implicits.catsStdInstancesForFuture
import com.advancedtelematic.libtuf.crypt.RsaKeyPair
import com.advancedtelematic.libtuf.data.ClientDataType.{ClientPrivateKey, RootRole}
import com.advancedtelematic.libtuf.data.TufDataType.{ClientSignature, KeyId, KeyType, RepoId, RoleType, SignedPayload}
import com.advancedtelematic.tuf.keyserver.db.{GeneratedRootRolesSupport, KeyRepositorySupport, RoleRepositorySupport, RootSignaturesSupport}
import com.advancedtelematic.tuf.keyserver.vault.VaultClient
import com.advancedtelematic.tuf.keyserver.vault.VaultClient.{VaultKey, VaultKeyNotFound}
import RsaKeyPair._
import akka.http.scaladsl.util.FastFuture
import cats.data.NonEmptyList
import cats.syntax.show.toShowOps

import scala.async.Async.{async, await}
import scala.concurrent.{ExecutionContext, Future}
import com.advancedtelematic.tuf.keyserver.db.KeyRepository.KeyNotFound
import com.advancedtelematic.libtuf.crypt.RsaKeyPair
import com.advancedtelematic.libtuf.crypt.RsaKeyPair._
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.{Key, TufPrivateKey}
import com.advancedtelematic.tuf.keyserver.db.Schema.{GeneratedRootRoles, RootSignature}
import com.advancedtelematic.tuf.keyserver.http.RoleSigning
import slick.driver.MySQLDriver.api._

class RootRoleKeyEdit(vaultClient: VaultClient)
                      (implicit val db: Database, val ec: ExecutionContext)
  extends KeyRepositorySupport with RootSignaturesSupport with GeneratedRootRolesSupport {
  def fetchPrivateKey(repoId: RepoId, keyId: KeyId): Future[ClientPrivateKey] = async {
    val rootPublicKey = await(ensureIsRepoRootKey(repoId, keyId))

    val privateKey = await(findParsedPrivateKey(rootPublicKey))

    ClientPrivateKey(KeyType.RSA, privateKey)
  }

  def deletePrivateKey(repoId: RepoId, keyId: KeyId): Future[ClientPrivateKey] = async {
    val rootPublicKey = await(ensureIsRepoRootKey(repoId, keyId))

    val privateKey = await(findParsedPrivateKey(rootPublicKey))

    await(vaultClient.deleteKey(rootPublicKey))

    ClientPrivateKey(KeyType.RSA, privateKey)
  }

  def addSignature(repoId: RepoId, clientPrivateKey: ClientPrivateKey): Future[Seq[RootSignature]] = {
    import RsaKeyPair.RsaPrivateKeyId
    import com.advancedtelematic.libtuf.data.TufCodecs._
    import com.advancedtelematic.libtuf.data.ClientCodecs._
    val tufKey = TufPrivateKey(clientPrivateKey.keyval.id, clientPrivateKey.keytype, clientPrivateKey.keyval) // TODO: Too many private keys

    async {
      val rootRole = await(generatedRootRoles.find(repoId)).get // TODO: Get

      val newSig = new RoleSigning().signForClient(rootRole)(tufKey) // TODO: Too many signature types
      val rootSig = RootSignature(repoId, newSig.keyid, newSig.toSignature)

      await(rootSignaturesRepo.persist(Seq(rootSig)))
    }
  }

  private def findParsedPrivateKey(keyId: KeyId): Future[PrivateKey] =
    vaultClient.findKey(keyId).flatMap { vaultKey =>
      Future.fromTry(RsaKeyPair.parseKeyPair(vaultKey.privateKey).map(_.getPrivate))
    }.recoverWith {
      case VaultKeyNotFound => Future.failed(KeyNotFound)
    }

  private def ensureIsRepoRootKey(repoId: RepoId, keyId: KeyId): Future[KeyId] = async {
    val rootPublicKey = await(keyRepo.repoKeys(repoId, RoleType.ROOT)).find(_.id == keyId)
    rootPublicKey.map(_.id).getOrElse(throw KeyNotFound)
  }
}
