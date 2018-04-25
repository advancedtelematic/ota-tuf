package com.advancedtelematic.tuf.keyserver.roles

import akka.http.scaladsl.util.FastFuture
import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.libtuf.data.TufDataType.{RepoId, _}
import com.advancedtelematic.tuf.keyserver.db._
import com.advancedtelematic.tuf.keyserver.http.Errors
import io.circe.Encoder
import slick.jdbc.MySQLProfile.api._

import scala.async.Async._
import scala.concurrent.{ExecutionContext, Future}

class RoleSigning()(implicit val db: Database, val ec: ExecutionContext)
  extends SignedRootRoleSupport with KeyRepositorySupport {

  import com.advancedtelematic.libtuf.data.RootManipulationOps._

  def signWithRole[T : Encoder](repoId: RepoId, roleType: RoleType, payload: T): Future[SignedPayload[T]] = async {
    val root = await(signedRootRoleRepo.findLatest(repoId)).signed
    val keys = root.roleKeys(roleType)

    if(keys.isEmpty)
      throw Errors.RoleKeysNotFound
    else
      await(signWithKeys(payload, keys))

  }.recoverWith {
    case KeyRepository.KeyNotFound => Future.failed(Errors.PrivateKeysNotFound)
  }

  protected [roles] def signWithKeys[T : Encoder](payload: T, keys: Seq[TufKey]): Future[SignedPayload[T]] = {
    Future.sequence {
      keys.map(signForClient(payload))
    }.map { signatures =>
      SignedPayload(signatures, payload)
    }
  }

  protected [roles] def signForClient[T : Encoder](payload: T)(key: TufKey): Future[ClientSignature] = {
    fetchPrivateKey(key).map { privateKey =>
      val signature = TufCrypto.signPayload(privateKey, payload)
      ClientSignature(key.id, signature.method, signature.sig)
    }
  }

  private def fetchPrivateKey(key: TufKey): Future[TufPrivateKey] =
    keyRepo.find(key.id).recoverWith {
      case KeyRepository.KeyNotFound =>
        FastFuture.failed(Errors.PrivateKeysNotFound)
    }.map(_.privateKey.value)
}
