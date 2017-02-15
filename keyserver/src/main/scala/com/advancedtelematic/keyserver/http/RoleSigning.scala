package com.advancedtelematic.keyserver.http

import java.security.{PrivateKey, PublicKey}

import akka.http.scaladsl.util.FastFuture
import com.advancedtelematic.libtuf.crypt.RsaKeyPair
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.libtuf.data.TufDataType._
import com.advancedtelematic.keyserver.data.KeyServerDataType.Key
import com.advancedtelematic.libtuf.data.TufDataType.RepoId
import com.advancedtelematic.keyserver.db.KeyRepositorySupport
import com.advancedtelematic.keyserver.vault.VaultClient
import io.circe.{Encoder, Json, JsonObject}

import scala.concurrent.{ExecutionContext, Future}
import slick.driver.MySQLDriver.api._
import io.circe.syntax._

object RoleSigning {
  import CanonicalJson._

  def isValid[T : Encoder](value: T, signature: ClientSignature, publicKey: PublicKey): Boolean = {
    val sig = Signature(signature.sig, signature.method)
    RsaKeyPair.isValid(publicKey, sig, value.asJson.canonical.getBytes)
  }
}

class RoleSigning(vaultClient: VaultClient)(implicit val db: Database, val ec: ExecutionContext)
  extends KeyRepositorySupport {

  import CanonicalJson._

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
      ClientSignature(key.id, signature.method, signature.hex)
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


object CanonicalJson {

  implicit class ToCanonicalJsonOps(value: Json) {
    def canonical: String = generate(value).noSpaces
  }

  private def generate(value: Json): Json =
    value.arrayOrObject[Json](
      value,
      array => Json.fromValues(array.map(generate)),
      obj =>
        JsonObject.fromIterable {
          obj
            .toList
            .map { case (k, v) =>
              k -> generate(v)
            }.sortBy(_._1)
        }.asJson
    )
}
