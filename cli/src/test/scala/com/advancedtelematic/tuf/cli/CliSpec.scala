package com.advancedtelematic.tuf.cli

import TryToFuture.TryToFutureOp
import java.security.Security
import java.time.Instant
import java.time.temporal.ChronoUnit

import com.advancedtelematic.libats.data.DataType.ValidChecksum
import com.advancedtelematic.libtuf.crypt.SignedPayloadSignatureOps._
import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.data.ClientDataType.{RoleKeys, RootRole, TargetsRole}
import com.advancedtelematic.libtuf.data.{ClientDataType, TufDataType}
import com.advancedtelematic.libtuf.data.TufDataType.{KeyId, KeyType, RoleType, SignedPayload, TufKey, TufKeyPair, TufPrivateKey}
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Seconds, Span}
import org.scalatest.{FunSuite, Matchers}
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.reposerver.UserReposerverClient
import com.advancedtelematic.libtuf.reposerver.UserReposerverClient.TargetsResponse
import eu.timepit.refined.api.Refined

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

abstract class CliSpec extends FunSuite with Matchers with ScalaFutures {
  Security.addProvider(new BouncyCastleProvider)

  override implicit def patienceConfig = PatienceConfig().copy(timeout = Span(10, Seconds))
}

class FakeUserReposerverClient(keyType: KeyType) extends UserReposerverClient {
  import scala.concurrent.ExecutionContext.Implicits.global

  private val oldPair = keyType.crypto.generateKeyPair(keyType.crypto.defaultKeySize)

  private val targetsPair = keyType.crypto.generateKeyPair(keyType.crypto.defaultKeySize)
  private var targetsPubKey = targetsPair.pubkey

  private var unsignedTargets = TargetsRole(Instant.now.plus(1, ChronoUnit.DAYS), Map.empty, 1)

  private var unsignedRoot = RootRole(
    Map(
    oldPair.pubkey.id -> oldPair.pubkey,
    targetsPubKey.id -> targetsPubKey),
    Map(
      RoleType.ROOT -> RoleKeys(Seq(oldPair.pubkey.id), 1),
      RoleType.TARGETS -> RoleKeys(Seq(targetsPubKey.id), 1)
    ), 1, Instant.now.plus(1, ChronoUnit.HOURS))

  override def root(): Future[SignedPayload[RootRole]] = Future.successful {
    val sig = TufCrypto.signPayload(oldPair.privkey, unsignedRoot).toClient(oldPair.pubkey.id)
    SignedPayload(Seq(sig), unsignedRoot)
  }

  override def deleteKey(keyId: KeyId): Future[Unit] = {
    if(keyId == oldPair.pubkey.id)
      Future.successful(())
    else
      Future.failed(new RuntimeException(s"[test] key not found $keyId"))
  }

  override def pushSignedRoot(signedRoot: TufDataType.SignedPayload[ClientDataType.RootRole]) =
    if (signedRoot.isValidFor(oldPair.pubkey)) {
      unsignedRoot = signedRoot.signed
      Future.successful(())
    } else
      Future.failed(new RuntimeException("[test] invalid signatures for root role"))

  override def targets(): Future[TargetsResponse] = Future.successful {
    val sig = TufCrypto.signPayload(targetsPair.privkey, unsignedTargets).toClient(targetsPubKey.id)
    val signedPayload = SignedPayload(Seq(sig), unsignedTargets)
    TargetsResponse(signedPayload, Option(Refined.unsafeApply("095c33175e5af42691c1b41d388c8ce842c7fbb792fcc6514b5436f7f80420db")))
  }

  def pushTargets(targetsRole: SignedPayload[TargetsRole], checksum: Option[Refined[String, ValidChecksum]]): Future[Unit] =
    Try(checksum.get).flatMap { _ =>
      val targetsPubKey = unsignedRoot.keys(unsignedRoot.roles(RoleType.TARGETS).keyids.head)

      if (targetsRole.isValidFor(targetsPubKey)) {
        unsignedTargets = targetsRole.signed
        Success(())
      } else {
        Failure(new RuntimeException("[test] invalid signatures for targets role"))
      }
    }.toFuture

  override def pushTargetsKey(key: TufKey): Future[TufKey] = Future.successful {
    targetsPubKey = key
    targetsPubKey
  }

  override def fetchKeyPair(keyId: KeyId): Future[TufKeyPair] =
    Future.successful(keyType.crypto.castToKeyPair(oldPair.pubkey, oldPair.privkey).get).filter(_.pubkey.id == keyId)
}
