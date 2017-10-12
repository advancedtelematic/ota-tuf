package com.advancedtelematic.tuf.cli

import java.security.Security
import java.time.Instant
import java.time.temporal.ChronoUnit

import com.advancedtelematic.libtuf.crypt.SignedPayloadSignatureOps._
import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.data.ClientDataType.{RoleKeys, RootRole, TargetsRole}
import com.advancedtelematic.libtuf.data.{ClientDataType, TufDataType}
import com.advancedtelematic.libtuf.data.TufDataType.{EdKeyType, KeyId, RoleType, SignedPayload, TufKey, TufPrivateKey}
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Seconds, Span}
import org.scalatest.{FunSuite, Matchers}
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.reposerver.UserReposerverClient

import scala.concurrent.Future

abstract class CliSpec extends FunSuite with Matchers with ScalaFutures {
  Security.addProvider(new BouncyCastleProvider)

  override implicit def patienceConfig = PatienceConfig().copy(timeout = Span(10, Seconds))
}

class FakeUserReposerverClient extends UserReposerverClient {
  private val (oldPublicKey, oldPrivateKey) = TufCrypto.generateKeyPair(EdKeyType, 256)

  private var (targetsPubKey, _) = TufCrypto.generateKeyPair(EdKeyType, 256)

  private var unsignedTargets = TargetsRole(Instant.now.plus(1, ChronoUnit.DAYS), Map.empty, 1)

  private var unsignedRoot = RootRole(
    Map(
    oldPublicKey.id -> oldPublicKey,
    targetsPubKey.id -> targetsPubKey),
    Map(
      RoleType.ROOT -> RoleKeys(Seq(oldPublicKey.id), 1),
      RoleType.TARGETS -> RoleKeys(Seq(targetsPubKey.id), 1)
    ), 1, Instant.now.plus(1, ChronoUnit.HOURS))

  override def root(): Future[SignedPayload[RootRole]] = Future.successful {
    val sig = TufCrypto.signPayload(oldPrivateKey, unsignedRoot).toClient(oldPublicKey.id)
    SignedPayload(Seq(sig), unsignedRoot)
  }

  override def deleteKey(keyId: KeyId): Future[TufPrivateKey] = {
    if(keyId == oldPublicKey.id)
      Future.successful(oldPrivateKey)
    else
      Future.failed(new RuntimeException(s"[test] key not found $keyId"))
  }

  override def pushSignedRoot(signedRoot: TufDataType.SignedPayload[ClientDataType.RootRole]) = {
    if(signedRoot.isValidFor(oldPublicKey)) {
      unsignedRoot = signedRoot.signed
      Future.successful(())
    } else
      Future.failed(new RuntimeException("[test] invalid signatures for root role"))
  }

  override def targets(): Future[SignedPayload[TargetsRole]] =
    throw new NotImplementedError("[test] targets not implemented for fake repo server")

  def pushTargets(targetsRole: SignedPayload[TargetsRole]): Future[Unit] = {
    val targetsPubKey = unsignedRoot.keys(unsignedRoot.roles(RoleType.TARGETS).keyids.head)

    if(targetsRole.isValidFor(targetsPubKey)) {
      unsignedTargets = targetsRole.signed
      Future.successful(())
    } else
      Future.failed(new RuntimeException("[test] invalid signatures for targets role"))
  }

  override def pushTargetsKey(key: TufKey): Future[TufKey] = Future.successful {
    targetsPubKey = key
    targetsPubKey
  }
}
