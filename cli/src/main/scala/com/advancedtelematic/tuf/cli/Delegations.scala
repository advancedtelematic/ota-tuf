package com.advancedtelematic.tuf.cli

import java.io.OutputStream
import java.nio.file.Path
import java.time.{Instant, Period}

import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.ClientDataType
import com.advancedtelematic.libtuf.data.ClientDataType.{ClientTargetItem, DelegatedRoleName, TargetsRole}
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.TufDataType.{SignedPayload, TargetFilename, TufKey, TufPrivateKey}
import com.advancedtelematic.libtuf.http.ReposerverClient
import com.advancedtelematic.tuf.cli.TryToFuture._
import io.circe.{Json, jawn}
import io.circe.syntax._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object Delegations {
  private val defaultExpirePeriod = Period.ofDays(365)

  def empty: TargetsRole = ClientDataType.TargetsRole(Instant.now().plus(defaultExpirePeriod), Map.empty, version = 1)

  def writeNew(output: OutputStream): Try[Unit] = Try {
    output.write(empty.asJson.spaces2.getBytes)
  }

  private def read(input: Path): Try[Json] = {
    io.circe.jawn.parseFile(input.toFile).toTry
  }

  def signPayload(keys: List[(TufKey, TufPrivateKey)], input : Path, output: OutputStream): Try[Unit] = {
    read(input).map { inJson =>
      val sigs = keys.map { case (pub, priv) =>
        TufCrypto.signPayload(priv, inJson).toClient(pub.id)
      }

      val signedPayload = SignedPayload(sigs, inJson, inJson)

      output.write(signedPayload.asJson.spaces2.getBytes)
    }.map(_ => ())
  }

  def push(reposerverClient: ReposerverClient, delegationName: DelegatedRoleName, delegationPath: Path)(implicit ec: ExecutionContext): Future[Unit] = {
    val delegationT = jawn.parseFile(delegationPath.toFile).flatMap(_.as[SignedPayload[TargetsRole]]).toTry.toFuture

    delegationT.flatMap { delegation =>
      reposerverClient.pushDelegation(delegationName, delegation)
    }
  }

  def pull(reposerverClient: ReposerverClient, delegationName: DelegatedRoleName, outputStream: OutputStream)
          (implicit ec: ExecutionContext): Future[SignedPayload[TargetsRole]] = {
    reposerverClient.pullDelegation(delegationName).map { delegation =>
      outputStream.write(delegation.asJson.spaces2.getBytes)
      delegation
    }
  }

  def addTarget(input: Path, output: OutputStream, targetFilename: TargetFilename, targetItem: ClientTargetItem): Try[Unit] = {
    read(input).flatMap { json =>
      json.as[TargetsRole].toTry.map(json -> _)
    }.map { case (rawJson, targets) =>
      val newTargets = targets.copy(targets = targets.targets + (targetFilename -> targetItem))
      val newJson = rawJson.deepMerge(newTargets.asJson)
      output.write(newJson.spaces2.getBytes)
    }
  }
}
