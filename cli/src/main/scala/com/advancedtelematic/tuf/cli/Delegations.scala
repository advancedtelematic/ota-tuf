package com.advancedtelematic.tuf.cli

import java.nio.file.{Files, Path}
import java.time.{Instant, Period}

import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.ClientDataType
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.TufDataType.{SignedPayload, TufKey, TufPrivateKey}
import com.advancedtelematic.tuf.cli.DataType.KeyName
import com.advancedtelematic.tuf.cli.repo.CliKeyStorage
import io.circe.syntax._
import org.slf4j.LoggerFactory

import scala.util.Try

// TODO:SM How do I test this shit?
object Delegations {

  private val log = LoggerFactory.getLogger(this.getClass)

  private val defaultExpirePeriod = Period.ofDays(365)

  def writeNew(output: Path): Try[Unit] = Try {
    val empty = ClientDataType.TargetsRole(Instant.now().plus(defaultExpirePeriod), Map.empty, version = 1)
    Files.write(output, empty.asJson.spaces2.getBytes)
  }

  def signPayload(keys: List[(TufKey, TufPrivateKey)], input : Path, output: Path): Try[Unit] = {
    io.circe.jawn.parseFile(input.toFile).map { inJson =>

      val sigs = keys.map { case (pub, priv) =>
        TufCrypto.signPayload(priv, inJson).toClient(pub.id)
      }

      val signedPayload = SignedPayload(sigs, inJson, inJson)

      Files.write(output, signedPayload.asJson.spaces2.getBytes)
    }.toTry.map(_ => ())
  }


//  def exportKey(keyName: KeyName): Future[Unit] = {
//    val cliKeyStorage = new CliKeyStorage(Paths.get("./")) // TODO:SM OI?
//
//    val keyPairT = cliKeyStorage.readKeyPair(keyName)
//
//    keyPairT.map { case (pubKey, privKey) =>
//      Map(pubKey.id -> Map("private" -> privKey.asJson, "public" -> pubKey.asJson).asJson).asJson
//    }.map { j =>
//      log.info(j.spaces2)
//    }.toFuture
//  }


//  def sign(keyNames: List[KeyName], input: Path, output: Path): Future[Unit] = {
//    // TODO:SM Must have two different commands? One for delegations, one for repo?
//    // TODO:SM What about key storage? But we don't have a repo
//    // TODO:SM Maybe create a new staging area for the user? Without a credentials.zip
//
//    val cliKeyStorage = new CliKeyStorage(Paths.get("./")) // TODO:SM OI?
//
//    val keysT: Try[List[(TufKey, TufPrivateKey)]] = keyNames.map { k =>
//      cliKeyStorage.readKeyPair(k)
//    }.sequence
//
//
//    // TODO: This is wrong, just overwrites old output with new ouptu
//    keysT.map { keys =>
//      Delegations.signPayload(keys, input, output).toFuture
//    }
//
//    ???
//  }
}
