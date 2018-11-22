package com.advancedtelematic.tuf.cli

import java.nio.file.{Files, Paths}

import com.advancedtelematic.libtuf.data.ClientDataType.TargetsRole
import com.advancedtelematic.tuf.cli.util.CliSpec
import io.circe.{Json, jawn}
import cats.syntax.either._
import com.advancedtelematic.libtuf.data.TufCodecs._
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.TufDataType.{KeyType, SignedPayload}

class DelegationsSpec extends CliSpec {

  val subject = Delegations


  test("generates an empty delegations role to a given Path") {
    val p = Files.createTempFile("delegations", ".json")

    subject.writeNew(p).get

    val emptyDelegations = jawn.parseFile(p.toFile).flatMap(_.as[TargetsRole]).valueOr(throw _)

    emptyDelegations.delegations shouldBe empty
  }

  test("signs a payload with the supplied keys") {
    val pair = KeyType.default.crypto.generateKeyPair()

    val in = Files.createTempFile("payload", ".json")
    Files.write(in, "{}".getBytes)

    val out = Files.createTempFile("payload-out", ".json")

    subject.signPayload(List(pair.pubkey -> pair.privkey), in, out).get

    val signedPayload = jawn.parseFile(out.toFile).flatMap(_.as[SignedPayload[Json]]).valueOr(throw _)

    signedPayload.signatures shouldNot be(empty)

    signedPayload.signed shouldBe Json.obj()
  }
}
