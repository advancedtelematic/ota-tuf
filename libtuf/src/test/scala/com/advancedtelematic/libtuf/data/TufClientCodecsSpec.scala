package com.advancedtelematic.libtuf.data

import org.scalatest.{FunSuite, Matchers}
import org.scalatest.concurrent.ScalaFutures
import io.circe.{parser => circeParser}
import cats.syntax.either._
import com.advancedtelematic.libtuf.data.ClientDataType.TargetsRole
import ClientCodecs._
import io.circe.syntax._
import com.advancedtelematic.libtuf.crypt.CanonicalJson._

class TufClientCodecsSpec extends FunSuite with ScalaFutures with Matchers {

  test("can decode targets.json without delegations") {
    val result = circeParser.parse(
      """
        | {
        |  "_type": "Targets",
        |  "expires": "2018-12-13T15:37:21Z",
        |  "targets": {
        |    "myfile01": {
        |      "hashes": {
        |        "sha256": "8f434346648f6b96df89dda901c5176b10a6d83961dd3c1ac88b59b2dc327aa4"
        |      },
        |      "length": 2,
        |      "custom": null
        |    }
        |  },
        |  "version": 1
        |}
      """.stripMargin)

    val targets = result.flatMap(_.as[TargetsRole]).valueOr(throw _)

    targets.delegations shouldBe empty
    targets.delegations shouldBe empty
  }


  test("can decode targets.json") {
    val result = circeParser.parse(
      """
        | {
        |  "_type": "Targets",
        |  "expires": "2018-12-13T15:37:21Z",
        |  "targets": {
        |    "myfile01": {
        |      "hashes": {
        |        "sha256": "8f434346648f6b96df89dda901c5176b10a6d83961dd3c1ac88b59b2dc327aa4"
        |      },
        |      "length": 2,
        |      "custom": null
        |    }
        |  },
        |  "version": 1,
        |  "delegations": {
        |    "keys": {},
        |    "roles": []
        |  }
        |}
      """.stripMargin)

    val targets = result.flatMap(_.as[TargetsRole]).valueOr(throw _)

    targets shouldBe a[TargetsRole]
    targets.delegations shouldNot be(empty)
  }

  test("encodes a decoded targets.json to the same canonical json") {
    val result = circeParser.parse(
      """
        | {
        |  "_type": "Targets",
        |  "expires": "2018-12-13T15:37:21Z",
        |  "targets": {
        |    "myfile01": {
        |      "hashes": {
        |        "sha256": "8f434346648f6b96df89dda901c5176b10a6d83961dd3c1ac88b59b2dc327aa4"
        |      },
        |      "length": 2,
        |      "custom": null
        |    }
        |  },
        |  "version": 1,
        |  "delegations": {
        |    "keys": {},
        |    "roles": []
        |  }
        |}
      """.stripMargin)

    val targets = result.flatMap(_.as[TargetsRole]).valueOr(throw _)

    targets.asJson.canonical shouldBe result.valueOr(throw _).canonical
  }
}
