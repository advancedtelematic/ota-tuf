package com.advancedtelematic.tuf.crypt

import cats.syntax.either._
import com.advancedtelematic.libtuf.crypt.CanonicalJson._
import io.circe.parser.decode
import io.circe.Json
import io.circe.syntax._
import org.scalatest.{FunSuite, Matchers}

class CanonicalJsonSpec extends FunSuite with Matchers {

  test("Should encode JSON as CJSON correctly") {
    val json = """{
                 |  "aa": "newline\nhere",
                 |  "b": "foo\\bar",
                 |  "a": "first"
                 |}
               """.stripMargin

    val out = """{"a":"first","aa":"newline
                |here","b":"foo\\bar"}""".stripMargin

    val parsed = decode[Json](json).valueOr(throw _)
    parsed.canonical shouldBe out
  }
}
