package com.advancedtelematic.ota_tuf.http

import akka.http.scaladsl.model.StatusCodes
import com.advancedtelematic.util.{ResourceSpec, OtaTufSpec}
import io.circe.generic.auto._
import org.genivi.sota.marshalling.CirceMarshallingSupport._

class KeyResourceSpec extends OtaTufSpec with ResourceSpec {
  test("POST creates a key gen request") {

    Post(apiUri("key")) ~> routes ~> check {
      status shouldBe StatusCodes.Accepted
    }
  }
}
