package com.advancedtelematic.ota_tuf.http

import akka.http.scaladsl.model.StatusCodes
import akka.stream.Materializer
import slick.driver.MySQLDriver.api._

import scala.concurrent.ExecutionContext
import com.advancedtelematic.ota_tuf.data.DataType._
import com.advancedtelematic.ota_tuf.data.KeyGenRequestStatus
import com.advancedtelematic.ota_tuf.db.KeyGenRequestSupport
import org.genivi.sota.marshalling.CirceMarshallingSupport._
import io.circe.generic.auto._

class KeyResource()
                 (implicit db: Database, ec: ExecutionContext, mat: Materializer) extends KeyGenRequestSupport {

  import akka.http.scaladsl.server.Directives._

  val route =
    path("key") {
      post {
        val keyGenReq = KeyGenRequest(KeyId.generate(), KeyGenRequestStatus.REQUESTED)
        val f = keyGenRepo.persist(keyGenReq).map(_ => StatusCodes.Accepted)
        complete(f)
      }
    }
}
