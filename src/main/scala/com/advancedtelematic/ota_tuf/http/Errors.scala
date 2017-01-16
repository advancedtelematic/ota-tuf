package com.advancedtelematic.ota_tuf.http

import akka.http.scaladsl.model.StatusCodes
import org.genivi.sota.http.Errors.RawError
import org.genivi.sota.rest.ErrorCode

object ErrorCodes {
  val BlueprintMissing = ErrorCode("blueprint_missing")
}

object Errors {
  val BlueprintMissing = RawError(ErrorCodes.BlueprintMissing, StatusCodes.PreconditionFailed, "blueprint reference does not exist")
}
