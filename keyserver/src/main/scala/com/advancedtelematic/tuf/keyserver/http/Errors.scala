package com.advancedtelematic.tuf.keyserver.http

import akka.http.scaladsl.model.StatusCodes
import com.advancedtelematic.libats.data.ErrorCode
import com.advancedtelematic.libats.http.Errors.{JsonError, RawError}
import com.advancedtelematic.libtuf.data.TufDataType.RepoId
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType.KeyGenId
import io.circe.syntax._

object ErrorCodes {
  val KeysNotReady = ErrorCode("keys_not_ready")
  val RoleKeysNotFound = ErrorCode("role_keys_not_found")
  val PrivateKeysNotFound = ErrorCode("private_key_not_found")
  val KeyGenerationFailed = ErrorCode("key_generation_failed")
}

object Errors {
  val KeysNotReady = RawError(ErrorCodes.KeysNotReady, StatusCodes.Locked, "A key generation request exists")
  val RoleKeysNotFound = RawError(ErrorCodes.RoleKeysNotFound, StatusCodes.NotFound, "There are no keys for this repoid/roletype")
  val PrivateKeysNotFound = RawError(ErrorCodes.PrivateKeysNotFound, StatusCodes.PreconditionFailed, "There are no private key for that role")

  def KeyGenerationFailed(repoId: RepoId, errors: Map[KeyGenId, String]) =
    JsonError(ErrorCodes.KeyGenerationFailed, StatusCodes.InternalServerError, errors.asJson, "Could not generate keys")
}
