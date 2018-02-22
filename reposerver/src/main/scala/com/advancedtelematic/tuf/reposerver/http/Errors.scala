package com.advancedtelematic.tuf.reposerver.http

import akka.http.scaladsl.model.StatusCodes
import com.advancedtelematic.libats.data.DataType.Namespace
import com.advancedtelematic.libats.data.ErrorCode
import com.advancedtelematic.libats.http.Errors.RawError


object ErrorCodes {
  val RoleKeysNotFound = ErrorCode("role_keys_not_found")
  val TargetNotFound = ErrorCode("target_not_found")
  val RoleChecksumNotProvided = ErrorCode("role_checksum_not_provided")
  val RoleChecksumMismatch = ErrorCode("role_checksum_mismatch")
  val NoRepoForNamespace = ErrorCode("no_repo_for_namespace")
  val NoUriForUnmanagedTarget = ErrorCode("no_uri_for_unmanaged_target")
  val TooManyReposForNamespace = ErrorCode("too_many_repos_for_namespace")
}

object Errors {
  val RoleKeysNotFound = RawError(ErrorCodes.RoleKeysNotFound, StatusCodes.NotFound, "There are no keys for this repoid/roletype")
  val TargetNotFoundError = RawError(ErrorCodes.TargetNotFound, StatusCodes.NotFound, "TargetNotFound")
  val NoUriForUnamanagedTarget = RawError(ErrorCodes.NoUriForUnmanagedTarget, StatusCodes.ExpectationFailed, "Cannot redirect to unmanaged resource, no known URI for this resource")
  val RoleChecksumNotProvided = RawError(ErrorCodes.RoleChecksumNotProvided, StatusCodes.PreconditionRequired, "A targets role already exists, but no previous checksum was sent")
  val RoleChecksumMismatch = RawError(ErrorCodes.RoleChecksumMismatch, StatusCodes.PreconditionFailed, "Provided checksum of previous role does not match current checksum")
  val TooManyReposForNamespace = RawError(ErrorCodes.TooManyReposForNamespace, StatusCodes.BadRequest, "Too many repos found for this namespace. Use the /repo/:repo_id API instead")

  case class NoRepoForNamespace(ns: Namespace)
    extends com.advancedtelematic.libats.http.Errors.Error(ErrorCodes.NoRepoForNamespace, StatusCodes.NotFound, s"No repository exists for namespace ${ns.get}")
}
