package com.advancedtelematic.libtuf_server.repo.server

import cats.Show
import com.advancedtelematic.libats.http.Errors.{JsonError, MissingEntity, MissingEntityId, RemoteServiceError}
import com.advancedtelematic.libtuf.data.ClientDataType.TufRole
import com.advancedtelematic.libtuf.data.TufDataType.RepoId
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import io.circe.Json

object Errors {
  implicit val showRepoIdError: Show[(RepoId, RoleType)] = Show.show { case (repoId: RepoId, roleType: RoleType) =>
    s"$repoId/$roleType"
  }

  def SignedRoleNotFound(repoId: RepoId, roleType: RoleType) = MissingEntityId[(RepoId, RoleType)](repoId -> roleType)

  def InvalidOfflineRoot(remoteServiceError: RemoteServiceError) =
    JsonError(remoteServiceError.causeCode, remoteServiceError.status, remoteServiceError.cause.flatMap(_.cause).getOrElse(Json.Null), remoteServiceError.cause.fold("")(_.description))

}
