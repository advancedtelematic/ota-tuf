package com.advancedtelematic.tuf.reposerver.db

import java.time.Instant

import com.advancedtelematic.libats.data.DataType.Checksum
import com.advancedtelematic.libtuf.data.ClientDataType.TufRole
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.libtuf.data.TufDataType.{JsonSignedPayload, RepoId}
import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType.SignedRole

protected [db] object DBDataType {
  protected [db] case class DbSignedRole(repoId: RepoId, roleType: RoleType, content: JsonSignedPayload, checksum: Checksum, length: Long, version: Int, expireAt: Instant)

  protected [db] implicit class DbSignedRoleOps(value: DbSignedRole) {
    def asSignedRole[T : TufRole]: SignedRole[T] =
      SignedRole[T](value.repoId, value.content, value.checksum, value.length, value.version, value.expireAt)
  }

  protected [db] implicit class SignedRoleAsDbSignedRoleOps[_](value: SignedRole[_]) {
    def asDbSignedRole: DbSignedRole =
      DbSignedRole(value.repoId, value.tufRole.roleType, value.content, value.checksum, value.length, value.version, value.expiresAt)
  }
}
