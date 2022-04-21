package com.advancedtelematic.libtuf.data

import com.advancedtelematic.libtuf.data.ClientDataType.{RoleKeys, RootRole}
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.libtuf.data.TufDataType.{KeyId, TufKey}

import java.time.Period
import scala.util.{Failure, Success, Try}

object RootManipulationOps {
  case class RemoveRoleKeysError(msg: String) extends Exception(msg)
  case class InvalidThresholdError(msg: String) extends Exception(msg)
  val InvalidThresholdErrorMessage = "Could not set threshold. Threshold value must be at least 1 and not greater than the number of keys for the role."
  val NotEnoughKeysAfterRemovalMessage = "Could not remove keys because the number of keys after removing would have been less than threshold for the role."

  implicit class RootRoleExtension(val rootRole: RootRole) extends AnyVal {

    def roleKeys(roleTypes: RoleType*): List[TufKey] = {
      val keyids = rootRole.roles.filterKeys(roleTypes.contains).values.map(_.keyids).toSet.flatten
      rootRole.keys.filterKeys(keyids.contains).values.toList
    }

    def withVersion(version: Int): RootRole =
      rootRole.copy(version = version)

    def addExpires(period: Period): RootRole =
      rootRole.copy(expires = rootRole.expires.plus(period))

    def addRoleKeys(roleType: RoleType, newKeys: TufKey*): RootRole = {
      val existingIds = rootRole.roles(roleType).keyids.toSet
      val existingKeys = rootRole.keys.filterKeys(existingIds.contains).values.toSeq
      withRoleKeys(roleType, existingKeys ++ newKeys:_*)
    }

    def removeRoleKeys(roleType: RoleType, keyIds: Set[KeyId]): Try[(RootRole, Int)] = {
      val oldKeyIds = rootRole.roles(roleType).keyids
      val newKeyIds = oldKeyIds.filterNot(keyIds.contains).toSet
      if (newKeyIds.size >= rootRole.roles(roleType).threshold) {
        val newKeys = rootRole.keys.filterKeys(newKeyIds.contains).values.toSeq
        Success((withRoleKeys(roleType, newKeys:_*), oldKeyIds.size - newKeyIds.size))
      } else {
        Failure(RemoveRoleKeysError(NotEnoughKeysAfterRemovalMessage))
      }
    }

    def withRoleKeys(roleType: RoleType, threshold: Int, keys: TufKey*): RootRole = {
      val newRoles = rootRole.roles + (roleType -> RoleKeys(keys.map(_.id).distinct, threshold))

      val newKeys = newRoles.values.flatMap(_.keyids).toSet[KeyId].map { keyid =>
        rootRole.keys.get(keyid).orElse(keys.find(_.id == keyid)).get
      }.map(k => k.id -> k).toMap

      rootRole.copy(keys = newKeys, roles = newRoles)
    }

    def withRoleKeys(roleType: RoleType, keys: TufKey*): RootRole = {
      val oldRoleKeys = rootRole.roles(roleType)
      withRoleKeys(roleType, oldRoleKeys.threshold, keys :_*)
    }

    def withRoleThreshold(roleType: RoleType, threshold: Int): Try[RootRole] =
      if (threshold >= 1 && threshold <= rootRole.roles(roleType).keyids.size) {
        val newRoles = rootRole.roles.map {
          case (_roleType, roleKeys) if _roleType == roleType => (roleType, RoleKeys(roleKeys.keyids, threshold))
          case role => role
        }
        Success(rootRole.copy(roles = newRoles))
      } else {
        Failure(InvalidThresholdError(InvalidThresholdErrorMessage))
      }
  }
}
