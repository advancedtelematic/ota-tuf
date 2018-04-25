package com.advancedtelematic.libtuf.data

import com.advancedtelematic.libtuf.data.ClientDataType.RootRole
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.libtuf.data.TufDataType.{KeyId, RoleType, TufKey}

object RootManipulationOps {
  implicit class RootManipulationOps(value: RootRole) {

    def roleKeys(roleType: RoleType): List[TufKey] = {
      val keyids = value.roles.get(roleType).map(_.keyids).toSet.flatten
      value.keys.filterKeys(keyids.contains).values.toList
    }

    def withVersion(version: Int): RootRole =
      value.copy(version = version)

    def addRoleKeys(roleType: RoleType, newKeys: TufKey*): RootRole = {
      val existingIds = value.roles(roleType).keyids.toSet
      val existingKeys = value.keys.filterKeys(existingIds.contains).values.toSeq
      withRoleKeys(roleType, existingKeys ++ newKeys:_*)
    }

    def removeRoleKeys(roleType: RoleType, keyIds: Set[KeyId]): RootRole = {
      val newRootKeys = value.roles(RoleType.ROOT).keyids.filterNot(keyIds.contains).toSet
      val newKeys = value.keys.filterKeys(newRootKeys.contains).values.toSeq
      withRoleKeys(roleType, newKeys:_*)
    }

    def withRoleKeys(roleType: RoleType, keys: TufKey*): RootRole = {
      val oldRoleKeys = value.roles(roleType)
      val newRoles = value.roles + (roleType -> oldRoleKeys.copy(keyids = keys.map(_.id)))

      val newKeys = newRoles.values.flatMap(_.keyids).toSet[KeyId].map { keyid =>
        value.keys.get(keyid).orElse(keys.find(_.id == keyid)).get
      }.map(k => k.id -> k).toMap

      value.copy(keys = newKeys, roles = newRoles)
    }
  }
}
