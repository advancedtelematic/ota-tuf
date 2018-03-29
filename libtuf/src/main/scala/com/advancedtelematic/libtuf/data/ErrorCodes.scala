package com.advancedtelematic.libtuf.data

import com.advancedtelematic.libats.data.ErrorCode

object ErrorCodes {
  object KeyServer {
    val KeysNotReady = ErrorCode("keys_not_ready")
    val RoleKeysNotFound = ErrorCode("role_keys_not_found")
    val RepoRootKeysNotFound = ErrorCode("repo_root_keys_not_found")
    val PrivateKeysNotFound = ErrorCode("private_keys_not_found")
    val KeyGenerationFailed = ErrorCode("key_generation_failed")
    val InvalidRootRole = ErrorCode("invalid_root_role")
  }
}
