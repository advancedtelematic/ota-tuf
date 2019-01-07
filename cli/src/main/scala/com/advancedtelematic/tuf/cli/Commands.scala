package com.advancedtelematic.tuf.cli

object Commands {
  sealed trait Command
  case object Help extends Command
  case object GenRepoKeys extends Command
  case object InitRepo extends Command
  case object MoveOffline extends Command
  case object GetTargets extends Command
  case object InitTargets extends Command
  case object AddTarget extends Command
  case object DeleteTarget extends Command
  case object SignTargets extends Command
  case object SignRoot extends Command
  case object PullTargets extends Command
  case object PushTargets extends Command
  case object PullRoot extends Command
  case object PushRoot extends Command
  case object AddRootKey extends Command
  case object RemoveRootKey extends Command
  case object ExportRepository extends Command
  case object VerifyRoot extends Command
  case object CreateDelegation extends Command
  case object SignDelegation extends Command
  case object GenUserKey extends Command
  case object IdUserKey extends Command
  case object PushDelegation extends Command
  case object PullDelegation extends Command
  case object AddTargetDelegation extends Command
}

