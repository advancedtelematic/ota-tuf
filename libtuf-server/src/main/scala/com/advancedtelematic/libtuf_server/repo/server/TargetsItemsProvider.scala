package com.advancedtelematic.libtuf_server.repo.server

import com.advancedtelematic.libtuf.data.ClientDataType.{ClientTargetItem, MetaItem, MetaPath, TargetsRole}
import com.advancedtelematic.libtuf.data.TufDataType.{RepoId, TargetFilename}
import com.advancedtelematic.libtuf_server.repo.server.DataType.SignedRole

import scala.concurrent.Future

trait TargetsItemsProvider {
  def findSignedTargetRoleDelegations(repoId: RepoId, signedRole: SignedRole[TargetsRole]): Future[Map[MetaPath, MetaItem]]

  def findTargets(repoId: RepoId): Future[Map[TargetFilename, ClientTargetItem]]
}
