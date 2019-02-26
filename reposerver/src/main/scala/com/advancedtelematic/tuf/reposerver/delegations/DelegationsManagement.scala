package com.advancedtelematic.tuf.reposerver.delegations

import java.time.Instant

import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNel
import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.ClientDataType.{DelegatedRoleName, Delegation, MetaItem, MetaPath, TargetsRole, ValidMetaPath}
import com.advancedtelematic.libtuf.data.TufDataType.{JsonSignedPayload, RepoId, SignedPayload}
import com.advancedtelematic.libtuf_server.keyserver.KeyserverClient
import com.advancedtelematic.tuf.reposerver.data.RepositoryDataType.{SignedRole, asMetaItem}
import com.advancedtelematic.tuf.reposerver.db.{DelegationRepositorySupport, SignedRoleRepositorySupport}
import com.advancedtelematic.tuf.reposerver.http.{Errors, RepoRoleRefresh, RoleSigner}
import eu.timepit.refined.api.Refined
import com.advancedtelematic.libats.data.RefinedUtils._
import slick.jdbc.MySQLProfile.api._

import scala.async.Async._
import scala.concurrent.{ExecutionContext, Future}

class DelegationsManagement()(implicit val db: Database, val ec: ExecutionContext)
                                                  extends DelegationRepositorySupport with SignedRoleRepositorySupport {

  def create(repoId: RepoId, roleName: DelegatedRoleName, delegationMetadata: SignedPayload[TargetsRole]): Future[Unit] = async {
    val targetsRole = await(signedRoleRepository.find[TargetsRole](repoId)).role
    val delegation = findDelegationMetadataByName(targetsRole, roleName)

    validateDelegationMetadataSignatures(targetsRole, delegation, delegationMetadata) match {
      case Valid(_) =>
        await(delegationsRepo.persist(repoId, roleName, delegationMetadata.asJsonSignedPayload))
      case Invalid(err) =>
        throw Errors.PayloadSignatureInvalid(err)
    }
  }

  def find(repoId: RepoId, roleName: DelegatedRoleName): Future[JsonSignedPayload] =
    delegationsRepo.find(repoId, roleName).map(_.content)

  private def findDelegationMetadataByName(targetsRole: TargetsRole, delegatedRoleName: DelegatedRoleName): Delegation = {
    targetsRole.delegations.flatMap(_.roles.find(_.name == delegatedRoleName)).getOrElse(throw Errors.DelegationNotDefined)
  }

  def findDelegations(targets: SignedRole[TargetsRole]): Future[Map[MetaPath, MetaItem]] = {
    val delegatedRoleNames = targets.role.delegations.map(_.roles.map(_.name)).getOrElse(Nil)
    val delegations = Future.sequence(delegatedRoleNames.map { name => find(targets.repoId, name).map((name, _)) })
      .recover { case Errors.DelegationNotFound => Nil }

    delegations.map(_.map { case (name: DelegatedRoleName, d: JsonSignedPayload) =>
      (name.value + ".json").refineTry[ValidMetaPath].get -> asMetaItem(d)
    }.toMap)
  }

  private def validateDelegationMetadataSignatures(targetsRole: TargetsRole,
                                                   delegation: Delegation,
                                                   delegationMetadata: SignedPayload[TargetsRole]): ValidatedNel[String, SignedPayload[TargetsRole]] = {
    val publicKeys = targetsRole.delegations.map(_.keys).getOrElse(Map.empty).filterKeys(delegation.keyids.contains)
    TufCrypto.payloadSignatureIsValid(publicKeys, delegation.threshold, delegationMetadata)
  }
}
