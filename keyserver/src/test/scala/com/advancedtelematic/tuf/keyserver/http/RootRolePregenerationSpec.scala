package com.advancedtelematic.tuf.keyserver.http

import com.advancedtelematic.tuf.util.TufKeyserverSpec

class RootRolePregenerationSpec extends TufKeyserverSpec {

  test("caches signed payload when finding") {

    fail("not implemented")

//    val repoId = RepoId.generate()
//    val rootKeyGenRequest = KeyGenRequest(KeyGenId.generate(),
//      repoId, KeyGenRequestStatus.GENERATED, RoleType.ROOT, keySize = 1024)
//
//    async {
//      await(keyGenRepo.persist(rootKeyGenRequest))
//
//      await(keyGenerationOp.processGenerationRequest(rootKeyGenRequest))
//
//      val signed = await(rootGeneration.findAndPersistGenerated(repoId))
//
//      await(signedRootRolesRepo.find(repoId)) shouldBe signed
//    }
  }
}
