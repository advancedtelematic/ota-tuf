package com.advancedtelematic.tuf.reposerver.batch

import com.advancedtelematic.libats.http.BootApp
import com.advancedtelematic.libats.slick.db.DatabaseConfig
import com.advancedtelematic.libtuf.keyserver.KeyserverHttpClient
import com.advancedtelematic.tuf.reposerver.http.SignedRoleGeneration
import com.advancedtelematic.tuf.reposerver.{Settings, VersionInfo}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object RefreshTimestamps extends BootApp with Settings with VersionInfo with DatabaseConfig {
  implicit val _db = db

  val keyStoreClient = KeyserverHttpClient(keyServerUri)
  val generator = new SignedRoleGeneration(keyStoreClient)

  Await.result(generator.refreshTimestampRoles(), Duration.Inf)
}
