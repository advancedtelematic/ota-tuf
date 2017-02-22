package com.advancedtelematic.tuf.util

import java.security.{KeyPair, PrivateKey, PublicKey}
import java.time.Instant
import java.util.NoSuchElementException
import java.util.concurrent.ConcurrentHashMap

import com.advancedtelematic.libtuf.crypt.RsaKeyPair._
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import com.advancedtelematic.libtuf.data.TufDataType._
import com.advancedtelematic.tuf.keyserver.http.TufKeyserverRoutes
import io.circe.{Encoder, Json}
import com.advancedtelematic.libats.test.{DatabaseSpec}
import io.circe.syntax._
import com.advancedtelematic.libtuf.crypt.CanonicalJson._
import cats.syntax.show._
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType._

import scala.concurrent.Future
import akka.actor.ActorSystem
import akka.http.scaladsl.util.FastFuture
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.TufCodecs._

import scala.concurrent.duration._
import scala.collection.JavaConverters._
import scala.util.Try
import akka.testkit.TestDuration
import com.advancedtelematic.libtuf.crypt.RsaKeyPair
import com.advancedtelematic.libtuf.data.TufDataType.RoleType.RoleType
import com.advancedtelematic.libtuf.data.TufDataType.{KeyType, RoleType}
import com.advancedtelematic.libtuf.keyserver.KeyserverClient
import com.advancedtelematic.tuf.keyserver.data.KeyServerDataType
import com.advancedtelematic.libtuf.data.ClientDataType.{ClientKey, RoleKeys, RootRole}
import com.advancedtelematic.libtuf.data.TufDataType.RepoId


trait LongHttpRequest {
  implicit def default(implicit system: ActorSystem) =
    RouteTestTimeout(10.seconds.dilated(system))
}

trait ResourceSpec extends TufKeyserverSpec
  with ScalatestRouteTest
  with DatabaseSpec
  with LongHttpRequest {
  def apiUri(path: String): String = "/api/v1/" + path

  lazy val routes = new TufKeyserverRoutes(fakeVault).routes
}
