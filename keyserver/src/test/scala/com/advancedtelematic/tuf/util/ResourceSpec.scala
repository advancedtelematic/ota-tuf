package com.advancedtelematic.tuf.util

import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import com.advancedtelematic.tuf.keyserver.http.TufKeyserverRoutes
import akka.actor.ActorSystem
import scala.concurrent.duration._
import akka.testkit.TestDuration
import com.advancedtelematic.libats.test.DatabaseSpec

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
