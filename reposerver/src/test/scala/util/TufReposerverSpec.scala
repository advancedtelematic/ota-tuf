package util

import java.security.Security
import java.util.concurrent.ConcurrentHashMap

import akka.actor.ActorSystem
import akka.http.scaladsl.testkit.RouteTestTimeout
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.scalatest.concurrent.{PatienceConfiguration, ScalaFutures}
import org.scalatest.{FunSuite, Matchers}

import scala.concurrent.duration._
import akka.testkit.TestDuration
import com.advancedtelematic.libtuf.data.TufDataType.KeyId
import org.scalatest.time.{Millis, Seconds, Span}

import scala.concurrent.Future


abstract class TufReposerverSpec extends FunSuite with Matchers with ScalaFutures {
  Security.addProvider(new BouncyCastleProvider())
}
