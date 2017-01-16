package com.advancedtelematic.util

import java.security.Security

import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FunSuite, Matchers}

abstract class OtaTufSpec extends FunSuite with Matchers with ScalaFutures {
  Security.addProvider(new BouncyCastleProvider())
}
