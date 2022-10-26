package com.advancedtelematic.tuf.cli

import java.time.{Instant, Period}

import com.advancedtelematic.tuf.cli.Errors.PastDate
import org.scalatest.{FunSuite, Matchers}

class CommandHandlerExpirationDateSpec extends FunSuite with Matchers {
  val now = Instant.EPOCH
  val oneDay = Period.ofDays(1)
  val twoDays = Period.ofDays(2)
  val inADay = now.plus(oneDay)
  val inTwoDays = now.plus(twoDays)
  val aDayAgo = now.minus(oneDay)

  // the actual command doesn't matter
  private val cmd = Commands.SignRoot

  test("expirationDate from previous value is used when previous value larger that default value") {
    val d = CommandHandler.expirationDate(Config(cmd), now, oneDay)(inTwoDays)
    d shouldBe inTwoDays
  }

  test("expirationDate from default value is used when previous value smaller that default value") {
    val d = CommandHandler.expirationDate(Config(cmd), now, twoDays)(inADay)
    d shouldBe inTwoDays
  }

  test("expirationDate from default value is used if previous value in the past") {
    val d = CommandHandler.expirationDate(Config(cmd), now, oneDay)(aDayAgo)
    d shouldBe inADay
  }

  test("expirationDate from expireOn") {
    val d = CommandHandler.expirationDate(Config(cmd, expireOn = Some(inADay)), now, twoDays)(aDayAgo)
    d shouldBe inADay
  }

  test("expirationDate from expireOn in the past") {
    intercept[PastDate] {
      CommandHandler.expirationDate(Config(cmd, expireOn = Some(now.minusSeconds(1))), now, twoDays)(aDayAgo)
    }
  }

  test("expirationDate from expireOn in the past, but forced") {
    val d = CommandHandler.expirationDate(Config(cmd, force = true, expireOn = Some(now.minusSeconds(1))), now, twoDays)(aDayAgo)
    d shouldBe now.minusSeconds(1)
  }

  test("expirationDate from expireAfter") {
    val d = CommandHandler.expirationDate(Config(cmd, expireAfter = Some(oneDay)), now, twoDays)(aDayAgo)
    d shouldBe inADay
  }

  test("expirationDate from expireAfter in the past") {
    intercept[PastDate] {
      CommandHandler.expirationDate(Config(cmd, expireAfter = Some(Period.ofDays(-1))), now, twoDays)(aDayAgo)
    }
  }

  test("expirationDate from expireAfter in the past, but forced") {
    val d = CommandHandler.expirationDate(Config(cmd, force = true, expireAfter = Some(Period.ofDays(-1))), now, twoDays)(aDayAgo)
    d shouldBe now.minus(oneDay)
  }

}
