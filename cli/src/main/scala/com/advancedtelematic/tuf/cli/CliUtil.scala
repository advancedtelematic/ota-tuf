package com.advancedtelematic.tuf.cli

import java.nio.file.Path

import cats.data.Validated.{Invalid, Valid}
import com.advancedtelematic.libtuf.crypt.TufCrypto
import com.advancedtelematic.libtuf.data.ClientDataType.RootRole
import com.advancedtelematic.libtuf.data.TufDataType.SignedPayload
import org.slf4j.LoggerFactory

import scala.concurrent.Future
import scala.util.control.NoStackTrace
import cats.syntax.either._
import io.circe.jawn.parseFile
import com.advancedtelematic.libtuf.data.ClientCodecs._
import com.advancedtelematic.libtuf.data.TufCodecs._

object CliUtil {

  private val _log = LoggerFactory.getLogger(this.getClass)

  case class InvalidPayload(msg: String) extends Throwable(msg) with NoStackTrace

  def verifyRootFile(rootPath: Path): Future[SignedPayload[RootRole]] = {
    val validatedPayload =
      parseFile(rootPath.toFile)
        .flatMap(_.as[SignedPayload[RootRole]])
        .leftMap(_.toString).toValidatedNel

    val isValidPayload = validatedPayload.andThen { parsedPayload =>
      TufCrypto.payloadSignatureIsValid(parsedPayload.signed, parsedPayload)
    }

    isValidPayload match {
      case Valid(rr) =>
        _log.info(s"Signatures for $rootPath are valid")
        Future.successful(rr)

      case Invalid(err) =>
        err.map(_log.error)
        Future.failed(InvalidPayload("Invalid root.json file"))
    }
  }
}
