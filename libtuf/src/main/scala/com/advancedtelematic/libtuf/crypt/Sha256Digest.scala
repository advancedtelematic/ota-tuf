package com.advancedtelematic.libtuf.crypt

import java.security.MessageDigest

import akka.stream.scaladsl.Sink
import akka.util.ByteString
import com.advancedtelematic.libtuf.data.TufDataType.{Checksum, HashMethod, ValidChecksum}
import org.bouncycastle.crypto.digests.SHA256Digest
import org.bouncycastle.util.encoders.Hex
import com.advancedtelematic.libats.data.RefinedUtils.RefineTry

import scala.concurrent.{ExecutionContext, Future}

object Sha256Digest {
  def digest(data: Array[Byte]): Checksum = {
    val digest = new SHA256Digest()
    val buf = Array.fill[Byte](digest.getDigestSize)(0)
    digest.update(data, 0, data.length)
    digest.doFinal(buf, 0)
    val checksum = Hex.toHexString(buf).refineTry[ValidChecksum].get

    Checksum(HashMethod.SHA256, checksum)
  }

  def asSink(implicit ec: ExecutionContext): Sink[ByteString, Future[Checksum]] = {
    Sink.fold(MessageDigest.getInstance("SHA-256")) { (d, b: ByteString) =>
      d.update(b.toArray)
      d
    }.mapMaterializedValue {
      _.map { dd =>
        val hex = Hex.toHexString(dd.digest())
        Checksum(HashMethod.SHA256, hex.refineTry[ValidChecksum].get)
      }
    }
  }
}
