package com.advancedtelematic.tuf.cli.http

import java.io.FileInputStream
import java.nio.file.Path
import java.security.{KeyStore, SecureRandom}

import javax.net.ssl.{KeyManagerFactory, SSLContext, TrustManagerFactory}
import scalaj.http.{HttpOptions, HttpRequest}

object Auth {
  val none = identity[HttpRequest] _

  def oauth(token: String): HttpRequest => HttpRequest = req => {
    req.header("Authorization", s"Bearer $token")
  }

  lazy val keyManagerFactory = KeyManagerFactory.getInstance("SunX509")
  lazy val trustFactory = TrustManagerFactory.getInstance("SunX509")

  def mutualTls(tlsCertPath: Path, serverCertPath: Option[Path]): HttpRequest => HttpRequest = {
    val keyInput = new FileInputStream(tlsCertPath.toFile)
    val keyStore = KeyStore.getInstance("PKCS12", "BC")
    keyStore.load(keyInput, "".toCharArray)
    keyInput.close()
    keyManagerFactory.init(keyStore, "".toCharArray)

    val trustManagers = serverCertPath.map { serverP12 =>
      val trustInput = new FileInputStream(serverP12.toFile)
      val trustKeyStore = KeyStore.getInstance("PKCS12", "BC")
      trustKeyStore.load(trustInput, "".toCharArray)
      trustInput.close()

      trustFactory.init(trustKeyStore)
      trustFactory.getTrustManagers
    }

    val context = SSLContext.getInstance("TLS")
    context.init(keyManagerFactory.getKeyManagers, trustManagers.orNull, new SecureRandom())

    val opts = HttpOptions.sslSocketFactory(context.getSocketFactory)

    req => {
      req.options(opts)
    }
  }
}
