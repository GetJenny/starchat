package com.getjenny.starchat.utils

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 30/04/19.
  */

import java.io.{FileInputStream, InputStream}
import java.security.{KeyStore, SecureRandom}

import javax.net.ssl.{KeyManagerFactory, SSLContext, TrustManagerFactory}

object SslContext {
  def pkcs12(path: String, password: String): SSLContext = {
    val ks: KeyStore = KeyStore.getInstance("PKCS12")
    val keystore: InputStream = if (path.startsWith("/tls/certs/")) {
      getClass.getResourceAsStream(path)
    } else {
      new FileInputStream(path)
    }

    val ksPassword: Array[Char] = if (password.nonEmpty)
      password.toCharArray
    else null

    require(keystore != None.orNull, "Keystore required!")
    ks.load(keystore, ksPassword)

    val keyManagerFactory: KeyManagerFactory = KeyManagerFactory.getInstance("SunX509")
    keyManagerFactory.init(ks, ksPassword)

    val tmf: TrustManagerFactory = TrustManagerFactory.getInstance("SunX509")
    tmf.init(ks)

    val sslContext: SSLContext = SSLContext.getInstance("TLS")
    sslContext.init(keyManagerFactory.getKeyManagers, tmf.getTrustManagers, new SecureRandom)
    sslContext
  }

  def jks(path: String, password: String): SSLContext = {
    val ks: KeyStore = KeyStore.getInstance("JKS")
    val keystore: InputStream = if (path.startsWith("/tls/certs/")) {
      getClass.getResourceAsStream(path)
    } else {
      new FileInputStream(path)
    }

    val ksPassword: Array[Char] = if (password.nonEmpty)
      password.toCharArray
    else null

    require(keystore != None.orNull, "Keystore required!")
    ks.load(keystore, ksPassword)

    val keyManagerFactory: KeyManagerFactory = KeyManagerFactory.getInstance("SunX509")
    keyManagerFactory.init(ks, ksPassword)

    val tmf: TrustManagerFactory = TrustManagerFactory.getInstance("SunX509")
    tmf.init(ks)

    val sslContext: SSLContext = SSLContext.getInstance("TLS")
    sslContext.init(keyManagerFactory.getKeyManagers, tmf.getTrustManagers, new SecureRandom)
    sslContext
  }
}
