package com.getjenny.starchat.services.esclient

import com.getjenny.starchat.utils.SslContext
import com.typesafe.config.{Config, ConfigFactory}
import org.apache.http.HttpHost
import org.apache.http.impl.nio.client.HttpAsyncClientBuilder
import org.apache.http.message.BasicHeader
import org.elasticsearch.client.{RestClient, RestClientBuilder, RestHighLevelClient}
import scalaz.Scalaz._

import java.net.InetAddress
import javax.net.ssl.{SSLContext, SSLSession}
import scala.collection.immutable.List

object ElasticsearchClientSingleton {

  private val config: Config = ConfigFactory.load()
  private val elasticsearchAuthentication: String = config.getString("es.authentication")
  private val hostProto: String = config.getString("es.host_proto")
  val esHttpClient: RestHighLevelClient = openHttp()

  private def openHttp(): RestHighLevelClient = new RestHighLevelClient({
    val addresses = inetAddresses()
    val clientBuilder = if (hostProto === "http") {
      RestClient.builder(addresses: _*)
    } else {
      RestClient.builder(addresses: _*)
        .setHttpClientConfigCallback(httpClientBuilder => {
          val sslContext: SSLContext = config.getString("starchat.client.https.certificates.format") match {
            case "jks" =>
              val path = config.getString("starchat.client.https.certificates.jks.keystore")
              val password = config.getString("starchat.client.https.certificates.jks.password")
              SslContext.jks(path, password)
            case "pkcs12" | _ =>
              val path = config.getString("starchat.client.https.certificates.pkcs12.keystore")
              val password = config.getString("starchat.client.https.certificates.pkcs12.password")
              SslContext.pkcs12(path, password)
          }
          val httpBuilder = httpClientBuilder.setSSLContext(sslContext)
          if (config.getBoolean("starchat.client.https.disable_host_validation"))
            httpBuilder.setSSLHostnameVerifier((_: String, _: SSLSession) => true)
          httpBuilder
        })
    }
    clientBuilder
      .setDefaultHeaders(Array(("Authorization", "Basic " + elasticsearchAuthentication))
      .map { case (key, value) =>
        new BasicHeader(key, value)
      })
  })


  private def inetAddresses(): List[HttpHost] = {
    config.getString("es.host_map")
      .split(";")
      .map(x => x.split("=")).map(x => (x(0), x(1).toInt))
      .toMap
      .map { case (k, v) => new HttpHost(InetAddress.getByName(k), v, hostProto) }
      .toList
  }


}
