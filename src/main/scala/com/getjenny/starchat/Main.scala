package com.getjenny.starchat

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 27/06/16.
  */

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.{ConnectionContext, Http, HttpsConnectionContext}
import akka.stream.ActorMaterializer
import com.getjenny.starchat.utils.SslContext

import scala.concurrent.ExecutionContextExecutor

case class Parameters(
                       http_enable: Boolean,
                       http_host: String,
                       http_port: Int,
                       https_enable: Boolean,
                       https_host: String,
                       https_port: Int)

final class StarChatService(parameters: Option[Parameters] = None) extends RestInterface {
  val params: Parameters = parameters match {
    case Some(p) => p
    case _ =>
      Parameters(
        http_enable = config.getBoolean("starchat.http.enable"),
        http_host = config.getString("starchat.http.host"),
        http_port = config.getInt("starchat.http.port"),
        https_enable = config.getBoolean("starchat.https.enable"),
        https_host = config.getString("starchat.https.host"),
        https_port = config.getInt("starchat.https.port")
      )
  }

  /* creation of the akka actor system which handle concurrent requests */
  implicit val system: ActorSystem = SCActorSystem.system
  /* "The Materializer is a factory for stream execution engines, it is the thing that makes streams run" */
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit val executionContext: ExecutionContextExecutor = system.dispatcher

  if (params.https_enable) {
    val certFormat = config.getString("starchat.https.certificates.format")
    val sslCtx = certFormat match {
      case "jks" =>
        val path = config.getString("starchat.https.certificates.jks.keystore")
        val password = config.getString("starchat.https.certificates.jks.password")
        SslContext.jks(path, password)
      case "pkcs12" | _ =>
        val path = config.getString("starchat.https.certificates.pkcs12.keystore")
        val password = config.getString("starchat.https.certificates.pkcs12.password")
        SslContext.pkcs12(path, password)
    }

    val https: HttpsConnectionContext = ConnectionContext.https(
      sslContext = sslCtx,
      sslConfig = None,
      enabledCipherSuites = None,
      enabledProtocols = None,
      clientAuth = None,
      sslParameters = None
    )

    Http()
      .bindAndHandleAsync(handler = Route.asyncHandler(routes),
        interface = params.https_host, port = params.https_port,
        connectionContext = https, log = system.log) map { binding =>
      system.log.info(s"REST (HTTPS) interface bound to ${binding.localAddress}")
    } recover { case eX =>
      system.log.error(s"REST (HTTPS) interface could not bind to ${params.https_host}:${params.https_port}",
        eX.getMessage)
    }
  }

  if((! params.https_enable) || params.http_enable) {
    Http().bindAndHandleAsync(handler = Route.asyncHandler(routes), interface = params.http_host,
      port = params.http_port, log = system.log) map { binding =>
      system.log.info(s"REST (HTTP) interface bound to ${binding.localAddress}")
    } recover { case eX: Exception =>
      system.log.error(s"REST (HTTP) interface could not bind to ${params.http_host}:${params.http_port}",
        eX.getMessage)
    }
  }

  /* activate cron job for initializing system indices */
  if(systemIndexManagementService.elasticClient.autoInitializeSystemIndex)
    cronInitializeSystemIndicesService.scheduleAction()

  /* activate cron jobs for data synchronization */
  cronReloadDTService.scheduleAction()
  cronCleanDTService.scheduleAction()

  /* activate cron jobs for the alive nodes listing */
  cronCleanDeadNodesService.scheduleAction()
  cronNodeAliveSignalService.scheduleAction()
  cronCleanDtLoadingRecordsService.scheduleAction()
}

object Main extends App {
  val starChatService = new StarChatService()
}
