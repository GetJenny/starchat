package com.getjenny.starchat.routing

import java.io.File
import java.util.concurrent.TimeoutException

import akka.event.{Logging, LoggingAdapter}
import akka.http.scaladsl.marshalling.ToEntityMarshaller
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.server.directives.FileInfo
import akka.http.scaladsl.server.{Directives, ExceptionHandler, Route}
import com.getjenny.starchat.SCActorSystem
import com.getjenny.starchat.serializers.JsonSupport
import com.getjenny.starchat.services.UserEsServiceException
import com.getjenny.starchat.services.auth.{AbstractStarChatAuthenticator, StarChatAuthenticator}
import com.getjenny.starchat.utils.Index
import com.typesafe.config.{Config, ConfigFactory}
import org.elasticsearch.index.IndexNotFoundException

import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal
import scala.util.matching.Regex

trait StarChatResource extends Directives with JsonSupport {
  implicit def executionContext: ExecutionContext
  protected[this] val defaultHeader: RawHeader = RawHeader("application", "json")
  protected[this] val config: Config = ConfigFactory.load()
  protected[this] val authRealm: String = config.getString("starchat.auth_realm")
  protected[this] val authenticator: AbstractStarChatAuthenticator = StarChatAuthenticator.authenticator
  protected[this] val log: LoggingAdapter = Logging(SCActorSystem.system, this.getClass.getCanonicalName)

  protected[this] val indexRegex: Regex = Index.indexMatchRegexDelimited
  protected[this] val orgNameRegex: Regex = Index.orgNameMatchRegexDelimited

  protected[this] def tempDestination(suffix: String)(fileInfo: FileInfo): File = {
    val file = File.createTempFile("uploadedFile_", suffix)
    log.info("Uploaded file(" + file.getAbsolutePath + ") contentType(" + fileInfo.contentType +
      ") fieldName(" + fileInfo.fieldName + ")")
    file
  }

  protected[this] val routesExceptionHandler = ExceptionHandler {
    case e: IndexNotFoundException =>
      extractUri { uri =>
        log.error("uri(" + uri + ") index error: " + e)
        respondWithDefaultHeader(defaultHeader) {
          complete(StatusCodes.BadRequest)
        }
      }
    case e: TimeoutException =>
      extractUri { uri =>
        log.error("uri(" + uri + ") request timeout: " + e)
        respondWithDefaultHeader(defaultHeader) {
          complete(StatusCodes.RequestTimeout)
        }
      }
    case e: UserEsServiceException =>
      extractUri { uri =>
        log.error("uri(" + uri + ") Unauthorized: " + e)
        respondWithDefaultHeader(defaultHeader) {
          complete(StatusCodes.Unauthorized)
        }
      }
    case NonFatal(e) =>
      extractUri { uri =>
        log.error("uri(" + uri + ") Internal Error: " + e)
        respondWithDefaultHeader(defaultHeader) {
          complete(StatusCodes.BadRequest)
        }
      }
  }

  protected[this] def completeResponse(status_code: StatusCode): Route = {
    complete(status_code)
  }

  protected[this] def completeResponse[A: ToEntityMarshaller](statusCode: StatusCode, data: Option[A]): Route = {
    data match {
      case Some(t) =>
        respondWithDefaultHeader(defaultHeader) {
          complete(statusCode, t)
        }
      case None =>
        complete(statusCode)
    }
  }

  protected[this] def completeResponse[A: ToEntityMarshaller](statusCode: StatusCode, data: A): Route = {
    respondWithDefaultHeader(defaultHeader) {
      complete(statusCode, data)
    }
  }

  protected[this] def completeResponse[A: ToEntityMarshaller](statusCodeOk: StatusCode, statusCodeFailed: StatusCode,
                                              data: Option[A]): Route = {
    data match {
      case Some(t) =>
        respondWithDefaultHeader(defaultHeader) {
          complete(statusCodeOk, t)
        }
      case None =>
        complete(statusCodeFailed)
    }
  }

  protected[this] def completeResponse[A: ToEntityMarshaller](statusCodeOk: StatusCode, statusCodeFailed: StatusCode,
                                              data: A): Route = {
    respondWithDefaultHeader(defaultHeader) {
      complete(statusCodeOk, data)
    }
  }

}
