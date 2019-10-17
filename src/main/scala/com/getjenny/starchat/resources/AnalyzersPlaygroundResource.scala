package com.getjenny.starchat.resources

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 07/04/17.
  */

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.pattern.CircuitBreaker
import com.getjenny.starchat.entities._
import com.getjenny.starchat.routing._
import com.getjenny.starchat.services.AnalyzerService

import scala.util.{Failure, Success}

trait AnalyzersPlaygroundResource extends StarChatResource {
  private[this] val analyzerService: AnalyzerService.type = AnalyzerService

  def analyzersPlaygroundRoutes: Route = handleExceptions(routesExceptionHandler) {
    pathPrefix(indexRegex ~ Slash ~ "analyzer" ~ Slash ~ "playground") { indexName =>
      pathEnd {
        post {
          authenticateBasicAsync(realm = authRealm,
            authenticator = authenticator.authenticator) { user =>
            authorizeAsync(_ =>
              authenticator.hasPermissions(user, indexName,
                Set(Permissions.read, Permissions.write, Permissions.read))) {
              entity(as[AnalyzerEvaluateRequest]) { request =>
                val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                onCompleteWithBreaker(breaker)(analyzerService.evaluateAnalyzer(indexName, request)) {
                  case Success(value) =>
                    completeResponse(StatusCodes.OK, StatusCodes.BadRequest, value)
                  case Failure(e) =>
                    log.error("user(" + user + ")index(" + indexName + ") route=analyzersPlaygroundRoutes method=POST: " + e.getMessage)
                    completeResponse(StatusCodes.BadRequest,
                      Option {
                        ReturnMessageData(code = 100, message = e.getMessage)
                      })
                }
              }
            }
          }
        }
      }
    }
  }
}
