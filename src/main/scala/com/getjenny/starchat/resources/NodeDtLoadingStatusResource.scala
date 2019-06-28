package com.getjenny.starchat.resources

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.pattern.CircuitBreaker
import com.getjenny.starchat.entities.{NodeDtLoadingStatus, Permissions, ReturnMessageData}
import com.getjenny.starchat.routing.{StarChatCircuitBreaker, StarChatResource}
import com.getjenny.starchat.services.{NodeDtLoadingStatusService, NodeDtLoadingStatusServiceException}

import scala.concurrent.Future
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 29/01/19.
  */

trait NodeDtLoadingStatusResource extends StarChatResource {
  private[this] val nodeDtLoadingStatusService: NodeDtLoadingStatusService.type = NodeDtLoadingStatusService
  private[this] val routeName: String = """node_dt_update"""

  def nodeDtLoadingStatusRoutes: Route = handleExceptions(routesExceptionHandler) {
    pathPrefix(indexRegex ~ Slash ~ routeName) { indexName =>
      get {
        authenticateBasicAsync(realm = authRealm,
          authenticator = authenticator.authenticator) { user =>
          authorizeAsync(_ =>
            authenticator.hasPermissions(user, indexName, Permissions.write)) {
            parameters("strict".as[Boolean] ? false) { strict =>
              extractMethod { method =>
                val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                onCompleteWithBreaker(breaker)(Future {
                  nodeDtLoadingStatusService.loadingStatus(indexName, strict)
                }) {
                  case Success(t) =>
                    completeResponse(StatusCodes.OK, StatusCodes.BadRequest, t)
                  case Failure(e) =>
                    log.error("DtUpdate(" + indexName + ") route=nodeDtLoadingStatusRoutes method=GET : " + e.getMessage)
                    e match {
                      case authException: NodeDtLoadingStatusServiceException =>
                        completeResponse(StatusCodes.BadRequest, authException.getMessage)
                      case NonFatal(nonFatalE) =>
                        completeResponse(StatusCodes.BadRequest,
                          ReturnMessageData(code = 100, message = e.getMessage))
                          case _: Exception =>
                            completeResponse(StatusCodes.BadRequest, e.getMessage)
                    }
                }
              }
            }
          }
        }
      }
    } ~
      pathPrefix(routeName) {
        pathEnd {
          get {
            authenticateBasicAsync(realm = authRealm, authenticator = authenticator.authenticator) { user =>
              authorizeAsync(_ =>
                authenticator.hasPermissions(user, "admin", Permissions.admin)) {
                parameters("verbose".as[Boolean] ? false, "strict".as[Boolean] ? false) { (verbose, strict) =>
                  val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                  onCompleteWithBreaker(breaker)(Future {
                    nodeDtLoadingStatusService.nodeLoadingStatusAll(verbose, strict)
                  }
                  ) {
                    case Success(t) =>
                      completeResponse(StatusCodes.OK, StatusCodes.BadRequest, t)
                    case Failure(e) =>
                      log.error("index(all) route=nodeDtLoadingStatusRoutes method=GET : " + e.getMessage)
                      e match {
                        case authException: NodeDtLoadingStatusServiceException =>
                          completeResponse(StatusCodes.BadRequest, authException.getMessage)
                        case NonFatal(nonFatalE) =>
                          completeResponse(StatusCodes.BadRequest,
                            ReturnMessageData(code = 101, message = e.getMessage))
                        case _: Exception =>
                          completeResponse(StatusCodes.BadRequest, e.getMessage)
                      }
                  }
                }
              }
            }
          } ~
            post {
              authenticateBasicAsync(realm = authRealm,
                authenticator = authenticator.authenticator) { user =>
                authorizeAsync(_ =>
                  authenticator.hasPermissions(user, "admin", Permissions.read)) {
                  extractMethod { method =>
                    entity(as[NodeDtLoadingStatus]) { document =>
                      val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                      onCompleteWithBreaker(breaker)(Future {
                        nodeDtLoadingStatusService.update(document)
                      }) {
                        case Success(_) =>
                          completeResponse(StatusCodes.OK)
                        case Failure(e) =>
                          log.error("DtUpdate(" + document.uuid +
                            ", " + document.index +
                            ") route=nodeDtLoadingStatusRoutes method=" + method + " : " + e.getMessage)
                          completeResponse(StatusCodes.BadRequest,
                            Option {
                              ReturnMessageData(code = 102, message = e.getMessage)
                            })
                      }
                    }
                  }
                }
              }
            } ~
            delete {
              authenticateBasicAsync(realm = authRealm,
                authenticator = authenticator.authenticator) { user =>
                authorizeAsync(_ =>
                  authenticator.hasPermissions(user, "admin", Permissions.read)) {
                  extractMethod { method =>
                    val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                    onCompleteWithBreaker(breaker)(Future {
                      nodeDtLoadingStatusService.cleanDeadNodesRecords
                    }) {
                      case Success(t) =>
                        completeResponse(StatusCodes.OK, StatusCodes.BadRequest, t)
                      case Failure(e) =>
                        log.error("DtUpdate(" + nodeDtLoadingStatusService.clusterNodesService.uuid +
                          ") route=nodeDtLoadingStatusRoutes method=" + method + " : " + e.getMessage)
                        completeResponse(StatusCodes.BadRequest,
                          Option {
                            ReturnMessageData(code = 103, message = e.getMessage)
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
