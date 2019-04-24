package com.getjenny.starchat.resources

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 19/12/16.
  */

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.pattern.CircuitBreaker
import com.getjenny.starchat.entities._
import com.getjenny.starchat.routing._
import com.getjenny.starchat.services.IndexManagementService

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success}

trait IndexManagementResource extends StarChatResource {

  private[this] val indexManagementService: IndexManagementService.type = IndexManagementService

  def postIndexManagementCreateRoutes: Route = handleExceptions(routesExceptionHandler) {
    pathPrefix(indexRegex ~ Slash ~ "index_management" ~ Slash ~ """create""") {
      indexName =>
        post {
          authenticateBasicAsync(realm = authRealm,
            authenticator = authenticator.authenticator) { user =>
            authorizeAsync(_ =>
              authenticator.hasPermissions(user, "admin", Permissions.admin)) {
              parameters("indexSuffix".as[Option[String]] ? Option.empty[String]) { indexSuffix =>
                val breaker: CircuitBreaker = StarChatCircuitBreaker
                  .getCircuitBreaker(maxFailure = 10, callTimeout = 30.seconds)
                onCompleteWithBreaker(breaker)(
                  indexManagementService.create(indexName = indexName, indexSuffix = indexSuffix)
                ) {
                  case Success(t) => completeResponse(StatusCodes.Created, StatusCodes.BadRequest, Option {
                    t
                  })
                  case Failure(e) => completeResponse(StatusCodes.BadRequest,
                    Option {
                      IndexManagementResponse(message = e.getMessage)
                    })
                }
              }
            }
          }
        }
    }
  }

  def postIndexManagementOpenCloseRoutes: Route = handleExceptions(routesExceptionHandler) {
    pathPrefix(indexRegex ~ Slash ~ "index_management" ~ Slash ~ """(open|close)""".r) {
      (indexName, operation) =>
        post {
          authenticateBasicAsync(realm = authRealm,
            authenticator = authenticator.authenticator) { user =>
            authorizeAsync(_ =>
              authenticator.hasPermissions(user, indexName, Permissions.write)) {
              parameters("indexSuffix".as[Option[String]] ? Option.empty[String]) { indexSuffix =>
                val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                onCompleteWithBreaker(breaker)(
                  Future { indexManagementService.openClose(indexName = indexName, indexSuffix = indexSuffix,
                    operation = operation) }
                ) {
                  case Success(t) => completeResponse(StatusCodes.OK, StatusCodes.BadRequest, Option {
                    t
                  })
                  case Failure(e) => completeResponse(StatusCodes.BadRequest,
                    Option {
                      IndexManagementResponse(message = e.getMessage)
                    })
                }
              }
            }
          }
        }
    }
  }

  def postIndexManagementRefreshRoutes: Route = handleExceptions(routesExceptionHandler) {
    pathPrefix(indexRegex ~ Slash ~ "index_management" ~ Slash ~ """refresh""") {
      indexName =>
        post {
          authenticateBasicAsync(realm = authRealm,
            authenticator = authenticator.authenticator) { user =>
            authorizeAsync(_ =>
              authenticator.hasPermissions(user, indexName, Permissions.write)) {
              parameters("indexSuffix".as[Option[String]] ? Option.empty[String]) { indexSuffix =>
                val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                onCompleteWithBreaker(breaker)(
                  indexManagementService.refresh(indexName = indexName, indexSuffix = indexSuffix)
                ) {
                  case Success(t) => completeResponse(StatusCodes.OK, StatusCodes.BadRequest, Option {
                    t
                  })
                  case Failure(e) => completeResponse(StatusCodes.BadRequest,
                    Option {
                      IndexManagementResponse(message = e.getMessage)
                    })
                }
              }
            }
          }
        }
    }
  }

  def putIndexManagementRoutes: Route = handleExceptions(routesExceptionHandler) {
    pathPrefix(indexRegex ~ Slash ~ "index_management" ~ Slash ~ """(mappings|settings)""".r) {
      (indexName, mappingOrSettings) =>
        pathEnd {
          put {
            authenticateBasicAsync(realm = authRealm,
              authenticator = authenticator.authenticator) { user =>
              authorizeAsync(_ =>
                authenticator.hasPermissions(user, "admin", Permissions.admin)) {
                parameters("indexSuffix".as[Option[String]] ? Option.empty[String]) { indexSuffix =>
                  val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                  onCompleteWithBreaker(breaker)(
                    mappingOrSettings match {
                      case "mappings" => indexManagementService.updateMappings(indexName = indexName,
                        indexSuffix = indexSuffix)
                      case "settings" => indexManagementService.updateSettings(indexName = indexName,
                        indexSuffix = indexSuffix)
                    }
                  ) {
                    case Success(t) => completeResponse(StatusCodes.OK, StatusCodes.BadRequest, Option {
                      t
                    })
                    case Failure(e) => completeResponse(StatusCodes.BadRequest,
                      Option {
                        IndexManagementResponse(message = e.getMessage)
                      })
                  }
                }
              }
            }
          }
        }
    }
  }

  def indexManagementRoutes: Route = handleExceptions(routesExceptionHandler) {
    pathPrefix(indexRegex ~ Slash ~ "index_management") {
      indexName =>
        pathEnd {
          get {
            authenticateBasicAsync(realm = authRealm,
              authenticator = authenticator.authenticator) { user =>
              authorizeAsync(_ =>
                authenticator.hasPermissions(user, indexName, Permissions.read)) {
                parameters("indexSuffix".as[Option[String]] ? Option.empty[String]) { indexSuffix =>
                  val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                  onCompleteWithBreaker(breaker)(
                    Future { indexManagementService.check(indexName = indexName, indexSuffix = indexSuffix) }
                  ) {
                    case Success(t) => completeResponse(StatusCodes.OK, StatusCodes.BadRequest, Option {
                      t
                    })
                    case Failure(e) => completeResponse(StatusCodes.BadRequest,
                      Option {
                        IndexManagementResponse(message = e.getMessage)
                      })
                  }
                }
              }
            }
          } ~
            delete {
              authenticateBasicAsync(realm = authRealm,
                authenticator = authenticator.authenticator) { user =>
                authorizeAsync(_ =>
                  authenticator.hasPermissions(user, "admin", Permissions.admin)) {
                  parameters("indexSuffix".as[Option[String]] ? Option.empty[String]) { indexSuffix =>
                    val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                    onCompleteWithBreaker(breaker)(
                      indexManagementService.remove(indexName = indexName, indexSuffix = indexSuffix)
                    ) {
                      case Success(t) => completeResponse(StatusCodes.OK, StatusCodes.BadRequest, Option {
                        t
                      })
                      case Failure(e) => completeResponse(StatusCodes.BadRequest,
                        Option {
                          IndexManagementResponse(message = e.getMessage)
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

