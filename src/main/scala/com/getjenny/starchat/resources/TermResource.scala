package com.getjenny.starchat.resources

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 12/03/17.
  */

import akka.NotUsed
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.pattern.CircuitBreaker
import akka.stream.scaladsl.Source
import com.getjenny.starchat.entities._
import com.getjenny.starchat.routing._
import com.getjenny.starchat.services.TermService

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success}

trait TermResource extends StarChatResource {

  private[this] val termService: TermService.type = TermService

  def termStreamRoutes: Route = handleExceptions(routesExceptionHandler) {
    pathPrefix(indexRegex ~ Slash ~ """stream""" ~ Slash ~ """term""") { indexName =>
      pathEnd {
        get {
          authenticateBasicAsync(realm = authRealm,
            authenticator = authenticator.authenticator) { user =>
            authorizeAsync(_ =>
              authenticator.hasPermissions(user, indexName, Permissions.stream)) {
              extractMethod { method =>
                val entryIterator = termService.allDocuments(indexName)
                val entries: Source[Term, NotUsed] =
                  Source.fromIterator(() => entryIterator)
                log.info("index(" + indexName + ") route=termRoutes method=" + method + " function=index")
                complete(entries)
              }
            }
          }
        }
      }
    }
  }

  def termRoutes: Route = handleExceptions(routesExceptionHandler) {
    pathPrefix(indexRegex ~ Slash ~ "term") { indexName =>
      path(Segment) { operation: String =>
        post {
          operation match {
            case "index" =>
              authenticateBasicAsync(realm = authRealm,
                authenticator = authenticator.authenticator) { user =>
                authorizeAsync(_ =>
                  authenticator.hasPermissions(user, indexName, Permissions.write)) {
                  parameters("refresh".as[Int] ? 0) { refresh =>
                    entity(as[Terms]) { request_data =>
                      val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                      onCompleteWithBreaker(breaker)(termService.indexTermFuture(indexName, request_data, refresh)) {
                        case Success(t) =>
                          completeResponse(StatusCodes.OK, StatusCodes.BadRequest, t)
                        case Failure(e) =>
                          log.error("index(" + indexName + ") route=termRoutes method=POST function=index : " +
                            e.getMessage)
                          completeResponse(StatusCodes.BadRequest,
                            Option {
                              ReturnMessageData(code = 100, message = e.getMessage)
                            })
                      }
                    }
                  }
                }
              }
            case "distance" =>
              authenticateBasicAsync(realm = authRealm,
                authenticator = authenticator.authenticator) { user =>
                authorizeAsync(_ =>
                  authenticator.hasPermissions(user, indexName, Permissions.read)) {
                  entity(as[DocsIds]) { requestData =>
                    val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                    onCompleteWithBreaker(breaker)(
                      termService.termsDistance( indexName = indexName, termsReq = requestData)
                    ) {
                      case Success(t) =>
                        completeResponse(StatusCodes.OK, StatusCodes.BadRequest, t)
                      case Failure(e) =>
                        log.error("index(" + indexName + ") route=termRoutes method=POST function=get : " +
                          e.getMessage)
                        completeResponse(StatusCodes.BadRequest,
                          Option {
                            ReturnMessageData(code = 101, message = e.getMessage)
                          })
                    }
                  }
                }
              }
            case "index_default_synonyms" =>
              withoutRequestTimeout {
                authenticateBasicAsync(realm = authRealm,
                  authenticator = authenticator.authenticator) { user =>
                  authorizeAsync(_ =>
                    authenticator.hasPermissions(user, indexName, Permissions.write)) {
                    parameters("refresh".as[Int] ? 0) { refresh =>
                      val breaker: CircuitBreaker =
                        StarChatCircuitBreaker.getCircuitBreaker(maxFailure = 5, callTimeout = 120.seconds,
                          resetTimeout = 120.seconds)
                      onCompleteWithBreaker(breaker)(
                        termService.indexDefaultSynonyms(indexName = indexName, refresh = refresh)) {
                        case Success(t) =>
                          completeResponse(StatusCodes.OK, StatusCodes.BadRequest, t)
                        case Failure(e) =>
                          log.error("index(" + indexName + ") " +
                            "route=termRoutes method=POST function=index_default_synonyms : " + e.getMessage)
                          completeResponse(StatusCodes.BadRequest,
                            Option {
                              ReturnMessageData(code = 102, message = e.getMessage)
                            })
                      }
                    }
                  }
                }
              }
            case "index_synonyms" =>
              withoutRequestTimeout {
                authenticateBasicAsync(realm = authRealm,
                  authenticator = authenticator.authenticator) { user =>
                  authorizeAsync(_ =>
                    authenticator.hasPermissions(user, indexName, Permissions.write)) {
                    storeUploadedFile("csv", tempDestination(".csv")) {
                      case (_, file) =>
                        val breaker: CircuitBreaker =
                          StarChatCircuitBreaker.getCircuitBreaker(maxFailure = 5,
                            callTimeout = 120.seconds, resetTimeout = 120.seconds)
                        onCompleteWithBreaker(breaker)(termService.indexSynonymsFromCsvFile(indexName, file)) {
                          case Success(t) =>
                            file.delete()
                            completeResponse(StatusCodes.OK, StatusCodes.BadRequest, Option {
                              t
                            })
                          case Failure(e) =>
                            log.error("index(" + indexName + ") " +
                              "route=termRoutes method=POST function=index_synonyms : " + e.getMessage)
                            if (file.exists()) {
                              file.delete()
                            }
                            completeResponse(StatusCodes.BadRequest,
                              Option {
                                ReturnMessageData(code = 103, message = e.getMessage)
                              })
                        }
                    }
                  }
                }
              }
            case "get" =>
              authenticateBasicAsync(realm = authRealm,
                authenticator = authenticator.authenticator) { user =>
                authorizeAsync(_ =>
                  authenticator.hasPermissions(user, indexName, Permissions.read)) {
                  entity(as[DocsIds]) { requestData =>
                    val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                    onCompleteWithBreaker(breaker)(
                      termService.getTermsByIdFuture(
                        indexName = indexName,
                        termsRequest = requestData)
                    ) {
                      case Success(t) =>
                        completeResponse(StatusCodes.OK, StatusCodes.BadRequest, t)
                      case Failure(e) =>
                        log.error("index(" + indexName + ") route=termRoutes method=POST function=get : " +
                          e.getMessage)
                        completeResponse(StatusCodes.BadRequest,
                          Option {
                            ReturnMessageData(code = 104, message = e.getMessage)
                          })
                    }
                  }
                }
              }
            case "delete" =>
              authenticateBasicAsync(realm = authRealm,
                authenticator = authenticator.authenticator) { user =>
                authorizeAsync(_ =>
                  authenticator.hasPermissions(user, indexName, Permissions.write)) {
                  parameters("refresh".as[Int] ? 0) { refresh =>
                    entity(as[DocsIds]) { requestData =>
                      if (requestData.ids.nonEmpty) {
                        val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                        onCompleteWithBreaker(breaker)(
                          Future { termService.delete(indexName, requestData.ids, refresh) }
                        ) {
                          case Success(t) =>
                            completeResponse(StatusCodes.OK, StatusCodes.BadRequest, t)
                          case Failure(e) =>
                            log.error("index(" + indexName + ") route=termRoutes method=DELETE : " + e.getMessage)
                            completeResponse(StatusCodes.BadRequest,
                              Option {
                                ReturnMessageData(code = 105, message = e.getMessage)
                              })
                        }
                      } else {
                        val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                        onCompleteWithBreaker(breaker)(
                          Future { termService.deleteAll(indexName) }
                        ) {
                          case Success(t) =>
                            completeResponse(StatusCodes.OK, StatusCodes.BadRequest, t)
                          case Failure(e) =>
                            log.error("index(" + indexName + ") route=termRoutes method=DELETE : " + e.getMessage)
                            completeResponse(StatusCodes.BadRequest,
                              Option {
                                ReturnMessageData(code = 106, message = e.getMessage)
                              })
                        }
                      }
                    }
                  }
                }
              }
            case "term" =>
              authenticateBasicAsync(realm = authRealm,
                authenticator = authenticator.authenticator) { user =>
                authorizeAsync(_ => authenticator.hasPermissions(user, indexName, Permissions.read)) {
                  entity(as[SearchTerm]) { requestData =>
                    val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                    parameters("analyzer".as[String] ? "space_punctuation") { analyzer =>
                      onCompleteWithBreaker(breaker)(
                        termService.searchFuture(indexName = indexName, query = requestData, analyzer = analyzer)
                      ) {
                        case Success(t) =>
                          completeResponse(StatusCodes.OK, StatusCodes.BadRequest, t)
                        case Failure(e) =>
                          log.error("index(" + indexName + ") route=termRoutes method=GET function=term : " +
                            e.getMessage)
                          completeResponse(StatusCodes.BadRequest,
                            Option {
                              IndexManagementResponse(message = e.getMessage)
                            })
                        }
                      }
                    }
                  }
                }
            case "text" =>
              authenticateBasicAsync(realm = authRealm,
                authenticator = authenticator.authenticator) { user =>
                authorizeAsync(_ => authenticator.hasPermissions(user, indexName, Permissions.read)) {
                  entity(as[String]) { requestData =>
                    val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                    parameters("analyzer".as[String] ? "space_punctuation") { analyzer =>
                      onCompleteWithBreaker(breaker)(
                        termService.searchFuture(indexName = indexName, query = requestData, analyzer = analyzer)
                      ) {
                        case Success(t) =>
                          completeResponse(StatusCodes.OK, StatusCodes.BadRequest, t)
                        case Failure(e) =>
                          log.error("index(" + indexName + ") route=termRoutes method=GET function=text : " +
                            e.getMessage)
                           completeResponse(StatusCodes.BadRequest,
                            Option {
                              IndexManagementResponse(message = e.getMessage)
                            })
                        }
                      }
                    }
                  }
                }
            case _ => completeResponse(StatusCodes.BadRequest,
              Option {
                IndexManagementResponse(message = "index(" + indexName + ") Operation not supported: " +
                  operation)
              })
          }
        }
      } ~
        pathEnd {
          put {
            authenticateBasicAsync(realm = authRealm,
              authenticator = authenticator.authenticator) { user =>
              authorizeAsync(_ =>
                authenticator.hasPermissions(user, indexName, Permissions.write)) {
                parameters("refresh".as[Int] ? 0) { refresh =>
                  entity(as[Terms]) { request_data =>
                    val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                    onCompleteWithBreaker(breaker)(termService.updateTermFuture(indexName, request_data, refresh)) {
                      case Success(t) =>
                        completeResponse(StatusCodes.OK, StatusCodes.BadRequest, t)
                      case Failure(e) =>
                        log.error("index(" + indexName + ") route=termRoutes method=PUT : " + e.getMessage)
                        completeResponse(StatusCodes.BadRequest, Option {
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
}
