package com.getjenny.starchat.resources

/**
 * Created by Angelo Leto <angelo@getjenny.com> on 27/06/16.
 */


import akka.http.javadsl.server.CircuitBreakerOpenRejection
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.pattern.CircuitBreaker
import com.getjenny.analyzer.analyzers.AnalyzerEvaluationException
import com.getjenny.starchat.entities.io
import com.getjenny.starchat.entities.io._
import com.getjenny.starchat.entities.persistents.{DTDocument, DTDocumentUpdate, SuggestedQuery}
import com.getjenny.starchat.routing._
import com.getjenny.starchat.services._
import scalaz.Scalaz._

import scala.concurrent.duration._
import scala.util.control.NonFatal
import scala.util.matching.Regex
import scala.util.{Failure, Success}

trait DecisionTableResource extends StarChatResource {

  private[this] val decisionTableService: DecisionTableService.type = DecisionTableService
  private[this] val analyzerService: AnalyzerService.type = AnalyzerService
  private[this] val responseService: ResponseService.type = ResponseService
  private[this] val dtReloadService: InstanceRegistryService.type = InstanceRegistryService
  private[this] val bayesOperatorCacheService: BayesOperatorCacheService.type = BayesOperatorCacheService
  private[this] val autoCompleteService: AutoCompleteService.type  = AutoCompleteService
  private[this] val fileTypeRegex: Regex = "^(csv|json)$".r

  def decisionTableCloneIndexRoutes: Route = handleExceptions(routesExceptionHandler) {
    pathPrefix(indexRegex ~ Slash ~ "decisiontable" ~ Slash ~ "clone" ~ Slash ~ indexRegex) {
      (indexNameSrc, indexNameDst)  =>
        pathEnd {
          post {
            authenticateBasicAsync(realm = authRealm,
              authenticator = authenticator.authenticator) { user =>
              authorizeAsync(_ => authenticator.hasPermissions(user, indexNameSrc, Permissions.read)) {
                authorizeAsync(_ => authenticator.hasPermissions(user, indexNameDst, Permissions.write)) {
                  extractRequest { request =>
                    parameters("reset".as[Boolean] ? true,
                      "propagate".as[Boolean] ? true,
                      "refresh".as[RefreshPolicy.Value] ? RefreshPolicy.`wait_for`) { (reset, propagate, refreshPolicy) =>
                      val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker(callTimeout = 120.seconds)
                      onCompleteWithBreakerFuture(breaker)(
                        decisionTableService.cloneIndexContentReindex(
                          indexNameSrc = indexNameSrc,
                          indexNameDst = indexNameDst,
                          reset = reset,
                          propagate = propagate,
                          refreshPolicy = refreshPolicy
                        )
                      ) {
                        case Success(t) =>
                          completeResponse(StatusCodes.OK, StatusCodes.BadRequest, t)
                        case Failure(e) =>
                          log.error(logTemplate(user.id, indexNameSrc + ":" + indexNameDst,
                            "decisionTableCloneIndexRoutes", request.method, request.uri), e)
                          completeResponse(StatusCodes.BadRequest,
                            Option {
                              ReturnMessageData(code = 104, message = e.getMessage)
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

  def decisionTableRoutesAllRoutes: Route = handleExceptions(routesExceptionHandler) {
    pathPrefix(indexRegex ~ Slash ~ "decisiontable" ~ Slash ~ "all") { indexName =>
      pathEnd {
        delete {
          authenticateBasicAsync(realm = authRealm,
            authenticator = authenticator.authenticator) { user =>
            authorizeAsync(_ =>
              authenticator.hasPermissions(user, indexName, Permissions.write)) {
              extractRequest { request =>
                parameters("refresh".as[RefreshPolicy.Value] ? RefreshPolicy.`wait_for`) { refreshPolicy =>
                  val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                  onCompleteWithBreakerFuture(breaker)(
                    decisionTableService.deleteAll(indexName, refreshPolicy)
                  ) {
                    case Success(t) =>
                      completeResponse(StatusCodes.OK, StatusCodes.BadRequest, t)
                    case Failure(e) =>
                      log.error(logTemplate(user.id, indexName, "decisionTableRoutes", request.method, request.uri), e)
                      completeResponse(StatusCodes.BadRequest,
                        Option {
                          ReturnMessageData(code = 105, message = e.getMessage)
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

  def decisionTableUploadFilesRoutes: Route = handleExceptions(routesExceptionHandler) {
    pathPrefix(indexRegex ~ Slash ~ "decisiontable" ~ Slash ~ "upload" ~ Slash ~ fileTypeRegex) { (indexName, fileType) =>
      pathEnd {
        post {
          authenticateBasicAsync(realm = authRealm, authenticator = authenticator.authenticator) { user =>
            authorizeAsync(_ =>
              authenticator.hasPermissions(user, indexName, Permissions.write)) {
              extractRequest { request =>
                storeUploadedFile(fileType, tempDestination("." + fileType)) {
                  case (_, file) =>
                    val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker(callTimeout = 120.seconds)
                    onCompleteWithBreakerFuture(breaker)(
                      if (fileType === "csv") {
                        decisionTableService.indexCSVFileIntoDecisionTable(indexName = indexName,
                          file = file, skipLines = 0, refreshPolicy = RefreshPolicy.`wait_for`)
                      } else if (fileType === "json") {
                        decisionTableService.indexJSONFileIntoDecisionTable(indexName = indexName, file = file,
                          refreshPolicy = RefreshPolicy.`false`)
                      } else {
                        throw DecisionTableServiceException("Bad or unsupported file format: " + fileType)
                      }
                    ) {
                      case Success(t) =>
                        file.delete()
                        completeResponse(StatusCodes.OK, StatusCodes.BadRequest, Option {
                          t
                        })
                      case Failure(e) =>
                        log.error(logTemplate(user.id, indexName, "decisionTableUploadCSVRoutes",
                          request.method, request.uri), e)
                        if (file.exists()) {
                          file.delete()
                        }
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
      }
    }
  }

  def decisionTableBulkCreateRoutes: Route = handleExceptions(routesExceptionHandler) {
    pathPrefix(indexRegex ~ Slash ~ "decisiontable" ~ Slash ~ "bulk") { indexName =>
      pathEnd {
        post {
          authenticateBasicAsync(realm = authRealm,
            authenticator = authenticator.authenticator) { user =>
            authorizeAsync(_ =>
              authenticator.hasPermissions(user, indexName, Permissions.read)) {
              extractRequest { request =>
                parameters("check".as[Boolean] ? true,
                  "refresh".as[RefreshPolicy.Value] ? RefreshPolicy.`wait_for`) { (check, refreshPolicy) =>
                  entity(as[IndexedSeq[DTDocument]]) { documents =>
                    val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker(callTimeout = 120.seconds)
                    onCompleteWithBreakerFuture(breaker)(
                      decisionTableService.bulkCreate(indexName = indexName, documents = documents,
                        check = check, refreshPolicy = refreshPolicy)) {
                      case Success(t) =>
                        completeResponse(StatusCodes.OK, StatusCodes.BadRequest, Some(t))
                      case Failure(e) =>
                        log.error(logTemplate(user.id, indexName, "decisionTableBulkCreateRoutes",
                          request.method, request.uri), e)
                        completeResponse(StatusCodes.BadRequest,
                          Option {
                            ReturnMessageData(code = 107, message = e.getMessage)
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

  def decisionTableAsyncReloadRoutes: Route = handleExceptions(routesExceptionHandler) {
    pathPrefix(indexRegex ~ Slash ~ "decisiontable" ~ Slash ~ "analyzer" ~ Slash ~ "async") { indexName =>
      pathEnd {
        post {
          authenticateBasicAsync(realm = authRealm,
            authenticator = authenticator.authenticator) { user =>
            authorizeAsync(_ =>
              authenticator.hasPermissions(user, indexName, Permissions.write)) {
              extractRequest { request =>
                val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                onCompleteWithBreakerFuture(breaker)(dtReloadService.updateTimestamp(indexName)) {
                  case Success(t) =>
                    completeResponse(StatusCodes.Accepted, StatusCodes.BadRequest, Option {
                      t
                    })
                  case Failure(e) =>
                    log.error(logTemplate(user.id, indexName, "decisionTableAsyncReloadRoutes",
                      request.method, request.uri), e)
                    completeResponse(StatusCodes.BadRequest,
                      Option {
                        ReturnMessageData(code = 108, message = e.getMessage)
                      })
                }
              }
            }
          }
        }
      }
    }
  }

  def decisionTableAnalyzerRoutes: Route = handleExceptions(routesExceptionHandler) {
    pathPrefix(indexRegex ~ Slash ~ "decisiontable" ~ Slash ~ "analyzer") { indexName =>
      pathEnd {
        get {
          authenticateBasicAsync(realm = authRealm,
            authenticator = authenticator.authenticator) { user =>
            authorizeAsync(_ =>
              authenticator.hasPermissions(user, indexName, Permissions.write)) {
              extractRequest { request =>
                val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                onCompleteWithBreakerFuture(breaker)(analyzerService.getDTAnalyzerMap(indexName)) {
                  case Success(t) =>
                    completeResponse(StatusCodes.OK, StatusCodes.BadRequest, Option {
                      t
                    })
                  case Failure(e) =>
                    log.error(logTemplate(user.id, indexName, "decisionTableAnalyzerRoutes",
                      request.method, request.uri), e)
                    completeResponse(StatusCodes.BadRequest,
                      Option {
                        ReturnMessageData(code = 109, message = e.getMessage)
                      })
                }
              }
            }
          }
        } ~
          post {
            authenticateBasicAsync(realm = authRealm,
              authenticator = authenticator.authenticator) { user =>
              authorizeAsync(_ =>
                authenticator.hasPermissions(user, indexName, Permissions.write)) {
                extractRequest { request =>
                  parameters("propagate".as[Boolean] ? true,
                    "incremental".as[Boolean] ? true) { (propagate, incremental) =>
                    val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker(callTimeout = 120.seconds)
                    onCompleteWithBreakerFuture(breaker)(analyzerService.loadAnalyzers(indexName = indexName,
                      incremental = incremental, propagate = propagate)) {
                      case Success(t) =>
                        completeResponse(StatusCodes.OK, StatusCodes.BadRequest, Option {
                          t
                        })
                      case Failure(e) =>
                        log.error(logTemplate(user.id, indexName, "decisionTableAnalyzerRoutes",
                          request.method, request.uri), e)
                        completeResponse(StatusCodes.BadRequest,
                          Option {
                            ReturnMessageData(code = 110, message = e.getMessage)
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

  def decisionTableSearchRoutes: Route = handleExceptions(routesExceptionHandler) {
    pathPrefix(indexRegex ~ Slash ~ "decisiontable" ~ Slash ~ "search") { indexName =>
      pathEnd {
        post {
          authenticateBasicAsync(realm = authRealm,
            authenticator = authenticator.authenticator) { user =>
            authorizeAsync(_ =>
              authenticator.hasPermissions(user, indexName, Permissions.read)) {
              extractRequest { request =>
                entity(as[DTDocumentSearch]) { docsearch =>
                  val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                  onCompleteWithBreakerFuture(breaker)(decisionTableService.search(indexName, docsearch)) {
                    case Success(t) =>
                      completeResponse(StatusCodes.OK, StatusCodes.BadRequest, Option {
                        t
                      })
                    case Failure(e) =>
                      log.error(logTemplate(user.id, indexName, "decisionTableSearchRoutes",
                        request.method, request.uri), e)
                      completeResponse(StatusCodes.BadRequest,
                        Option {
                          ReturnMessageData(code = 111, message = e.getMessage)
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

  def decisionTableResponseRequestRoutes: Route = handleExceptions(routesExceptionHandler) {
    pathPrefix(indexRegex ~ Slash ~ "get_next_response") { indexName =>
      pathEnd {
        post {
          authenticateBasicAsync(realm = authRealm,
            authenticator = authenticator.authenticator) { user =>
            authorizeAsync(_ =>
              authenticator.hasPermissions(user, indexName, Permissions.read)) {
              extractRequest { request =>
                entity(as[ResponseRequestIn]) {
                  response_request =>
                    val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                    onCompleteWithBreakerFuture(breaker)(responseService.getNextResponse(indexName, response_request)) {
                      case Failure(e) =>
                        e match {
                          case rsDtNotLoadedE: ResponseServiceDTNotLoadedException =>
                            completeResponse(StatusCodes.ResetContent,
                              Option {
                                ResponseRequestOutOperationResult(
                                  ReturnMessageData(code = 112, message = rsDtNotLoadedE.getMessage),
                                  Option {
                                    List.empty[ResponseRequestOut]
                                  })
                              }
                            )
                          case e@(_: ResponseServiceNoResponseException) =>
                            log.error(logTemplate(user.id, indexName, "decisionTableResource", request.method,
                              request.uri, "No response"), e)
                            completeResponse(StatusCodes.NoContent)
                          case e@(_: AnalyzerEvaluationException) =>
                            val message = logTemplate(user.id, indexName, "decisionTableResource", request.method,
                              request.uri, "Unable to complete the request, due to analyzer")
                            log.error(message, e)
                            completeResponse(StatusCodes.BadRequest,
                              Option {
                                io.ResponseRequestOutOperationResult(
                                  ReturnMessageData(code = 113, message = message),
                                  Option {
                                    List.empty[ResponseRequestOut]
                                  })
                              }
                            )
                          case e@(_: ResponseServiceDocumentNotFoundException) =>
                            val message = logTemplate(user.id, indexName, "decisionTableResource", request.method,
                              request.uri, "Requested document not found")
                            log.error(message, e)
                            completeResponse(StatusCodes.BadRequest,
                              Option {
                                io.ResponseRequestOutOperationResult(
                                  ReturnMessageData(code = 114, message = message),
                                  Option {
                                    List.empty[ResponseRequestOut]
                                  })
                              }
                            )
                          case e@(_: CircuitBreakerOpenRejection) =>
                            val message = logTemplate(user.id, indexName, "decisionTableResource", request.method,
                              request.uri, "The request the takes too much time")
                            log.error(message, e)
                            completeResponse(StatusCodes.RequestTimeout,
                              Option {
                                io.ResponseRequestOutOperationResult(
                                  ReturnMessageData(code = 115, message = message),
                                  Option {
                                    List.empty[ResponseRequestOut]
                                  })
                              }
                            )
                          case NonFatal(nonFatalE) =>
                            val message = logTemplate(user.id, indexName, "decisionTableResource", request.method,
                              request.uri, "Unable to complete the request")
                            log.error(message, e)
                            completeResponse(StatusCodes.BadRequest,
                              Option {
                                io.ResponseRequestOutOperationResult(
                                  ReturnMessageData(code = 116, message = message),
                                  Option {
                                    List.empty[ResponseRequestOut]
                                  })
                              }
                            )
                        }
                      case Success(responseValue) =>
                        if (responseValue.status.code === 200) {
                          completeResponse(StatusCodes.OK, StatusCodes.BadRequest, responseValue.responseRequestOut)
                        } else {
                          completeResponse(StatusCodes.NoContent) // no response found
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

  def decisionTableAutoCompleteRequestRoutes: Route = handleExceptions(routesExceptionHandler) {
    pathPrefix(indexRegex ~ Slash ~ "autocomplete") { indexName =>
      pathEnd {
        post {
          authenticateBasicAsync(realm = authRealm,
            authenticator = authenticator.authenticator) { user =>
            authorizeAsync(_ =>
              authenticator.hasPermissions(user, indexName, Permissions.read)) {
              extractRequest { request =>
                entity(as[AutoCompleteRequest]) {
                  autoComplete_request =>
                    val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                    onCompleteWithBreakerFuture(breaker)(autoCompleteService.autoComplete(indexName, autoComplete_request)) {
                      case Failure(e) =>
                        e match {
                          case e@(_: CircuitBreakerOpenRejection) =>
                            val message = logTemplate(user.id, indexName, "decisionTableResource", request.method,
                              request.uri, "The request the takes too much time")
                            log.error(message, e)
                            completeResponse(StatusCodes.RequestTimeout,
                              Option {
                                io.AutoCompleteResponse(0, 0, List.empty[SuggestedQuery])
                              }
                            )
                          case NonFatal(nonFatalE) =>
                            val message = logTemplate(user.id, indexName, "decisionTableResource", request.method,
                              request.uri, "Unable to complete the request")
                            log.error(message, e)
                            completeResponse(StatusCodes.BadRequest,
                              Option {
                                io.AutoCompleteResponse(0, 0, List.empty[SuggestedQuery])
                              }
                            )
                        }
                      case Success(responseValue) =>
                        completeResponse(StatusCodes.OK, StatusCodes.BadRequest, Option(responseValue))
                    }
                }
              }
            }
          }
        }
      }
    }
  }
  def decisionTableRoutes: Route = handleExceptions(routesExceptionHandler) {
    pathPrefix(indexRegex ~ Slash ~ "decisiontable") { indexName =>
      pathEnd {
        post {
          authenticateBasicAsync(realm = authRealm,
            authenticator = authenticator.authenticator) { user =>
            authorizeAsync(_ =>
              authenticator.hasPermissions(user, indexName, Permissions.write)) {
              extractRequest { request =>
                parameters("check".as[Boolean] ? true,
                  "refresh".as[RefreshPolicy.Value] ? RefreshPolicy.`false`) { (check, refreshPolicy) =>
                  entity(as[DTDocument]) { document =>
                    val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                    onCompleteWithBreakerFuture(breaker)(decisionTableService.create(
                      indexName = indexName,
                      document = document,
                      check = check,
                      refreshPolicy = refreshPolicy)) {
                      case Success(t) =>
                        completeResponse(StatusCodes.Created, StatusCodes.BadRequest, t)
                      case Failure(e) =>
                        log.error(logTemplate(user.id, indexName, "decisionTableRoutes", request.method, request.uri), e)
                        completeResponse(StatusCodes.BadRequest,
                          Option {
                            ReturnMessageData(code = 116, message = e.getMessage)
                          })
                    }
                  }
                }
              }
            }
          }
        } ~
          get {
            authenticateBasicAsync(realm = authRealm,
              authenticator = authenticator.authenticator) { user =>
              authorizeAsync(_ =>
                authenticator.hasPermissions(user, indexName, Permissions.read)) {
                extractRequest { request =>
                  parameters("id".as[String].*, "dump".as[Boolean] ? false) { (ids, dump) =>
                    if (!dump) {
                      val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                      onCompleteWithBreakerFuture(breaker)(decisionTableService.read(indexName, ids.toList)) {
                        case Success(t) =>
                          completeResponse(StatusCodes.OK, StatusCodes.BadRequest, Some(t))
                        case Failure(e) =>
                          log.error(logTemplate(user.id, indexName, "decisionTableRoutes", request.method, request.uri), e)
                          completeResponse(StatusCodes.BadRequest,
                            Option {
                              ReturnMessageData(code = 117, message = e.getMessage)
                            })
                      }
                    } else {
                      val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                      onCompleteWithBreakerFuture(breaker)(decisionTableService.getDTDocuments(indexName)) {
                        case Success(t) =>
                          completeResponse(StatusCodes.OK, StatusCodes.BadRequest, Some(t))
                        case Failure(e) =>
                          log.error(logTemplate(user.id, indexName, "decisionTableRoutes", request.method, request.uri), e)
                          completeResponse(StatusCodes.BadRequest,
                            Option {
                              ReturnMessageData(code = 118, message = e.getMessage)
                            })
                      }
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
                authenticator.hasPermissions(user, indexName, Permissions.write)) {
                extractRequest { request =>
                  parameters("id".as[String].*,
                    "refresh".as[RefreshPolicy.Value] ? RefreshPolicy.`false`) { (ids, refreshPolicy) =>
                    val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                    onCompleteWithBreakerFuture(breaker)(decisionTableService.delete(indexName, ids.toList, refreshPolicy)) {
                      case Success(t) =>
                        completeResponse(StatusCodes.OK, StatusCodes.BadRequest, t)
                      case Failure(e) =>
                        log.error(logTemplate(user.id, indexName, "decisionTableRoutes", request.method, request.uri), e)
                        completeResponse(StatusCodes.BadRequest,
                          Option {
                            ReturnMessageData(code = 119, message = e.getMessage)
                          })
                    }
                  }
                }
              }
            }
          }
      } ~
        put {
          authenticateBasicAsync(realm = authRealm,
            authenticator = authenticator.authenticator) { user =>
            authorizeAsync(_ =>
              authenticator.hasPermissions(user, indexName, Permissions.write)) {
              extractRequest { request =>
                entity(as[DTDocumentUpdate]) { document =>
                  parameters("check".as[Boolean] ? true,
                    "refresh".as[RefreshPolicy.Value] ? RefreshPolicy.`false`) { (check, refreshPolicy) =>
                    val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                    onCompleteWithBreakerFuture(breaker)(
                      decisionTableService.update(indexName = indexName,
                        document = document, check = check, refreshPolicy = refreshPolicy)) {
                      case Success(t) =>
                        completeResponse(StatusCodes.OK, StatusCodes.BadRequest, Some(t))
                      case Failure(e) =>
                        log.error(logTemplate(user.id, indexName, "decisionTableRoutes", request.method, request.uri), e)
                        completeResponse(StatusCodes.BadRequest,
                          Option {
                            ReturnMessageData(code = 120, message = e.getMessage)
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

  def decisionTableCacheLoad: Route = handleExceptions(routesExceptionHandler) {
    concat(
      pathPrefix(indexRegex ~ Slash ~ "decisiontable" ~ Slash ~ "bayescache") { indexName =>
        pathEnd {
          post {
            extractRequest { request =>
              authenticateBasicAsync(realm = authRealm, authenticator = authenticator.authenticator) { user =>
                authorizeAsync(_ => authenticator.hasPermissions(user, "admin", Permissions.write)) {
                  val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker(maxFailure = 5, callTimeout = 60.seconds, resetTimeout = 120.seconds)
                  onCompleteWithBreakerFuture(breaker)(bayesOperatorCacheService.load(indexName)) {
                    case Success(t) =>
                      val status = if(t.status) StatusCodes.OK else StatusCodes.BadRequest
                      completeResponse(status, StatusCodes.BadRequest, Option(t))
                    case Failure(e) =>
                      log.error(logTemplate(user.id, indexName, "decisionTableCacheLoad", request.method, request.uri), e)
                      completeResponse(StatusCodes.BadRequest,
                        Option {
                          ReturnMessageData(code = 120, message = e.getMessage)
                        })
                  }
                }
              }
            }
          }
        }
      }, pathPrefix(indexRegex ~ Slash ~ "decisiontable" ~ Slash ~ "bayescache"  ~ Slash ~ "async") { indexName =>
        pathEnd {
          post {
            extractRequest { request =>
              authenticateBasicAsync(realm = authRealm, authenticator = authenticator.authenticator) { user =>
                authorizeAsync(_ => authenticator.hasPermissions(user, "admin", Permissions.write)) {
                  val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                  onCompleteWithBreakerFuture(breaker)(bayesOperatorCacheService.loadAsync(indexName)) {
                    case Success(t) =>
                      val status = if(t.status) StatusCodes.OK else StatusCodes.BadRequest
                      completeResponse(status, StatusCodes.BadRequest, Option(t))
                    case Failure(e) =>
                      log.error(logTemplate(user.id, indexName, "decisionTableCacheLoadAsync", request.method, request.uri), e)
                      completeResponse(StatusCodes.BadRequest,
                        Option {
                          ReturnMessageData(code = 120, message = e.getMessage)
                        })
                  }
                }
              }
            }
          }
        }
      })
  }

  def decisionTableRoutesBulkDeleteRoutes: Route = handleExceptions(routesExceptionHandler) {
    pathPrefix(indexRegex ~ Slash ~ "decisiontable" ~ Slash ~ "delete") { indexName =>
      pathEnd {
        post {
          authenticateBasicAsync(realm = authRealm,
            authenticator = authenticator.authenticator) { user =>
            authorizeAsync(_ =>
              authenticator.hasPermissions(user, indexName, Permissions.write)) {
              extractRequest { request =>
                entity(as[DocsIds]) { ids =>
                  val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                  onCompleteWithBreakerFuture(breaker)(
                    decisionTableService.delete(indexName,
                      ids.ids, RefreshPolicy.`wait_for`)
                  ) {
                    case Success(t) =>
                      completeResponse(StatusCodes.OK, StatusCodes.BadRequest, t)
                    case Failure(e) =>
                      log.error(logTemplate(user.id,
                        indexName, "decisionTableRoutesBulkDeleteRoutes", request.method, request.uri), e)
                      completeResponse(StatusCodes.BadRequest,
                        Option {
                          ReturnMessageData(code = 121, message = e.getMessage)
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

