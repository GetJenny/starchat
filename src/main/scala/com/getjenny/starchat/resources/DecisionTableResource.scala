package com.getjenny.starchat.resources

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 27/06/16.
  */


import akka.http.javadsl.server.CircuitBreakerOpenRejection
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.pattern.CircuitBreaker
import com.getjenny.analyzer.analyzers.AnalyzerEvaluationException
import com.getjenny.starchat.entities._
import com.getjenny.starchat.routing._
import com.getjenny.starchat.services._
import scalaz.Scalaz._

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.control.NonFatal
import scala.util.matching.Regex
import scala.util.{Failure, Success}

trait DecisionTableResource extends StarChatResource {

  private[this] val decisionTableService: DecisionTableService.type = DecisionTableService
  private[this] val analyzerService: AnalyzerService.type = AnalyzerService
  private[this] val responseService: ResponseService.type = ResponseService
  private[this] val dtReloadService: DtReloadService.type = DtReloadService
  private[this] val fileTypeRegex: Regex = ("^(csv|json)$").r

  def decisionTableRoutesAllRoutes: Route = handleExceptions(routesExceptionHandler) {
    pathPrefix(indexRegex ~ Slash ~ "decisiontable" ~ Slash ~ "all") { indexName =>
      pathEnd {
        delete {
          authenticateBasicAsync(realm = authRealm,
            authenticator = authenticator.authenticator) { user =>
            authorizeAsync(_ =>
              authenticator.hasPermissions(user, indexName, Permissions.write)) {
              val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
              onCompleteWithBreaker(breaker)(
                Future{decisionTableService.deleteAll(indexName)}
              ) {
                case Success(t) =>
                  completeResponse(StatusCodes.OK, StatusCodes.BadRequest, t)
                case Failure(e) =>
                  log.error("index(" + indexName + ") route=decisionTableRoutes method=DELETE : " + e.getMessage)
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

  def decisionTableUploadFilesRoutes: Route = handleExceptions(routesExceptionHandler) {
    pathPrefix(indexRegex ~ Slash ~ "decisiontable" ~ Slash ~ "upload" ~ Slash ~ fileTypeRegex) { (indexName, fileType) =>
      pathEnd {
        post {
          authenticateBasicAsync(realm = authRealm, authenticator = authenticator.authenticator) { user =>
            authorizeAsync(_ =>
              authenticator.hasPermissions(user, indexName, Permissions.write)) {
              storeUploadedFile(fileType, tempDestination("." + fileType)) {
                case (_, file) =>
                  val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker(callTimeout = 60.seconds)
                  onCompleteWithBreaker(breaker)(
                    if (fileType == "csv") {
                      decisionTableService.indexCSVFileIntoDecisionTable(indexName, file, 0)
                    } else if (fileType == "json") {
                      decisionTableService.indexJSONFileIntoDecisionTable(indexName, file)
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
                      log.error("index(" + indexName + ") route=decisionTableUploadCSVRoutes method=POST: " + e.getMessage)
                      if (file.exists()) {
                        file.delete()
                      }
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

  def decisionTableAsyncReloadRoutes: Route = handleExceptions(routesExceptionHandler) {
    pathPrefix(indexRegex ~ Slash ~ "decisiontable" ~ Slash ~ "analyzer" ~ Slash ~ "async") { indexName =>
      pathEnd {
        post {
          authenticateBasicAsync(realm = authRealm,
            authenticator = authenticator.authenticator) { user =>
            authorizeAsync(_ =>
              authenticator.hasPermissions(user, indexName, Permissions.write)) {
              val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
              onCompleteWithBreaker(breaker)(dtReloadService.updateDTReloadTimestamp(indexName, refresh = 1)) {
                case Success(t) =>
                  completeResponse(StatusCodes.Accepted, StatusCodes.BadRequest, Option {
                    t
                  })
                case Failure(e) =>
                  log.error("index(" + indexName + ") route=decisionTableAsyncReloadRoutes method=POST: " + e.getMessage)
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

  def decisionTableAnalyzerRoutes: Route = handleExceptions(routesExceptionHandler) {
    pathPrefix(indexRegex ~ Slash ~ "decisiontable" ~ Slash ~ "analyzer") { indexName =>
      pathEnd {
        get {
          authenticateBasicAsync(realm = authRealm,
            authenticator = authenticator.authenticator) { user =>
            authorizeAsync(_ =>
              authenticator.hasPermissions(user, indexName, Permissions.write)) {
              val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
              onCompleteWithBreaker(breaker)(analyzerService.getDTAnalyzerMap(indexName)) {
                case Success(t) =>
                  completeResponse(StatusCodes.OK, StatusCodes.BadRequest, Option {
                    t
                  })
                case Failure(e) =>
                  log.error("index(" + indexName + ") route=decisionTableAnalyzerRoutes method=GET: " + e.getMessage)
                  completeResponse(StatusCodes.BadRequest,
                    Option {
                      ReturnMessageData(code = 106, message = e.getMessage)
                    })
              }
            }
          }
        } ~
          post {
            authenticateBasicAsync(realm = authRealm,
              authenticator = authenticator.authenticator) { user =>
              authorizeAsync(_ =>
                authenticator.hasPermissions(user, indexName, Permissions.write)) {
                parameters("propagate".as[Boolean] ? true,
                  "incremental".as[Boolean] ? true) { (propagate, incremental) =>
                  val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker(callTimeout = 120.seconds)
                  onCompleteWithBreaker(breaker)(analyzerService.loadAnalyzers(indexName = indexName,
                    incremental = incremental, propagate = propagate)) {
                    case Success(t) =>
                      completeResponse(StatusCodes.OK, StatusCodes.BadRequest, Option {
                        t
                      })
                    case Failure(e) =>
                      log.error("index(" + indexName +
                        ") route=decisionTableAnalyzerRoutes method=POST: " + e.getMessage)
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

  def decisionTableSearchRoutes: Route = handleExceptions(routesExceptionHandler) {
    pathPrefix(indexRegex ~ Slash ~ "decisiontable" ~ Slash ~ "search") { indexName =>
      pathEnd {
        post {
          authenticateBasicAsync(realm = authRealm,
            authenticator = authenticator.authenticator) { user =>
            authorizeAsync(_ =>
              authenticator.hasPermissions(user, indexName, Permissions.read)) {
              entity(as[DTDocumentSearch]) { docsearch =>
                val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                onCompleteWithBreaker(breaker)(decisionTableService.search(indexName, docsearch)) {
                  case Success(t) =>
                    completeResponse(StatusCodes.OK, StatusCodes.BadRequest, Option {
                      t
                    })
                  case Failure(e) =>
                    log.error("index(" + indexName + ") route=decisionTableSearchRoutes method=POST: " + e.getMessage)
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

  def decisionTableResponseRequestRoutes: Route = handleExceptions(routesExceptionHandler) {
    pathPrefix(indexRegex ~ Slash ~ "get_next_response") { indexName =>
      pathEnd {
        post {
          authenticateBasicAsync(realm = authRealm,
            authenticator = authenticator.authenticator) { user =>
            authorizeAsync(_ =>
              authenticator.hasPermissions(user, indexName, Permissions.read)) {
              entity(as[ResponseRequestIn]) {
                response_request =>
                  val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                  onCompleteWithBreaker(breaker)(responseService.getNextResponse(indexName, response_request)) {
                    case Failure(e) =>
                      e match {
                        case rsDtNotLoadedE: ResponseServiceDTNotLoadedException =>
                          completeResponse(StatusCodes.ResetContent,
                            Option {
                              ResponseRequestOutOperationResult(
                                ReturnMessageData(code = 109, message = rsDtNotLoadedE.getMessage),
                                Option {
                                  List.empty[ResponseRequestOut]
                                })
                            }
                          )
                        case e@(_: ResponseServiceNoResponseException) =>
                          val message = "index(" + indexName + ") DecisionTableResource: " +
                            "No response: " + e.getMessage
                          log.info(message = message)
                          completeResponse(StatusCodes.NoContent)
                        case e@(_: AnalyzerEvaluationException) =>
                          val message = "index(" + indexName + ") DecisionTableResource: " +
                            "Unable to complete the request, due to analyzer: " + e.getMessage
                          log.error(message = message)
                          completeResponse(StatusCodes.BadRequest,
                            Option {
                              ResponseRequestOutOperationResult(
                                ReturnMessageData(code = 110, message = message),
                                Option {
                                  List.empty[ResponseRequestOut]
                                })
                            }
                          )
                        case e@(_: ResponseServiceDocumentNotFoundException) =>
                          val message = "index(" + indexName + ") DecisionTableResource: " +
                            "Requested document not found: " + e.getMessage
                          log.error(message = message)
                          completeResponse(StatusCodes.BadRequest,
                            Option {
                              ResponseRequestOutOperationResult(
                                ReturnMessageData(code = 111, message = message),
                                Option {
                                  List.empty[ResponseRequestOut]
                                })
                            }
                          )
                        case e@(_: CircuitBreakerOpenRejection) =>
                          val message = "index(" + indexName + ") DecisionTableResource: " +
                            "The request the takes too much time: " + e.getMessage +
                            " : stacktrace(" + e.getStackTrace.map(x => x.toString).mkString(";") + ")"
                          log.error(message = message)
                          completeResponse(StatusCodes.RequestTimeout,
                            Option {
                              ResponseRequestOutOperationResult(
                                ReturnMessageData(code = 112, message = message),
                                Option {
                                  List.empty[ResponseRequestOut]
                                })
                            }
                          )
                        case NonFatal(nonFatalE) =>
                          val message = "index(" + indexName + ") DecisionTableResource: " +
                            "Unable to complete the request: " + nonFatalE.getMessage +
                            " : stacktrace(" + nonFatalE.getStackTrace.map(x => x.toString).mkString(";") + ")"
                          log.error(message = message)
                          completeResponse(StatusCodes.BadRequest,
                            Option {
                              ResponseRequestOutOperationResult(
                                ReturnMessageData(code = 113, message = message),
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

  def decisionTableRoutes: Route = handleExceptions(routesExceptionHandler) {
    pathPrefix(indexRegex ~ Slash ~ "decisiontable") { indexName =>
      pathEnd {
        post {
          authenticateBasicAsync(realm = authRealm,
            authenticator = authenticator.authenticator) { user =>
            authorizeAsync(_ =>
              authenticator.hasPermissions(user, indexName, Permissions.write)) {
              parameters("refresh".as[Int] ? 0) { refresh =>
                entity(as[DTDocument]) { document =>
                  val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                  onCompleteWithBreaker(breaker)(decisionTableService.createFuture(indexName, document, refresh)) {
                    case Success(t) =>
                      completeResponse(StatusCodes.Created, StatusCodes.BadRequest, t)
                    case Failure(e) =>
                      log.error("index(" + indexName + ") route=decisionTableRoutes method=POST: " + e.getMessage)
                      completeResponse(StatusCodes.BadRequest,
                        Option {
                          ReturnMessageData(code = 100, message = e.getMessage)
                        })
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
                parameters("id".as[String].*, "dump".as[Boolean] ? false) { (ids, dump) =>
                  if (!dump) {
                    val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                    onCompleteWithBreaker(breaker)(decisionTableService.read(indexName, ids.toList)) {
                      case Success(t) =>
                        completeResponse(StatusCodes.OK, StatusCodes.BadRequest, Option {
                          t
                        })
                      case Failure(e) =>
                        log.error("index(" + indexName + ") route=decisionTableRoutes method=GET: " + e.getMessage)
                        completeResponse(StatusCodes.BadRequest,
                          Option {
                            ReturnMessageData(code = 101, message = e.getMessage)
                          })
                    }
                  } else {
                    val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                    onCompleteWithBreaker(breaker)(decisionTableService.getDTDocuments(indexName)) {
                      case Success(t) =>
                        completeResponse(StatusCodes.OK, StatusCodes.BadRequest, Option {
                          t
                        })
                      case Failure(e) =>
                        log.error("index(" + indexName + ") route=decisionTableRoutes method=GET: " + e.getMessage)
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
                authenticator.hasPermissions(user, indexName, Permissions.write)) {
                parameters("id".as[String].*, "refresh".as[Int] ? 0) { (ids, refresh) =>
                  val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                  onCompleteWithBreaker(breaker)(
                    Future { decisionTableService.delete(indexName, ids.toList, refresh) }
                  ) {
                    case Success(t) =>
                      completeResponse(StatusCodes.OK, StatusCodes.BadRequest, t)
                    case Failure(e) =>
                      log.error("index(" + indexName + ") route=decisionTableRoutes method=DELETE : " + e.getMessage)
                      completeResponse(StatusCodes.BadRequest,
                        Option {
                          ReturnMessageData(code = 104, message = e.getMessage)
                        })
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
              entity(as[DTDocumentUpdate]) { update =>
                parameters("refresh".as[Int] ? 0) { refresh =>
                  val breaker: CircuitBreaker = StarChatCircuitBreaker.getCircuitBreaker()
                  onCompleteWithBreaker(breaker)(decisionTableService.update(indexName, update, refresh)) {
                    case Success(t) =>
                      completeResponse(StatusCodes.OK, StatusCodes.BadRequest, Option {
                        t
                      })
                    case Failure(e) =>
                      log.error("index(" + indexName + ") route=decisionTableRoutes method=PUT : " + e.getMessage)
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

