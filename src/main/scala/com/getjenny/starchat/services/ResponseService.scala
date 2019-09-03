package com.getjenny.starchat.services

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 01/07/16.
  */

import akka.event.{Logging, LoggingAdapter}
import com.getjenny.analyzer.analyzers._
import com.getjenny.analyzer.expressions.{AnalyzersDataInternal, Context, Result}
import com.getjenny.starchat.SCActorSystem
import com.getjenny.starchat.entities.{ResponseRequestOut, _}
import com.getjenny.starchat.services.actions._
import com.getjenny.starchat.services.esclient.DecisionTableElasticClient
import scalaz.Scalaz._

import scala.collection.immutable.Map
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

case class ResponseServiceException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)

case class ResponseServiceDocumentNotFoundException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)

case class ResponseServiceNoResponseException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)

case class ResponseServiceDTNotLoadedException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)

/**
  * Implements response functionalities
  */
object ResponseService extends AbstractDataService {
  override val elasticClient: DecisionTableElasticClient.type = DecisionTableElasticClient
  private[this] val log: LoggingAdapter = Logging(SCActorSystem.system, this.getClass.getCanonicalName)
  private[this] val decisionTableService: DecisionTableService.type = DecisionTableService

  private[this] def executeAction(indexName: String, document: ResponseRequestOut): ResponseRequestOut = {
    if (document.action.startsWith(DtAction.actionPrefix)) {
      val res = DtAction(indexName, document.state, document.action, document.action_input)
      document.copy(actionResult = Some{res})
    } else {
      document
    }
  }

  def getNextResponse(indexName: String, request: ResponseRequestIn): Future[ResponseRequestOutOperationResult] = Future {

    if(! AnalyzerService.analyzersMap.contains(indexName)) {
      val message = "Decision table not ready for index(" + indexName + "), please retry later"
      log.debug(message)
      throw ResponseServiceDTNotLoadedException(message)
    }

    val userText: String = request.user_input match {
      case Some(t) =>
        t.text.getOrElse("")
      case _ => ""
    }

    val conversationId: String = request.conversation_id

    val variables : Map[String, String] = request.values match {
      case Some(vars) => vars.data.getOrElse(Map[String, String]())
      case _ => Map.empty[String, String]
    }

    val traversedStates: Vector[String] = request.traversed_states.getOrElse(Vector.empty[String])
    val traversedStatesCount: Map[String, Int] =
      traversedStates.foldLeft(Map.empty[String, Int])((map, word) => map + (word -> (map.getOrElse(word,0) + 1)))

    // refresh last_used timestamp
    Try(AnalyzerService.analyzersMap(indexName).lastEvaluationTimestamp = System.currentTimeMillis) match {
      case Success(_) => ;
      case Failure(e) =>
        val message = "could not update the last evaluation timestamp for: " + indexName
        log.error(message)
        throw ResponseServiceException(message, e)
    }

    // prepare search result for search analyzer
    val analyzerEvaluateRequest = AnalyzerEvaluateRequest(analyzer = "", query = userText, data = None,
      searchAlgorithm = request.search_algorithm)
    val searchResAnalyzers = decisionTableService
      .searchDtQueries(indexName, analyzerEvaluateRequest).map(searchRes => { // map to wait the future value
      val analyzersInternalData = decisionTableService.resultsToMap(searchRes)
      AnalyzersDataInternal(
        context = Context(indexName = indexName, stateName = ""), // stateName is not important here
        extractedVariables = variables,
        traversedStates = traversedStates,
        data = analyzersInternalData)
    })

    val return_value: Option[String] = request.values
      .getOrElse(ResponseRequestInValues(None, None))
      .return_value
    val state: Option[List[String]] = if(return_value.isDefined) {
      if(return_value.getOrElse("") === "") {
        None
      } else {
        Some{List(return_value.getOrElse(""))}
      }
    } else {
      None
    }
    val returnState: Future[ResponseRequestOutOperationResult] = searchResAnalyzers.map( data => {
      val maxResults: Int = request.max_results.getOrElse(2)
      val threshold: Double = request.threshold.getOrElse(0.0d)
      val (evaluationList, reqState) = state match {
        case Some(states) =>
          (
            states.map { state =>
              (state,
                AnalyzerService.analyzersMap(indexName).analyzerMap.get(state) match {
                  case Some(v) => v
                  case _ =>
                    throw ResponseServiceDocumentNotFoundException("Requested state not found: state(" + state + ")")
                }
              )
            }, true
          )
        case _ =>
          (AnalyzerService.analyzersMap(indexName).analyzerMap.toList, false)
      }

      val analyzersEvalData: Map[String, Result] = evaluationList
        .filter { case (_, runtimeAnalyzerItem) => runtimeAnalyzerItem.analyzer.build === true }
        .filter { case (stateName, runtimeAnalyzerItem) =>
          val traversedStateCount = traversedStatesCount.getOrElse(stateName, 0)
          val maxStateCount = runtimeAnalyzerItem.maxStateCounter
          maxStateCount === 0 ||
            traversedStateCount < maxStateCount // skip states already evaluated too much times
        }.map { case (stateName, runtimeAnalyzerItem) =>
        val analyzerEvaluation = runtimeAnalyzerItem.analyzer.analyzer match {
          case Some(starchatAnalyzer) =>
            val analyzersDataInternal: AnalyzersDataInternal =  data.copy(
              context = Context(indexName = indexName, stateName = stateName)
            )
            Try(starchatAnalyzer.evaluate(userText, data = analyzersDataInternal)) match {
              case Success(evalRes) =>
                log.debug("ResponseService: Evaluation of State(" +
                  stateName + ") Query(" + userText + ") Score(" + evalRes.toString + ")")
                evalRes
              case Failure(e) =>
                val message = "ResponseService: Evaluation of State(" + stateName + ") : " + e.getMessage
                log.error(message)
                throw AnalyzerEvaluationException(message, e)
            }
          case _ =>
            val message = "ResponseService: analyzer is None (" + stateName + ")"
            log.debug(message)
            val score = if(reqState) threshold else 0.0
            Result(score = score, data = data)
        }
        val stateId = stateName
        (stateId, analyzerEvaluation)
      }.filter { case (_, analyzerEvaluation) => analyzerEvaluation.score >= threshold }
        .sortWith {
          case ((_, analyzerEvaluation1), (_, analyzerEvaluation2)) =>
            analyzerEvaluation1.score > analyzerEvaluation2.score
        }.take(maxResults).toMap

      if(analyzersEvalData.isEmpty) {
        throw ResponseServiceNoResponseException(
          "The analyzers evaluation list is empty, threshold could be too high")
      }

      decisionTableService.read(indexName, analyzersEvalData.keys.toList).map { docResults =>
        val dtDocumentsList = docResults.hits.par.map { item =>
          val doc: DTDocument = item.document
          val state = doc.state
          val evaluationRes: Result = analyzersEvalData(state)
          val maxStateCount: Int = doc.max_state_count
          val analyzer: String = doc.analyzer
          var bubble: String = doc.bubble
          val action: String = doc.action
          var actionInput: Map[String, String] = doc.action_input
          val stateData: Map[String, String] = doc.state_data

          for ((key, value) <- data.extractedVariables) {
            bubble = bubble.replaceAll("%" + key + "%", value)
            actionInput = doc.action_input map { case (ki, vi) =>
              val newValue: String = vi.replaceAll("%" + key + "%", value)
              (ki, newValue)
            }
          }

          for ((key, value) <- evaluationRes.data.extractedVariables) {
            bubble = bubble.replaceAll("%" + key + "%", value)
            actionInput = doc.action_input map { case (ki, vi) =>
              val newValue: String = vi.replaceAll("%" + key + "%", value)
              (ki, newValue)
            }
          }

          val cleanedData =
            data.extractedVariables ++
              evaluationRes.data.extractedVariables
                .filter { case (key, _) => !(key matches "\\A__temp__.*") }

          val traversedStatesUpdated: Vector[String] = traversedStates ++ Vector(state)
          val responseItem: ResponseRequestOut = ResponseRequestOut(conversation_id = conversationId,
            state = state,
            max_state_count = maxStateCount,
            traversed_states = traversedStatesUpdated,
            analyzer = analyzer,
            bubble = bubble,
            action = action,
            data = cleanedData,
            action_input = actionInput,
            state_data = stateData,
            success_value = doc.success_value,
            failure_value = doc.failure_value,
            score = evaluationRes.score)
          responseItem
        }.toList.sortWith(_.score > _.score).map { document =>
          executeAction(indexName, document = document)
        }

        if(dtDocumentsList.isEmpty) {
          throw ResponseServiceNoResponseException(
            "The analyzers evaluation list is empty, it is possible that no states were found for the evaluationClass")
        }

        ResponseRequestOutOperationResult(ReturnMessageData(200, ""),
          Option {dtDocumentsList})
      }

    }).flatten
    Await.result(returnState, 30.second)
  }
}
