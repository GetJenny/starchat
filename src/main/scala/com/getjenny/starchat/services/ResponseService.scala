package com.getjenny.starchat.services

/**
 * Created by Angelo Leto <angelo@getjenny.com> on 01/07/16.
 */

import java.security.SecureRandom

import akka.event.{Logging, LoggingAdapter}
import com.getjenny.analyzer.analyzers._
import com.getjenny.analyzer.expressions.{AnalyzersDataInternal, Context, Result}
import com.getjenny.starchat.SCActorSystem
import com.getjenny.starchat.entities.io._
import com.getjenny.starchat.services.actions._
import com.getjenny.starchat.services.esclient.DecisionTableElasticClient
import scalaz.Scalaz._
import spray.json.{JsString, _}

import scala.collection.immutable.Map
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
  private[this] val matchTemplateRegex = "%([^%]*)%"

  private[this] def extractRequiredStarChatVarNames(input: String): Set[StarChatVariables.Value] = {
    StarChatVariables.values.map(name => (name, input contains "%" + name.toString + "%"))
      .filter(_._2).map(_._1)
  }

  private[this] def extractRequiredStarChatVarNames(input: Seq[JsObject]): Set[StarChatVariables.Value] = {
    input.map(jo => extractRequiredStarChatVarNames(jo.toString))
      .fold(Set.empty[StarChatVariables.Value])((acc, cur) => acc ++ cur)
  }

  private[this] def extractSCVariables(indexName: String,
                                       input: Seq[JsObject],
                                       request: ResponseRequestIn): Map[String, String] = {
    extractSCVariables(indexName = indexName,
      variables = extractRequiredStarChatVarNames(input),
      request = request
    )
  }

  private[this] def extractSCVariables(indexName: String,
                                       variables: Set[StarChatVariables.Value],
                                       request: ResponseRequestIn): Map[String, String] = {
    val conversationLogsService: ConversationLogsService.type = ConversationLogsService
    variables.map {
      case StarChatVariables.GJ_CONV_FEEDBACK_SCORE =>
        val ids = DocsIds(ids = List(request.conversationId))
        val feedbackConvScore =
          conversationLogsService.conversations(indexName, ids).conversations.headOption match {
            case Some(value) =>
              value.docs.map(c =>
                c.annotations match {
                  case Some(annotations) =>
                    annotations.feedbackConvScore.getOrElse(-1.0d)
                  case _ => -1.0d
                }).headOption.getOrElse(-1.0d)
            case _ => -1.0d
          }
        (StarChatVariables.GJ_CONV_FEEDBACK_SCORE.toString, feedbackConvScore.toString)
      case StarChatVariables.GJ_CONVERSATION_ID =>
        (StarChatVariables.GJ_CONVERSATION_ID.toString, request.conversationId)
      case StarChatVariables.GJ_LAST_USER_INPUT_TEXT =>
        (StarChatVariables.GJ_LAST_USER_INPUT_TEXT.toString,
          request.userInput.getOrElse(ResponseRequestInUserInput()).text.getOrElse(""))
      case StarChatVariables.GJ_CONVERSATION_V1 =>
        val ids = DocsIds(ids = List(request.conversationId))
        val conversation =
          conversationLogsService.conversations(indexName, ids).conversations.headOption match {
            case Some(value) => value.docs.map(c =>
              c.coreData match {
                case Some(core) =>
                  "Question(" + c.indexInConversation + "): " + core.question.getOrElse("EMPTY") + "\r\n" +
                    "Answer(" + c.indexInConversation + "): " + core.answer.getOrElse("EMPTY") + "\r\n"
                case _ => ""
              }).filter(_.nonEmpty).mkString("---\r\n")
            case _ => "EMPTY CONVERSATION"
          }
        (StarChatVariables.GJ_CONVERSATION_V1.toString, conversation)
      case _ =>
        throw ResponseServiceException("Requested an unknown or invalid variable")
    }.toMap
  }

  private[this] def executeAction(indexName: String, document: ResponseRequestOut, query: String, request: ResponseRequestIn): List[ResponseRequestOut] = {
    val isAnalyzerInAction = document.action.startsWith(DtAction.analyzerActionPrefix)
    if (document.action.startsWith(DtAction.actionPrefix) || isAnalyzerInAction) {
      val actionVariables = document.data
      val actionResult = DtAction(indexName,
        document.state, document.action,
        document.actionInput, actionVariables,
        query)

      // error in execution and negative results lead both to failureValue
      val state = if (actionResult.success && actionResult.code === 0)
        document.successValue
      else
        document.failureValue

      val completeData = document.data ++ actionResult.data
      if (state.isEmpty || state === document.state) { // avoiding recursive state fetch.
        List(document.copy(
          actionResult = Option(actionResult),
          data = completeData,
          score = if (actionResult.success) 1.0 else 0)
        )
      } else {
        getNextResponse(indexName,
          request.copy(
            traversedStates = Some(document.traversedStates),
            userInput = request.userInput,
            data = Option(completeData),
            evaluationClass = None,
            maxResults = None,
            state = Some(List(state))
          )
        ).responseRequestOut.getOrElse(List.empty)
      }
    } else {
      List(document)
    }
  }

  def getNextResponse(indexName: String, request: ResponseRequestIn): ResponseRequestOutOperationResult = {
    val evaluationClass = request.evaluationClass match {
      case Some(c) => c
      case _ => "default"
    }

    if (!AnalyzerService.analyzersMap.contains(indexName)) {
      val message = "Decision table not ready for index(" + indexName + "), please retry later"
      log.debug(message)
      throw ResponseServiceDTNotLoadedException(message)
    }

    val userText: String = request.userInput match {
      case Some(t) =>
        t.text.getOrElse("")
      case _ => ""
    }

    val conversationId: String = request.conversationId

    val variables: Map[String, String] = request.data.getOrElse(Map.empty[String, String])

    val traversedStates: Vector[String] = request.traversedStates.getOrElse(Vector.empty[String])
    val traversedStatesCount: Map[String, Int] =
      traversedStates.foldLeft(Map.empty[String, Int])((map, word) => map + (word -> (map.getOrElse(word, 0) + 1)))

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
      searchAlgorithm = request.searchAlgorithm, evaluationClass = request.evaluationClass)
    val searchResult = decisionTableService.searchDtQueries(indexName, analyzerEvaluateRequest)
    val analyzersInternalData = decisionTableService.resultsToMap(searchResult)
    val searchResAnalyzers = AnalyzersDataInternal(
      context = Context(indexName = indexName, stateName = ""), // stateName is not important here
      extractedVariables = variables,
      traversedStates = traversedStates,
      data = analyzersInternalData)

    val (evaluationList, reqState) = request.state match {
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
    val maxResults: Int = request.maxResults.getOrElse(2)
    val threshold: Double = request.threshold.getOrElse(elasticClient.getNextResponseThreshold)

    // attach search res analyzer result to the states
    val analyzersEvalData: Map[String, Result] = evaluationList
      .filter { case (_, runtimeAnalyzerItem) => runtimeAnalyzerItem.evaluationClass === evaluationClass }
      .filter { case (_, runtimeAnalyzerItem) => runtimeAnalyzerItem.analyzer.build === true }
      .filter { case (stateName, runtimeAnalyzerItem) =>
        val traversedStateCount = traversedStatesCount.getOrElse(stateName, 0)
        val maxStateCount = runtimeAnalyzerItem.maxStateCounter
        maxStateCount === 0 || traversedStateCount < maxStateCount // skip states already evaluated too much times
      }.map { case (stateName, runtimeAnalyzerItem) =>
      val analyzerEvaluation = runtimeAnalyzerItem.analyzer.analyzer match {
        case Some(starchatAnalyzer) =>
          val analyzersDataInternal: AnalyzersDataInternal = searchResAnalyzers.copy(
            context = Context(indexName = indexName, stateName = stateName)
          )
          Try(starchatAnalyzer.evaluate(userText, data = analyzersDataInternal)) match {
            case Success(evalRes: Result) =>
              log.debug("ResponseService: Evaluation of State({}) Query({}) Score({})",
                stateName, userText, evalRes.toString)
              evalRes
            case Failure(e) =>
              val message = "ResponseService: Evaluation of State(" + stateName + ") : " + e.getMessage
              log.error(message)
              throw AnalyzerEvaluationException(message, e)
          }
        case _ =>
          log.debug("ResponseService: analyzer is None ({})", stateName)
          val score = if (reqState) threshold else 0.0 // if requested the threshold is matched by default
          Result(score = score, data = searchResAnalyzers)
      }
      val stateId = stateName
      (stateId, analyzerEvaluation)
    }.filter { case (_, analyzerEvaluation) => analyzerEvaluation.score >= threshold }
      .sortWith {
        case ((_, analyzerEvaluation1), (_, analyzerEvaluation2)) =>
          analyzerEvaluation1.score > analyzerEvaluation2.score
      }.take(maxResults).toMap

    if (analyzersEvalData.isEmpty) {
      throw ResponseServiceNoResponseException(
        "The analyzers evaluation list is empty, threshold could be too high")
    }

    val dtDocumentsList = analyzersEvalData.keys.map(state => {
      AnalyzerService.analyzersMap(indexName).analyzerMap.get(state) match {
        case Some(item) =>
          val evaluationRes: Result = analyzersEvalData(state)
          val maxStateCount: Int = item.maxStateCounter
          val analyzer: String = item.analyzer.declaration
          val stateData: Map[String, String] = item.stateData

          val mergedVariables = searchResAnalyzers.extractedVariables ++ evaluationRes.data.extractedVariables
          val randomizedBubbleValue = randomizeBubble(item.bubble)
          val bubble = replaceTemplates(randomizedBubbleValue, mergedVariables)
          val action = replaceTemplates(item.action, mergedVariables) //FIXME: action shouldn't contain templates

          val onDemandStarChatVariables = mergedVariables ++ extractSCVariables(indexName, item.actionInput, request)
          val actionInput = replaceTemplates(item.actionInput, onDemandStarChatVariables)

          val cleanedData = mergedVariables.filter { case (key, _) => !(key matches "\\A__temp__.*") }

          val traversedStatesUpdated: Vector[String] = traversedStates ++ Vector(state)
          ResponseRequestOut(conversationId = conversationId,
            state = state,
            maxStateCount = maxStateCount,
            traversedStates = traversedStatesUpdated,
            analyzer = analyzer,
            bubble = bubble,
            action = action,
            data = cleanedData,
            actionInput = actionInput,
            stateData = stateData,
            successValue = item.successValue,
            failureValue = item.failureValue,
            score = evaluationRes.score)
        case _ => throw ResponseServiceDocumentNotFoundException("DTDocument not in runtime map: " + state)
      }
    }).toList.sortWith(_.score > _.score)
      .flatMap { document =>
        executeAction(indexName, document = document, userText, request.copy(threshold = Option(threshold)))
      }

    if (dtDocumentsList.isEmpty) {
      throw ResponseServiceNoResponseException(
        "The analyzers evaluation list is empty, it is possible that no states were found for the evaluationClass")
    }

    ResponseRequestOutOperationResult(ReturnMessageData(200, ""), Some(dtDocumentsList))
  }

  private[this] def escapeJson(input: String): String = JsString(input).toString.replaceAll("^\"|\"$", "")

  private[this] def replaceTemplates(input: String, values: Map[String, String]): String = {
    val substitutedString = values.foldLeft(input) {
      case (b, (k, v)) =>
        b.replaceAllLiterally("%" + k + "%", escapeJson(v))
    }
    clearNonSubstitutedTemplates(substitutedString)
  }

  private[this] def replaceTemplates(input: Seq[JsObject],
                                     values: Map[String, String]): Seq[JsObject] = {
    input.map { item =>
      val jsonObjString = values.foldLeft(item.toString) { case (acc, (replKey, replValue)) =>
        acc.replaceAllLiterally("%" + replKey + "%", escapeJson(replValue))
      }
      clearNonSubstitutedTemplates(jsonObjString)
        .parseJson
        .asJsObject
    }
  }

  private[this] def clearNonSubstitutedTemplates(input: String): String = {
    input.replaceAll(matchTemplateRegex, "")
  }

  private[this] def randomizeBubble(bubble: String): String = {
    val splittedBubble = bubble.split('|')
    if (splittedBubble.length > 1) {
      val secRandom = new SecureRandom
      val randomIdx = secRandom.nextInt(splittedBubble.length)
      splittedBubble(randomIdx)
    } else {
      bubble
    }
  }

}
