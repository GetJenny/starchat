package com.getjenny.starchat.analyzer.atoms

/**
  * Created by Andrea Collamati <andrea@getjenny.com> on 07/09/2019.
  */

import com.getjenny.analyzer.atoms.{AbstractAtomic, ExceptionAtomic}
import com.getjenny.analyzer.expressions.{AnalyzersDataInternal, Result}
import com.getjenny.starchat.entities._
import com.getjenny.starchat.services._
import scalaz.Scalaz._

import scala.util.matching.Regex

class KeywordAtomic2(arguments: List[String], restrictedArgs: Map[String, String]) extends AbstractAtomic {

  val atomName: String = "keyword2"

  val atomArgument: String = arguments.headOption match {
    case Some(t) => t
    case _ =>
      throw ExceptionAtomic(atomName + " requires as argument a keyword or a regex")
  }


  private[this] val argumentIsSingleWord: Boolean = {
    val specialCharactersAndSpaces: String = "[!@#$%^&*(),.?\":{}|\\[\\]\\\\<>]"
    specialCharactersAndSpaces.r.findFirstMatchIn(atomArgument).isEmpty && !atomArgument.contains(" ")
  }

  private[this] val argumentIsSingleStartsWithExpression: Boolean = {
    atomArgument.count(_ === '*') === 1 && !atomArgument.contains(" ") && atomArgument.endsWith("*")
  }

  // regular expression used to understand if the user query contains the keyword/regex specified in the atom argument
  private[this] val rxToBeMatched: Regex = {"""(?ui)\b""" + atomArgument.replace("*", """\w*""") + """\b"""}.r

  override def toString: String = atomName + "(\"" + atomArgument + "\")"

  val isEvaluateNormalized: Boolean = true
  override val matchThreshold: Double = 0.0

  val decisionTableService: DecisionTableService.type = DecisionTableService
  val conversationLogsService: ConversationLogsService.type = ConversationLogsService

  // Keyword Frequency Functions
  private[this] def fractionOfQueriesWithWordOfState(indexName: String, word: String, state: String): Double = {
    // get keyword freq in queries associated to the current state
    //TODO: to be optimized
    DecisionTableService.wordFrequenciesInQueriesByState(indexName).find(item => item.state === state) match {
      case Some(t) =>
        t.wordFreqs.get(word) match {
          case Some(v) => v
          case _ => 0.0d
        }
      case _ => 0.0d
    }

  }

  private[this] def wordFrequencyInQueriesFieldOfAllStates(indexName: String, word: String): Double = {
    // get keyword freq in all queries
    DecisionTableService.wordFrequenciesInQueries(indexName).getOrElse(word, 0.0d)
  }

  private[this] def keywordAbsoluteProbability(indexName: String, word: String): Double = {
    wordFrequencyInQueriesFieldOfAllStates(indexName, atomArgument) match {
      case v: Double if v > 0 => v
      case _ => 0.5d
    }
  }

  // Regex Frequency Functions

  private[this] def fractionOfQueriesMatchingRegEx(indexName: String, rx: String, totalQueries: Long): Double = {
    totalQueries match {
      case v: Long if v <= 0 => throw ExceptionAtomic(atomName + ":totalQueries = 0 not expected here")
      case _ => DecisionTableService.totalQueriesMatchingRegEx(indexName, rx) / totalQueries.toDouble
    }
  }

  private[this] def fractionOfQueriesOfStateMatchingRegEx(indexName: String, rx: String, state: String, totalStateQueries: Long): Double = {
    totalStateQueries match {
      case v: Long if v <= 0 => throw ExceptionAtomic(atomName + ":totalQueries = 0 not expected here")
      case _ => DecisionTableService.totalQueriesOfStateMatchingRegEx(indexName, rx, state) / totalStateQueries.toDouble
    }
  }


  private[this] def stateFrequency(indexName: String, stateName: String): Double = {
    val activeAnalyzeMap = AnalyzerService.analyzersMap.get(indexName) match {
      case Some(t) => t
      case _ => throw ExceptionAtomic(atomName + ":active analyzer map not found, DT not posted")
    }

    val currentState: DecisionTableRuntimeItem = activeAnalyzeMap.analyzerMap.get(stateName) match {
      case Some(t) => t
      case _ => throw ExceptionAtomic(atomName + ":state not found in map")
    }

    val nStates = activeAnalyzeMap.analyzerMap.size
    val nQueries = currentState.queries.length

    /*  ---- Solution using conversation logs temporarily disabled because of normalization problems -----
    val request = QAAggregatedAnalyticsRequest(
      aggregations = Some(List(QAAggregationsTypes.qaMatchedStatesHistogram, QAAggregationsTypes.qaMatchedStatesWithScoreHistogram)))

    val queryResult: QAAggregatedAnalytics = ConversationLogsService.analytics(indexName, request)

    // extract histograms qaMatchedStatesHistogram and qaMatchedStatesWithScoreHistogram
    val histograms: Map[String, List[LabelCountHistogramItem]] = queryResult.labelCountHistograms match {
      case Some(t) => t
      case _ => throw ExceptionAtomic(atomName + ":analytics does not return histograms")
    }

    // extract histogram qaMatchedStatesHistogram (occurrences of state regardless score)
    val stateFrequenciesHist: List[LabelCountHistogramItem] = histograms.get("qaMatchedStatesHistogram") match {
      case Some(t) => t
      case _ => throw ExceptionAtomic(atomName + ":analytics does not contain qaMatchedStatesHistogram")
    }

    // look if actually processed state is present in the histogram. There should be not more than one
    val stateHits: Double = stateFrequenciesHist.find(elem => elem.key === stateName) match {
      case Some(t) => t.docCount
      case _ => 0
    }

    (stateHits + 100.0d) / (100.0d * nStates + queryResult.totalDocuments)
    */
    nStates match {
      case v: Int if v > 0 => {
        nQueries match {
          case n: Int if n > 0 => nQueries.toDouble / nStates
          case _ => 1.0 / nStates
        }
      }
      case _ => 0d
    }
  }

  def probStateGivenAKeyword(userQuery: String, keyword: String, indexName: String, stateName: String): Result = {

    // check if keyword is present in the userQuery using regex with word boundary condition
    if (rxToBeMatched.findFirstMatchIn(userQuery).isEmpty) {
      //keyword not present in the user's query: return 0, whatever the other conditions are
      Result(score = 0.0d)
    }
    else {
      val activeAnalyzeMap = AnalyzerService.analyzersMap.get(indexName) match {
        case Some(t) => t
        case _ => throw ExceptionAtomic(atomName + ":active analyzer map not found, DT not posted")
      }

      val currentState: DecisionTableRuntimeItem = activeAnalyzeMap.analyzerMap.get(stateName) match {
        case Some(t) => t
        case _ => throw ExceptionAtomic(atomName + ":state not found in map")
      }

      val pS = stateFrequency(indexName, stateName)

      //  keyword present in the user's query but not in the state's queries: return P(S) (because we cannot return 0)
      if (currentState.queries.isEmpty) {
        Result(score = pS)
      }
      else {
        val pKS = fractionOfQueriesWithWordOfState(indexName, keyword, stateName)
        val pK = keywordAbsoluteProbability(indexName, keyword)
        // keyword present in the user's query but not in the state's queries: return P(S) (because we cannot return 0)
        if (pKS === 0 || pK === 0) {
          val msg = atomName + "(" + atomArgument + "):Result(" + pS + ") : state(" + stateName + ") : PK_S(" + pKS + ") : PK(" + pK + ") : PS(" + pS + ")"
          println(msg) //"TODO: remove me"
          Result(score = pS)
        }
        else {
          val pSK = pKS * pS / pK
          val msg = atomName + "(" + atomArgument + "):Result(" + pSK + ") : state(" + stateName + ") : PK_S(" + pKS + ") : PK(" + pK + ") : PS(" + pS + ") : PS_K(" + pSK + ")"
          println(msg) //"TODO: remove me"

          Result(score = pSK)
        }

      }

    }

  }

  def probStateGivenARegEx(userQuery: String, rx: String, indexName: String, stateName: String): Result = {
    // check if regular expression is present in the userQuery.
    if (rxToBeMatched.findFirstMatchIn(userQuery).isEmpty) {
      //regex not matched in the user's query: return 0, whatever the other conditions are
      Result(score = 0.0d)
    }
    else {
      val activeAnalyzeMap = AnalyzerService.analyzersMap.get(indexName) match {
        case Some(t) => t
        case _ => throw ExceptionAtomic(atomName + ":active analyzer map not found, DT not posted")
      }

      val currentState: DecisionTableRuntimeItem = activeAnalyzeMap.analyzerMap.get(stateName) match {
        case Some(t) => t
        case _ => throw ExceptionAtomic(atomName + ":state not found in map")
      }

      val pS = stateFrequency(indexName, stateName)

      //  keyword present in the user's query but not in the state's queries: return P(S) (because we cannot return 0)
      if (currentState.queries.isEmpty) {
        Result(score = pS)
      }
      else {
        val totalQueries = activeAnalyzeMap.analyzerMap.foldLeft(0) { (sum, state) => sum + state._2.queries.size }
        val pReS = fractionOfQueriesOfStateMatchingRegEx(indexName, rx, stateName, currentState.queries.length)
        val pRe = fractionOfQueriesMatchingRegEx(indexName, rx, totalQueries)

        if (pReS === 0 || pRe === 0) {
          val msg = atomName + "(" + atomArgument + "):Result(" + pS + ") : state(" + stateName + ") : PRe_S(" + pReS + ") : PRe(" + pRe + ") : PS(" + pS + ")"
          println(msg) //"TODO: remove me"
          Result(score = pS)
        }
        else {
          val pSRe = pReS * pS / pRe
          val msg = atomName + "(" + atomArgument + "):Result(" + pSRe + ") : state(" + stateName + ") : Re_S(" + pReS + ") : PK(" + pRe + ") : PS(" + pS + ") : PS_K(" + pSRe + ")"
          println(msg) //"TODO: remove me"

          Result(score = pSRe)
        }

      }

    }
  }

  def evaluate(userQuery: String, data: AnalyzersDataInternal = AnalyzersDataInternal()): Result = {
    val indexName = data.context.indexName
    val stateName = data.context.stateName

    if (argumentIsSingleWord) {
      probStateGivenAKeyword(userQuery, atomArgument, indexName, stateName)
    }
    else if (argumentIsSingleStartsWithExpression) {
      val rxForES = atomArgument.replace("*", ".*")
      probStateGivenARegEx(userQuery, rxForES, indexName, stateName)
    }
    else {
      // we assume is double words expression. Only boolean result.
      if (rxToBeMatched.findFirstMatchIn(userQuery).isEmpty)
        Result(score = 0.0)
      else
        Result(score = 1.0)
    }


  }
}
