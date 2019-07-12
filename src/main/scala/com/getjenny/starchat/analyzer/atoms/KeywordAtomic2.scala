package com.getjenny.starchat.analyzer.atoms

/**
  * Created by Andrea Collamati <andrea@getjenny.com> on 07/09/2019.
  */

import com.getjenny.analyzer.atoms.{AbstractAtomic, ExceptionAtomic}
import com.getjenny.analyzer.expressions.{AnalyzersDataInternal, Result}
import com.getjenny.starchat.entities._
import com.getjenny.starchat.services._
import scalaz.Scalaz._

class KeywordAtomic2(arguments: List[String], restrictedArgs: Map[String, String]) extends AbstractAtomic {

  val atomName:String = "keyword2"

  val keyword: String = arguments.headOption match {
    case Some(t) => t
    case _ =>
      throw ExceptionAtomic(atomName + ": search requires argument")
  }

  override def toString: String = atomName + "(\"" + keyword + "\")"

  val isEvaluateNormalized: Boolean = true
  override val matchThreshold: Double = 0.0

  val decisionTableService: DecisionTableService.type = DecisionTableService
  val conversationLogsService: ConversationLogsService.type = ConversationLogsService


  private[this] def fractionOfQueriesWithWordOfState(indexName: String, word: String, state: String): Double = {
    // get keyword freq in queries associated to the current state
    //TODO: to be optimized"
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
    wordFrequencyInQueriesFieldOfAllStates(indexName, keyword) match {
      case v: Double if v > 0 => v
      case _ => 0.5d
    }
  }


  private[this] def stateFrequency(indexName: String, stateName: String): Double = {
    val activeAnalyzeMap = AnalyzerService.analyzersMap.get(indexName) match {
      case Some(t) => t
      case _ => throw ExceptionAtomic(atomName + ":active analyzer map not found, DT not posted")
    }
    val nStates = activeAnalyzeMap.analyzerMap.size
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

  }

  def evaluate(userQuery: String, data: AnalyzersDataInternal = AnalyzersDataInternal()): Result = {

    if (!userQuery.contains(keyword)) {
      //keyword not present in the user's query: return 0, whatever the other conditions are
      Result(score = 0.0d)
    }
    else {

      val currentStateName = data.context.stateName
      val indexName = data.context.indexName
      val activeAnalyzeMap = AnalyzerService.analyzersMap.get(indexName) match {
        case Some(t) => t
        case _ => throw ExceptionAtomic(atomName + ":active analyzer map not found, DT not posted")
      }
      val currentState: DecisionTableRuntimeItem =
        activeAnalyzeMap.analyzerMap.get(currentStateName) match {
          case Some(t) => t
          case _ => throw ExceptionAtomic(atomName + ":state not found in map")
        }

      val pS = stateFrequency(indexName, currentStateName)

      //  keyword present in the user's query but not in the state's queries: return P(S) (because we cannot return 0)
      if (currentState.queries.isEmpty) {
        Result(score = pS)
      }
      else {
        val pKS = fractionOfQueriesWithWordOfState(indexName, keyword, currentStateName)
        val pK = keywordAbsoluteProbability(indexName, keyword)
        // keyword present in the user's query but not in the state's queries: return P(S) (because we cannot return 0)
        if (pKS === 0 || pK === 0) {
          val msg = atomName + ":Result(" + pS + ") : state(" + currentStateName + ") : PK_S(" + pKS + ") : PK(" + pK + ") : PS(" + pS + ")"
          println(msg) //"TODO: remove me"
          Result(score = pS)
        }
        else {
          val pSK = pKS * pS / pK
          val msg = atomName + ":Result(" + pSK + ") : state(" + currentStateName + ") : PK_S(" + pKS + ") : PK(" + pK + ") : PS(" + pS + ") : PS_K(" + pSK + ")"
          println(msg) //"TODO: remove me"

          Result(score = pSK)
        }

      }

    }

  }
}
