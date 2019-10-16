package com.getjenny.starchat.analyzer.atoms

/**
  * Created by mal on 20/02/2017.
  */

import com.getjenny.analyzer.atoms.{AbstractAtomic, ExceptionAtomic}
import com.getjenny.analyzer.expressions.{AnalyzersDataInternal, Result}
import com.getjenny.starchat.entities._
import com.getjenny.starchat.services._

/**
  * Query ElasticSearch
  */
class SearchAtomic(arguments: List[String], restrictedArgs: Map[String, String]) extends AbstractAtomic {

  val atomName: String = "search"

  val stateArg: Option[String] = arguments.headOption

  override def toString: String = atomName + "(\"" + stateArg + "\")"
  val isEvaluateNormalized: Boolean = false

  override val matchThreshold: Double = 0.65

  val decisionTableService: DecisionTableService.type = DecisionTableService

  def evaluate(query: String, data: AnalyzersDataInternal = AnalyzersDataInternal()): Result = {
    val state = stateArg.getOrElse(data.context.stateName)
    val score = data.data.get("dt_queries_search_result") match {
      case Some(searchResult) =>
        val res = searchResult.asInstanceOf[Map[String, (Float, SearchDTDocument)]]
        res.get(state) match {
          case Some((referenceStateScore, _)) =>
            referenceStateScore
          case _ => 0.0d
        }
      case _ => 0.0d
    }

    Result(score=score * 1.5d)
  } // returns elasticsearch score of the highest query in queries
}
