package com.getjenny.starchat.services

/**
  * Created by Andrea Collamati <andrea@getjenny.com> on 13/04/2020.
  */

import java.util

import akka.event.{Logging, LoggingAdapter}
import com.getjenny.starchat.SCActorSystem
import com.getjenny.starchat.entities.io._
import com.getjenny.starchat.entities.persistents.{SuggestedQueriesEntityManager}
import com.getjenny.starchat.services.esclient.DecisionTableElasticClient
import com.getjenny.starchat.services.esclient.crud.IndexLanguageCrud
import org.elasticsearch.common.xcontent.{ToXContent}
import org.elasticsearch.search.suggest.{SuggestBuilder, SuggestBuilders}
import org.elasticsearch.search.suggest.completion.context.{CategoryQueryContext}
import scala.collection.immutable.{List}

case class AutoCompleteException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)


case class AutoCompleteDTNotLoadedException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)


/**
  * Implements response functionalities
  */
object AutoCompleteService extends AbstractDataService {
  override val elasticClient: DecisionTableElasticClient.type = DecisionTableElasticClient
  private[this] val log: LoggingAdapter = Logging(SCActorSystem.system, this.getClass.getCanonicalName)


  def autoComplete(indexName: String, request: AutoCompleteRequest): AutoCompleteResponse = {
    val sortAlgorithm = request.sortAlgorithm match {
      case Some(a) => a
      case _ => SuggesterSortAlgorithm.DEFAULT
    }

    val maxResults = request.maxResults match {
      case Some(v) => Option(v)
      case _ => Option(10)
    }

    val categories = request.suggestionCategories match {
      case Some(v) => v
      case _ => List(SuggestionCategory.VALID)
    }

    val suggester = request.suggesterType match {
      case Some(st) => st
      case _ => Suggester.DEFAULT
    }

    val threshold = request.threshold match {
      case Some(t) => t
      case _ => 0.0
    }

    /*
    POST index_english.state/_search?pretty
    {
        "suggest": {
            "query-suggest" : {
                "prefix" : "Can",
                "completion" : {
                    "field" : "queries.query.suggest",
                    "contexts": {
                        "sugg_weight": ["1","0"]
                    }
                }
            }
        }
    }
    */


    // prepare completion suggest query from categories parameter
    val categoriesXContents = new java.util.LinkedList[CategoryQueryContext]
    for (c <- categories) {
      categoriesXContents.addLast(new CategoryQueryContext.Builder().setCategory(c.toString).build())
    }
    val context = new util.HashMap[String, util.List[_ <: ToXContent]]
    context.put("sugg_weight", categoriesXContents)

    val suggesterField = suggester match {
      case Suggester.DEFAULT => "queries.query.suggest"
      case _ => "queries.query.suggest" // For future implementations
    }
    val completionSuggestionBuilder = SuggestBuilders.completionSuggestion(suggesterField).contexts(context).prefix(request.userText)
    val suggestBuilder = new SuggestBuilder().addSuggestion("suggest", completionSuggestionBuilder)


    // run query
    val indexLanguageCrud = IndexLanguageCrud(elasticClient, indexName)
    val suggestions = indexLanguageCrud.suggest(suggestBuilder, entityManager = SuggestedQueriesEntityManager, maxItems = maxResults)

    val maxScore: Double = if (suggestions.nonEmpty) {
      suggestions.maxBy(_.score).score
    } else {
      0.0f
    }
    AutoCompleteResponse(suggestions.size, maxScore, suggestions)
  }

}
