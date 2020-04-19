package com.getjenny.starchat.entities.persistents

import com.getjenny.starchat.entities.io.SuggestionCategory
import org.elasticsearch.action.get.GetResponse
import org.elasticsearch.action.search.SearchResponse
import org.elasticsearch.search.suggest.completion.CompletionSuggestion
import sun.reflect.generics.reflectiveObjects.NotImplementedException

import scala.collection.JavaConverters._
import scala.collection.immutable.{List, Map}


object SuggestedQueriesEntityManager extends ReadEntityManager[SuggestedQuery] {
  override def fromSearchResponse(response: SearchResponse): List[SuggestedQuery] = {

    val completionSuggestion: CompletionSuggestion = response.getSuggest().getSuggestion("suggest")
    completionSuggestion.getOptions().asScala.map(
      opt => SuggestedQuery(
        opt.getScore(),
        opt.getText().string(),
        SuggestionCategory.value(opt.getContexts().get("sugg_weight").asScala.head))).toList
  }

  override def fromGetResponse(response: List[GetResponse]): List[SuggestedQuery] = {
    throw new NotImplementedException()
  }
}