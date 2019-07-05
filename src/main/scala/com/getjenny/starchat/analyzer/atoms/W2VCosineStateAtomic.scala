package com.getjenny.starchat.analyzer.atoms

/**
  * Created by angelo on 11/04/17.
  */

import com.getjenny.analyzer.atoms.{AbstractAtomic, ExceptionAtomic}
import com.getjenny.analyzer.expressions.{AnalyzersDataInternal, Result}
import com.getjenny.analyzer.util.VectorUtils._
import com.getjenny.starchat.analyzer.utils.TextToVectorsTools
import com.getjenny.starchat.entities.{CommonOrSpecificSearch, _}
import com.getjenny.starchat.services._
import com.getjenny.starchat.utils.Index

class W2VCosineStateAtomic(val arguments: List[String], restrictedArgs: Map[String, String]) extends AbstractAtomic  {
  /**
    * cosine distance between sentences renormalized at [0, 1]: (cosine + 1)/2
    *
    * state_lost_password_cosine = Cosine("lost password")
    * state_lost_password_cosine.evaluate("I'm desperate, I've lost my password")
    *
    */

  val state: String = arguments.headOption match {
    case Some(t) => t
    case _ =>
      throw ExceptionAtomic("similarState requires an argument")
  }

  val commonOrSpecific: CommonOrSpecificSearch.Value = arguments.lastOption match {
    case Some(t) => CommonOrSpecificSearch.value(t)
    case _ => CommonOrSpecificSearch.COMMON
  }

  override def toString: String = "similarState(\"" + state + "\")"

  val analyzerService: AnalyzerService.type = AnalyzerService

  val originalIndexName: String = restrictedArgs("index_name")
  val indexName: String = Index.resolveIndexName(originalIndexName, commonOrSpecific)

  val querySentences: Option[DecisionTableRuntimeItem] =
    AnalyzerService.analyzersMap(indexName).analyzerMap.get(state)
  if (querySentences.isEmpty) {
    analyzerService.log.error(toString + " : state does not exists")
  } else {
    analyzerService.log.info(toString + " : initialized")
  }

  val queryTerms: List[TextTerms] = querySentences match {
    case Some(t) => t.queriesTerms
    case _ => List.empty[TextTerms]
  }

  val queryVectors: List[(Vector[Double], Double)] = queryTerms.map(item => {
    val query_vector = TextToVectorsTools.sumOfTermsVectors(item)
    query_vector
  })

  val isEvaluateNormalized: Boolean = true
  def evaluate(query: String, data: AnalyzersDataInternal = AnalyzersDataInternal()): Result = {
    val distance = queryVectors.map{
      case (sentenceVector, reliabilityFactor) =>
        val (querySentenceVector, queryReliabilityFactor) =
            TextToVectorsTools.sumOfVectorsFromText(indexName, query)
        val dist = (1.0 - cosineDist(sentenceVector, querySentenceVector)) *
          (reliabilityFactor * queryReliabilityFactor)
        dist
    }
    val dist = if (distance.nonEmpty) distance.max else 0.0
    Result(score=dist)
  }

  // Similarity is normally the cosine itself. The threshold should be at least
  // angle < pi/2 (cosine > 0), but for synonyms let's put cosine > 0.6, i.e. self.evaluate > 0.8
  override val matchThreshold: Double = 0.8
}
