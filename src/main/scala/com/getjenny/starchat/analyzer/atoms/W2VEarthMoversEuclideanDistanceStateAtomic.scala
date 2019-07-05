package com.getjenny.starchat.analyzer.atoms

/**
  * Created by angelo on 11/04/17.
  */

import com.getjenny.analyzer.atoms.{AbstractAtomic, ExceptionAtomic}
import com.getjenny.analyzer.expressions.{AnalyzersDataInternal, Result}
import com.getjenny.starchat.analyzer.utils.EMDVectorDistances
import com.getjenny.starchat.entities.{CommonOrSpecificSearch, TextTerms}
import com.getjenny.starchat.services._
import com.getjenny.starchat.utils.Index

class W2VEarthMoversEuclideanDistanceStateAtomic(val arguments: List[String], restrictedArgs: Map[String, String])
  extends AbstractAtomic  {
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
      throw ExceptionAtomic("similarEucEmdState requires an argument")
  }

  val commonOrSpecific: CommonOrSpecificSearch.Value = arguments.lastOption match {
    case Some(t) => CommonOrSpecificSearch.value(t)
    case _ => CommonOrSpecificSearch.COMMON
  }

  val termService: TermService.type = TermService

  implicit class Crosstable[X](xs: Traversable[X]) {
    def cross[Y](ys: Traversable[Y]) = for { x <- xs; y <- ys } yield (x, y)
  }

  override def toString: String = "similarEucEmdState(\"" + state + "\")"

  val analyzerService: AnalyzerService.type = AnalyzerService

  val originalIndexName: String = restrictedArgs("index_name")
  val indexName: String = Index.resolveIndexName(originalIndexName, commonOrSpecific)

  val queriesSentences: Option[DecisionTableRuntimeItem] =
    AnalyzerService.analyzersMap(indexName).analyzerMap.get(state)
  if (queriesSentences.isEmpty) {
    analyzerService.log.error(toString + " : state is null")
  } else {
    analyzerService.log.info(toString + " : initialized")
  }

  val queriesVectors: List[TextTerms] = queriesSentences match {
    case Some(sentences) => sentences.queriesTerms.map(item => item)
    case _ => List.empty[TextTerms]
  }
  val isEvaluateNormalized: Boolean = true
  def evaluate(query: String, data: AnalyzersDataInternal = AnalyzersDataInternal()): Result = {
    val queryVectors = termService.textToVectors(indexName = indexName, text = query)
    val emdDistQueries = queriesVectors.map(q => {
      val dist = EMDVectorDistances.distanceEuclidean(q , queryVectors)
      dist
    })

    val emdDist = if (emdDistQueries.nonEmpty) emdDistQueries.max else 0.0
    Result(score=emdDist)
  }

  // Similarity is normally the cosine itself. The threshold should be at least

  // angle < pi/2 (cosine > 0), but for synonyms let's put cosine > 0.6, i.e. self.evaluate > 0.8
  override val matchThreshold: Double = 0.8
}
