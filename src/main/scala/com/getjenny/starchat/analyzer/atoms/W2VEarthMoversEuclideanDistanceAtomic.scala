package com.getjenny.starchat.analyzer.atoms

import com.getjenny.analyzer.atoms.{AbstractAtomic, ExceptionAtomic}
import com.getjenny.analyzer.entities.{AnalyzersDataInternal, Result}
import com.getjenny.starchat.analyzer.utils.EMDVectorDistances
import com.getjenny.starchat.entities.io.CommonOrSpecificSearch
import com.getjenny.starchat.services._
import com.getjenny.starchat.utils.Index

/**
  * Created by angelo on 04/04/17.
  */

class W2VEarthMoversEuclideanDistanceAtomic(val arguments: List[String],
                                            restrictedArgs: Map[String, String]) extends AbstractAtomic {
  /**
    * cosine distance between sentences renormalized at [0, 1]: (cosine + 1)/2
    *
    * state_lost_password_cosine = Cosine("lost password")
    * state_lost_password_cosine.evaluate("I'm desperate, I've lost my password")
    *
    */

  val sentence: String = arguments.headOption match {
    case Some(t) => t
    case _ =>
      throw ExceptionAtomic("similarEucEmd requires an argument")
  }

  val commonOrSpecific: CommonOrSpecificSearch.Value = arguments.lastOption match {
    case Some(t) => CommonOrSpecificSearch.value(t)
    case _ => CommonOrSpecificSearch.COMMON
  }

  val termService: TermService.type = TermService

  override def toString: String = "similarEucEmd(\"" + sentence + "\")"

  val isEvaluateNormalized: Boolean = true

  def evaluate(query: String, data: AnalyzersDataInternal = AnalyzersDataInternal()): Result = {
    val indexName = Index.resolveIndexName(data.context.indexName, commonOrSpecific)

    val emdDist = EMDVectorDistances.distanceEuclidean(indexName, query, sentence)
    Result(score = emdDist)
  }

  // Similarity is normally the cosine itself. The threshold should be at least
  // angle < pi/2 (cosine > 0), but for synonyms let's put cosine > 0.6, i.e. self.evaluate > 0.8
  override val matchThreshold: Double = 0.8
}
