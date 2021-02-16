package com.getjenny.starchat.entities.io

import scalaz.Scalaz._

object SuggesterSortAlgorithm extends Enumeration {
  val NONE,
  STATE_POPULARITY,
  QUERY_POPULARITY,
  DEFAULT = SuggesterSortAlgorithm.Value

  def value(algorithm: String): SuggesterSortAlgorithm.Value = values.find(_.toString === algorithm).getOrElse(DEFAULT)
}