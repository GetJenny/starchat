package com.getjenny.starchat.entities.io

import scalaz.Scalaz._

object SuggestionCategory extends Enumeration {
  val NOT_VALIDATED,
  VALID,
  DISCARDED = SuggestionCategory.Value

  def value(algorithm: String): SuggestionCategory.Value = values.find(_.toString === algorithm).getOrElse(NOT_VALIDATED)
}
