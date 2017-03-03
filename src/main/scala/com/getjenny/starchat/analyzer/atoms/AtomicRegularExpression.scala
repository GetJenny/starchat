package com.getjenny.starchat.analyzer.atoms

import com.getjenny.starchat.util.Vectors._

/**
  * Created by mal on 20/02/2017.
  */

class AtomicRegularExpression(re: String) extends AbstractAtomic {
  /**
    * A Regex
    */
  override def toString: String = "regex(\"" + re + "\")"
  val isEvaluateNormalized: Boolean = false
  private val rx = re.r
  def evaluate(query: String): Double = rx.findAllIn(query).toList.length
}