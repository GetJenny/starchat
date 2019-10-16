package com.getjenny.starchat.analyzer.atoms

import com.getjenny.analyzer.atoms.{AbstractAtomic, ExceptionAtomic}
import com.getjenny.analyzer.expressions.{AnalyzersDataInternal, Result}

class HasTravStateInPositionRevAtomic(arguments: List[String], restrictedArgs: Map[String, String]) extends AbstractAtomic{
  val state: String = arguments.headOption match {
    case Some(t) => t
    case _ =>
      throw ExceptionAtomic("hasTravStateInPositionRev: missing first argument")
  }
  val position: Int = arguments.lift(1) match {
    case Some(t) => t.toInt
    case _ =>
      throw ExceptionAtomic("hasTravStateInPositionRev: missing second argument")
  }

  val isEvaluateNormalized: Boolean = true

  override def toString: String = "hasTravStateInPosition(\"" + state + "\", \"" + position + "\")"

  def evaluate(query: String, data: AnalyzersDataInternal = AnalyzersDataInternal()): Result = {
    data.traversedStates.reverse.lift(position-1) match {
      case Some(this.state) => Result(1)
      case _ => Result(0)
    }
  }
}
