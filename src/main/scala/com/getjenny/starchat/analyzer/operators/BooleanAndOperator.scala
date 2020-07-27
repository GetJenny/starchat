package com.getjenny.starchat.analyzer.operators

import com.getjenny.analyzer.expressions._
import com.getjenny.analyzer.operators._
import scalaz.Scalaz._

/**
 * Created by mal on 21/02/2017.
 */

class BooleanAndOperator(children: List[Expression]) extends AbstractOperator(children: List[Expression]) {
  override def toString: String = "BooleanAndOperator(" + children.mkString(", ") + ")"
  def add(e: Expression, level: Int = 0): AbstractOperator = {
    if (level === 0) new BooleanAndOperator(e :: children)
    else {
      children.headOption match {
        case Some(t) =>
          t match {
            case c: AbstractOperator => new BooleanAndOperator(c.add(e, level - 1) :: children.tail)
            case _ => throw OperatorException("BooleanAndOperator: trying to add to smt else than an operator")
          }
        case _ =>
          throw OperatorException("BooleanAndOperator: trying to add None instead of an operator")
      }
    }
  }

  def evaluate(query: String, analyzersDataInternal: AnalyzersDataInternal = AnalyzersDataInternal()): Result = {
    def booleanAnd(l: List[Expression]): Result = {
      if (l.tail.isEmpty) {
        val res = l.head.matches(query, analyzersDataInternal)
        Result(score = res.score,
          AnalyzersDataInternal(
            context = analyzersDataInternal.context,
            traversedStates = analyzersDataInternal.traversedStates,
            extractedVariables = analyzersDataInternal.extractedVariables ++ res.data.extractedVariables, // order is important, as res elements must override pre-existing elements
            data = analyzersDataInternal.data ++ res.data.data
          )
        )
      } else {
        val val1 = l.head.matches(query, analyzersDataInternal)
        val val2 = booleanAnd(l.tail)
        val finalScore = val1.score * val2.score
        if (finalScore  < 1.0d) {
          Result(score = finalScore,
            AnalyzersDataInternal(
              context = analyzersDataInternal.context,
              traversedStates = analyzersDataInternal.traversedStates,
              extractedVariables = analyzersDataInternal.extractedVariables,
              data = analyzersDataInternal.data
            )
          )
        } else {
          Result(score = finalScore,
            AnalyzersDataInternal(
              context = analyzersDataInternal.context,
              traversedStates = analyzersDataInternal.traversedStates,
              extractedVariables = val2.data.extractedVariables ++ val1.data.extractedVariables, // order is important, as var1 elements must override var2 existing elements
              data = val2.data.data ++ val1.data.data
            )
          )
        }
      }
    }
    booleanAnd(children)
  }
}
