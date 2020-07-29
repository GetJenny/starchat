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
      val valHead = l(0).matches(query, analyzersDataInternal)
      if (l.tail.isEmpty) {
        Result(score = valHead.score,
          AnalyzersDataInternal(
            context = analyzersDataInternal.context,
            traversedStates = analyzersDataInternal.traversedStates,
            // map summation order is important, as valHead elements must override pre-existing elements
            extractedVariables = analyzersDataInternal.extractedVariables ++ valHead.data.extractedVariables,
            data = analyzersDataInternal.data ++ valHead.data.data
          )
        )
      } else {
        val valTail = booleanAnd(l.tail)
        val finalScore = valHead.score * valTail.score
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
              // map summation order is important, as valHead elements must override valTail existing elements
              extractedVariables = valTail.data.extractedVariables ++ valHead.data.extractedVariables,
              data = valTail.data.data ++ valHead.data.data
            )
          )
        }
      }
    }
    booleanAnd(children)
  }
}
