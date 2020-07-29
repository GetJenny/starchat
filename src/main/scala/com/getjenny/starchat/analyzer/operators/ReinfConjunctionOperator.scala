package com.getjenny.starchat.analyzer.operators

import com.getjenny.analyzer.expressions._
import com.getjenny.analyzer.operators._
import scalaz.Scalaz._
import scala.math.Ordering.Double.equiv

/**
 * Created by angelo on 18/01/2018.
 */

class ReinfConjunctionOperator(children: List[Expression]) extends AbstractOperator(children: List[Expression]) {
  override def toString: String = "ReinfConjunctionOperator(" + children.mkString(", ") + ")"
  def add(e: Expression, level: Int = 0): AbstractOperator = {
    if (level === 0) {
      new ReinfConjunctionOperator(e :: children)
    } else if(children.isEmpty){
      throw OperatorException("ReinfConjunctionOperator children list is empty")
    } else {
      children.headOption match {
        case Some(t) =>
          t match {
            case c: AbstractOperator => new ReinfConjunctionOperator(c.add(e, level - 1) :: children.tail)
            case _ => throw OperatorException("ReinfConjunctionOperator: trying to add to smt else than an operator")
          }
        case _ =>
          throw OperatorException("ReinfConjunctionOperator: trying to add None instead of an operator")
      }
    }
  }

  def evaluate(query: String, analyzersDataInternal: AnalyzersDataInternal = new AnalyzersDataInternal): Result = {
    def reinfConjunction(l: List[Expression]): Result = {
      val valHead = l(0).evaluate(query, analyzersDataInternal)
      if (l.tail.isEmpty) {
        Result(score = valHead.score * 1.1,
          AnalyzersDataInternal(
            context = analyzersDataInternal.context,
            traversedStates = analyzersDataInternal.traversedStates,
            // map summation order is important, as valHead elements must override pre-existing elements
            extractedVariables = analyzersDataInternal.extractedVariables ++ valHead.data.extractedVariables,
            data = analyzersDataInternal.data ++ valHead.data.data
          )
        )
      } else {
        val valTail = reinfConjunction(l.tail)
        val finalScore = (valHead.score * 1.1) * valTail.score
        if (equiv(finalScore, 0)) {
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
    reinfConjunction(children)
  }
}
