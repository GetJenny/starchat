package com.getjenny.starchat.analyzer.operators

import com.getjenny.analyzer.expressions._
import com.getjenny.analyzer.operators._
import scalaz.Scalaz._

/**
 * Created by michele boggia on 24/07/2020.
 */

class ConjunctionOperator(children: List[Expression]) extends AbstractOperator(children: List[Expression]) {
  override def toString: String = "ConjunctionOperator(" + children.mkString(", ") + ")"
  def add(e: Expression, level: Int = 0): AbstractOperator = {
    if (level === 0) {
      new ConjunctionOperator(e :: children)
    } else if(children.isEmpty){
      throw OperatorException("ConjunctionOperator: children list is empty")
    } else {
      children.headOption match {
        case Some(t) =>
          t match {
            case c: AbstractOperator => new ConjunctionOperator(c.add(e, level - 1) :: children.tail)
            case _ => throw OperatorException("ConjunctionOperator: trying to add to smt else than an operator")
          }
        case _ => throw OperatorException("ConjunctionOperator: trying to add None instead of an operator")

      }
    }
  }

  def evaluate(query: String, analyzersDataInternal: AnalyzersDataInternal = AnalyzersDataInternal()): Result = {
    def conjunction(l: List[Expression]): Result = {
      if (l.tail.isEmpty) {
        val res = l.head.evaluate(query, analyzersDataInternal)
        //        println("SCORE_NIL: " + res.score * 1.1 + "(" + res.score + ")")
        Result(score = res.score,
          AnalyzersDataInternal(
            context = analyzersDataInternal.context,
            traversedStates = analyzersDataInternal.traversedStates,
            extractedVariables = analyzersDataInternal.extractedVariables ++ res.data.extractedVariables, // order is important, as res elements must override pre-existing elements
            data = analyzersDataInternal.data ++ res.data.data
          )
        )
      } else {
        val val1 = l.head.evaluate(query, analyzersDataInternal)
        val val2 = conjunction(l.tail)
        //        println("SCORE_NOT_NIL: " + (val1.score * 1.1) * val2.score + "(" + val1.score + ")" + "(" + val2.score + ")")
        val finalScore = val1.score * val2.score
        if (finalScore != 0) {
          Result(score = finalScore,
            AnalyzersDataInternal(
              context = analyzersDataInternal.context,
              traversedStates = analyzersDataInternal.traversedStates,
              extractedVariables = val2.data.extractedVariables ++ val1.data.extractedVariables, // order is important, as var1 elements must override var2 existing elements
              data = val2.data.data ++ val1.data.data
            )
          )
        } else {
          Result(score = finalScore,
            AnalyzersDataInternal(
              context = analyzersDataInternal.context,
              traversedStates = analyzersDataInternal.traversedStates,
              extractedVariables = analyzersDataInternal.extractedVariables,
              data = analyzersDataInternal.data
            )
          )
        }
      }
    }
    conjunction(children)
  }
}
