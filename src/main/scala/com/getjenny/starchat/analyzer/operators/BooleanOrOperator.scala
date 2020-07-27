package com.getjenny.starchat.analyzer.operators

import com.getjenny.analyzer.expressions._
import com.getjenny.analyzer.operators._
import scalaz.Scalaz._

/**
 * Created by mal on 21/02/2017.
 */

class BooleanOrOperator(children: List[Expression]) extends AbstractOperator(children: List[Expression]) {
  override def toString: String = "BooleanOrOperator(" + children.mkString(", ") + ")"
  def add(e: Expression, level: Int): AbstractOperator = {

    if (level === 0) new BooleanOrOperator(e :: children)
    else {
      children.headOption match {
        case Some(t) =>
          t match {
            case c: AbstractOperator => new BooleanOrOperator(c.add(e, level - 1) :: children.tail)
            case _ => throw OperatorException("BooleanOrOperator: trying to add to smt else than an operator")
          }
        case _ =>
          throw OperatorException("BooleanOrOperator: trying to add None instead of an operator")
      }
    }
  }

  def evaluate(query: String, analyzersDataInternal: AnalyzersDataInternal = AnalyzersDataInternal()): Result = {
    def loop(l: List[Expression]): Result = {
      val res = l.head.matches(query, analyzersDataInternal)
      if (l.tail.isEmpty) {
        Result(score = res.score,
          AnalyzersDataInternal(
            context = analyzersDataInternal.context,
            traversedStates = analyzersDataInternal.traversedStates,
            extractedVariables = analyzersDataInternal.extractedVariables ++ res.data.extractedVariables,
            data = analyzersDataInternal.data ++ res.data.data
          )
        )
      } else {
        if (res.score < 1.0d) {
          loop(l.tail)
        } else {
          Result(score = res.score,
            AnalyzersDataInternal(
            context = analyzersDataInternal.context,
            traversedStates = analyzersDataInternal.traversedStates,
            extractedVariables = analyzersDataInternal.extractedVariables ++ res.data.extractedVariables,
            data = analyzersDataInternal.data ++ res.data.data
            )
          )
        }
      }
    }
    loop(children)
  }
}
