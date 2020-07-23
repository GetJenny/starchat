package com.getjenny.analyzer.operators

import com.getjenny.analyzer.expressions._
import scalaz.Scalaz._

/**
 * Created by angelo on 18/01/2018.
 */

class TestReinfConjunctionOperator(children: List[Expression]) extends AbstractOperator(children: List[Expression]) {
  override def toString: String = "TestReinfConj(" + children.mkString(", ") + ")"
  def add(e: Expression, level: Int = 0): AbstractOperator = {
    if (level === 0) {
      new TestReinfConjunctionOperator(e :: children)
    } else if(children.isEmpty){
      throw OperatorException("TestReinfConj children list is empty")
    } else {
      children.headOption match {
        case Some(t) =>
          t match {
            case c: AbstractOperator => new TestReinfConjunctionOperator(c.add(e, level - 1) :: children.tail)
            case _ => throw OperatorException("TestReinfConj: trying to add to smt else than an operator")
          }
        case _ =>
          throw OperatorException("TestReinfConj: trying to add None instead of an operator")
      }
    }
  }

  def evaluate(query: String, analyzersDataInternal: AnalyzersDataInternal = new AnalyzersDataInternal): Result = {
    def reinfConjunction(l: List[Expression]): Result = {
      val res = l.head.evaluate(query, analyzersDataInternal)
      if (l.tail.isEmpty) {
        //        println("SCORE_NIL: " + res.score * 1.1 + "(" + res.score + ")")
        Result(score = res.score * 1.1,
          AnalyzersDataInternal(
            context = analyzersDataInternal.context,
            traversedStates = analyzersDataInternal.traversedStates,
            extractedVariables = analyzersDataInternal.extractedVariables ++ res.data.extractedVariables, // order is important, as res elements must override pre-existing elements
            data = res.data.data
          )
        )
      } else {
        val val1 = l.head.evaluate(query, analyzersDataInternal)
        val val2 = reinfConjunction(l.tail)
        //        println("SCORE_NOT_NIL: " + (val1.score * 1.1) * val2.score + "(" + val1.score + ")" + "(" + val2.score + ")")
        val finalScore = (val1.score * 1.1) * val2.score
        if (finalScore != 0) {
          Result(score = finalScore,
            AnalyzersDataInternal(
              context = analyzersDataInternal.context,
              traversedStates = analyzersDataInternal.traversedStates,
              extractedVariables = val2.data.extractedVariables ++ val1.data.extractedVariables, // order is important, as var1 elements must override var2 existing elements
              data = analyzersDataInternal.data ++ val1.data.data ++ val2.data.data
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
    reinfConjunction(children)
  }
}
