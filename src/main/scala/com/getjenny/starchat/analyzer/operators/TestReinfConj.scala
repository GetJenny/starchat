package com.getjenny.analyzer.operators

import com.getjenny.analyzer.expressions._
import scalaz.Scalaz._

/**
 * Created by angelo on 18/01/2018.
 */

class TestReinfConj(children: List[Expression]) extends AbstractOperator(children: List[Expression]) {
  override def toString: String = "TestReinfConj(" + children.mkString(", ") + ")"
  def add(e: Expression, level: Int = 0): AbstractOperator = {
    if (level === 0) {
      new TestReinfConj(e :: children)
    } else if(children.isEmpty){
      throw OperatorException("TestReinfConj children list is empty")
    } else {
      children.headOption match {
        case Some(t) =>
          t match {
            case c: AbstractOperator => new TestReinfConj(c.add(e, level - 1) :: children.tail)
            case _ => throw OperatorException("TestReinfConj: trying to add to smt else than an operator")
          }
        case _ =>
          throw OperatorException("TestReinfConj: trying to add None instead of an operator")
      }
    }
  }

  def evaluate(query: String, data: AnalyzersDataInternal = new AnalyzersDataInternal): Result = {
    def reinfConjunction(l: List[Expression]): Result = {
      val res = l.head.evaluate(query, data)
      if (l.tail.isEmpty) {
        //        println("SCORE_NIL: " + res.score * 1.1 + "(" + res.score + ")")
        Result(score = res.score * 1.1,
          AnalyzersDataInternal(
            context = data.context,
            traversedStates = data.traversedStates,
            extractedVariables = data.extractedVariables ++ res.data.extractedVariables,
            data = res.data.data
          )
        )
      } else {
        val val1 = l.head.evaluate(query, data)
        val val2 = reinfConjunction(l.tail)
        //        println("SCORE_NOT_NIL: " + (val1.score * 1.1) * val2.score + "(" + val1.score + ")" + "(" + val2.score + ")")
        Result(score = (val1.score * 1.1) * val2.score,
          AnalyzersDataInternal(
            context = data.context,
            traversedStates = data.traversedStates,
            extractedVariables = val1.data.extractedVariables ++ val2.data.extractedVariables,
            data = data.data ++ val1.data.data ++ val2.data.data
          )
        )
      }
    }
    reinfConjunction(children)
  }
}
