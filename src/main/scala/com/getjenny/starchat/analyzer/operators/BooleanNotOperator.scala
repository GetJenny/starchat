package com.getjenny.starchat.analyzer.operators

import com.getjenny.analyzer.expressions._
import com.getjenny.analyzer.operators._
import scalaz.Scalaz._

/** Not Operator
 *
 * It can only take one argument --we leave List instead of Set as argument
 * so that Parser.gobble_command can add one Expression. Still, we throw exception if
 * more than one child is added
 *
 * Created by mal on 21/02/2017.
 */

class BooleanNotOperator(child: List[Expression]) extends AbstractOperator(child: List[Expression]) {
  require(child.length <= 1, "BooleanNotOperator can only have one Expression")
  override def toString: String = "booleanNot(" + child + ")"
  def add(e: Expression, level: Int = 0): AbstractOperator = {
    if (level === 0) {
      if (child.nonEmpty)
        throw OperatorException("BooleanNotOperator: trying to add more than one expression.")
      new BooleanNotOperator(e :: child)
    } else child.headOption match {
      case Some(c: AbstractOperator) =>
        child.tailOption match {
          case Some(tail) =>
            if (tail.nonEmpty)
              throw OperatorException("BooleanNotOperator: more than one child expression.")
            else
              new BooleanNotOperator(c.add(e, level - 1) :: child.tail)
          case _ =>
            throw OperatorException("BooleanNotOperator: requires one argument")
        }
      case _ => throw OperatorException("BooleanNotOperator: trying to add to smt else than an operator.")
    }
  }

  def evaluate(query: String, analyzersDataInternal: AnalyzersDataInternal = AnalyzersDataInternal()): Result = {
    val res = child.headOption match {
      case Some(arg) => arg.matches(query, analyzersDataInternal)
      case _ => throw OperatorException("BooleanNotOperator: inner expression is empty")
    }
    Result(score = 1 - res.score, data = analyzersDataInternal)
  }
}
