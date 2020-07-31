package com.getjenny.starchat.analyzer.operators

/**
  * Created by Andrea Collamati <andrea@getjenny.com> on 20/09/2019.
  */

import com.getjenny.analyzer.expressions.Expression
import com.getjenny.analyzer.interfaces._
import com.getjenny.analyzer.operators._
import com.getjenny.starchat.analyzer.{operators => newOp}

class StarchatFactoryOperator extends OperatorFactoryTrait[List[Expression], AbstractOperator] {

  override val operations: Set[String] = Set(
    "or",
    "and",
    "conjunction",
    "disjunction",
    "bor",
    "band",
    "booleanor",
    "booleanand",
    "booleanOr",
    "booleanAnd",
    "booleanNot",
    "booleannot",
    "bnot",
    "maximum",
    "max",
    "reinfConjunction",
    "binarize",
    "eq",
    "lt",
    "gt",
    "lte",
    "gte",
    "bayes"
  )

  override def get(name: String, argument: List[Expression]): AbstractOperator = name.filter(c => !c.isWhitespace ) match {
    case "booleanOr" | "booleanor" | "bor" => new newOp.BooleanOrOperator(argument)
    case "booleanAnd"| "booleanand"| "band" => new newOp.BooleanAndOperator(argument)
    case "booleanNot"| "booleannot"| "bnot" => new newOp.BooleanNotOperator(argument)
    case "conjunction" | "and" => new newOp.ConjunctionOperator(argument)
    case "disjunction" | "or" => new newOp.DisjunctionOperator(argument)
    case "reinfConjunction" => new newOp.ReinfConjunctionOperator(argument)
    case "maximum" | "max" => new MaxOperator(argument)
    case "binarize" => new BinarizeOperator(argument)
    case "eq" => new EqOperator(argument)
    case "lt" => new LtOperator(argument)
    case "gt" => new GtOperator(argument)
    case "lte" => new LteOperator(argument)
    case "gte" => new GteOperator(argument)
    case "bayes" => new BayesOperator(argument)
    case _ => throw OperatorNotFoundException("Operator \'" + name + "\' not found")
  }

}
