package com.getjenny.starchat.analyzer.analyzers.script_support.scalajs

import com.getjenny.analyzer.expressions.{AnalyzersDataInternal, Result}
import com.getjenny.starchat.analyzer.analyzers.AbstractAnalyzer
import javax.script.{CompiledScript, Invocable, ScriptEngine}

import scala.util.{Failure, Success, Try}

final case class ScalaJSAnalyzerExcpetion(
                                           private val message: String = "",
                                           private val cause: Throwable = None.orNull
                                         ) extends Exception(message, cause)

class ScalaJSAnalyzer(compiledScript: CompiledScript, restrictedArgs: Map[String, String]) extends AbstractAnalyzer {

  private[this] val invocable: ScriptEngine with Invocable = compiledScript.getEngine match {
    case se: ScriptEngine with Invocable => se
    case _ => throw ScalaJSAnalyzerExcpetion("Could not initialize Invocable ScriptEngine")
  }
  private[this] val bindings = invocable.createBindings()
  Try(compiledScript.eval(bindings)) match {
    case Failure(e) => throw ScalaJSAnalyzerExcpetion("Evaluation error: " + e.getMessage, e.getCause)
    case Success(_) => Unit
  }
  private[this] val analyzer = bindings.get("analyzer")

  def evaluate(sentence: String, data: AnalyzersDataInternal): Result = {

    val result = invocable.invokeMethod(analyzer, "call", analyzer, sentence, data, restrictedArgs) match {
      case r: Result => r
      case _ => Result(0, data)
    }

    result
  }
}
