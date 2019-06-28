package com.getjenny.starchat.analyzer.analyzers.script_support.scalajs

import com.getjenny.analyzer.expressions.{AnalyzersDataInternal, Result}
import com.getjenny.starchat.analyzer.analyzers.AbstractAnalyzer
import javax.script.{CompiledScript, ScriptEngine}

final case class ScalaJSAnalyzerExcpetion(
                                           private val message: String = "",
                                           private val cause: Throwable = None.orNull
                                         ) extends Exception(message, cause)

class ScalaJSAnalyzer(compiledScript: CompiledScript) extends AbstractAnalyzer {

  val engine: ScriptEngine = compiledScript.getEngine

  def evaluate(sentence: String, data: AnalyzersDataInternal): Result = {

    // bind parameters
    val bindings = engine.createBindings()
    bindings.put("sentence", sentence)
    bindings.put("analyzersDataInternal", data)

    // evaluate script
    try{
      compiledScript.eval(bindings)
    } catch {
      case e: Exception => throw ScalaJSAnalyzerExcpetion("Evaluation error: " + e.getMessage, e.getCause)
    }

    // get result
    val result = bindings.get("result") match {
      case r: Result => r
      case _ => Result(0, data)
    }

    result
  }
}
