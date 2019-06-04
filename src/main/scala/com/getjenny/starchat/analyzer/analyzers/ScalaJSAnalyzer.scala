package com.getjenny.starchat.analyzer.analyzers
import com.getjenny.analyzer.expressions.{AnalyzersDataInternal, Result}
import javax.script.{CompiledScript, ScriptEngine}

class ScalaJSAnalyzer(compiledScript: CompiledScript) extends AbstractAnalyzer {

  val engine: ScriptEngine = compiledScript.getEngine

  def evaluate(sentence: String, data: AnalyzersDataInternal): Result = {

    // bind parameters
    val bindings = engine.createBindings()
    bindings.put("sentence", sentence)
    bindings.put("analyzersDataInternal", data)

    // evaluate script
    compiledScript.eval(bindings) // can fail

    // get result
    val result = bindings.get("result") match {
      case r: Result => r
      case _ => Result(0, data) // no result from the script
    }

    result
  }
}
