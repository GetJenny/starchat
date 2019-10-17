package com.getjenny.starchat.analyzer.analyzers

import com.getjenny.analyzer.expressions.{AnalyzersDataInternal, Result}
import com.getjenny.starchat.analyzer.analyzers.script_support.scalajs.ScalaJSAnalyzerBuilder
import scalaz.Scalaz._

trait AbstractAnalyzer {
  def evaluate(sentence: String, data: AnalyzersDataInternal = AnalyzersDataInternal()): Result
}

trait AbstractAnalyzerBuilder {
  def build(command: String, restrictedArgs: Map[String, String]): AbstractAnalyzer
}

object ScriptEngines extends Enumeration {
  val SCALAJS,
      GJANALYZERS = ScriptEngines.Value

  def value(v: String) = values.find(_.toString === v).getOrElse(GJANALYZERS)
}

object AnalyzerBuilderFactory {

  def get(scriptEngine: ScriptEngines.Value): AbstractAnalyzerBuilder = scriptEngine match {
    case ScriptEngines.SCALAJS => ScalaJSAnalyzerBuilder
    case ScriptEngines.GJANALYZERS => GetJennyAnalyzerBuilder
    case _ => GetJennyAnalyzerBuilder
  }

}

