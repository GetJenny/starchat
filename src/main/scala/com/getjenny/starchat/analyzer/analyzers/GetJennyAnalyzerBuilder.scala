package com.getjenny.starchat.analyzer.analyzers

object GetJennyAnalyzerBuilder extends AnalyzerAbstractBuilder {
  def build(script: String, restrictedArgs: Map[String, String]): AbstractAnalyzer =
    new StarChatAnalyzer(script, restrictedArgs)
}
