package com.getjenny.starchat.analyzer.analyzers

class GetJennyAnalyzer(command: String, restrictedArgs: Map[String, String])
  extends StarChatAnalyzer(command, restrictedArgs) with AbstractAnalyzer

object GetJennyAnalyzerBuilder extends AbstractAnalyzerBuilder {
  def build(script: String, restrictedArgs: Map[String, String]): AbstractAnalyzer =
    new GetJennyAnalyzer(script, restrictedArgs)
}
