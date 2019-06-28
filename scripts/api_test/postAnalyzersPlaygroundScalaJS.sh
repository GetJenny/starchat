#!/usr/bin/env bash

# analyzer returns random score when traversed states is empty, else 0. Stores query length in extracted variables
PORT=${1:-8888}
INDEX_NAME=${2:-index_getjenny_english_0}
curl -H "Authorization: Basic $(echo -n 'admin:adminp4ssw0rd' | base64)" \
  -H "Content-Type: application/json" -X POST http://localhost:${PORT}/${INDEX_NAME}/analyzer/playground -d '
{
	"analyzer": "type/SCALAJS\nimport scala.util.Random\nimport scala.scalajs.js.Dynamic.{ global => g }\n\nobject ScalaJSAnalyzer {\n\n  def main(args: Array[String]): Unit = {\n\n    // get sentence and analyzersDataInternal from global scope\n    val sentence = g.sentence.asInstanceOf[String]\n    val analyzersDataInternal = g.analyzersDataInternal\n\n    // can access Java objects also\n    val Math = g.Java.applyDynamic(\"type\")(\"java.lang.Math\")\n    val pi = Math.PI\n\n    // read content from the analyzersDataInternal and make updates\n    val traversedStates = analyzersDataInternal.applyDynamic(\"traversedStates\")()\n    val extractedVariables = analyzersDataInternal.applyDynamic(\"extractedVariables\")()\n      .applyDynamic(\"updated\")(\"sentence_length\", sentence.length.toString)\n    val data = analyzersDataInternal.applyDynamic(\"data\")()\n      .applyDynamic(\"updated\")(\"pi\", pi)\n\n    // create new AnalyzersDataInternal object\n    val AnalyzersDataInternal = g.Java.applyDynamic(\"type\")(\"com.getjenny.analyzer.expressions.AnalyzersDataInternal\")\n    val dataInternal = AnalyzersDataInternal.applyDynamic(\"apply\")(traversedStates, extractedVariables, data)\n\n    // give random score if traversedStates is empty\n    val score = traversedStates.applyDynamic(\"isEmpty\")().asInstanceOf[Boolean] match {\n      case true => Random.nextDouble()\n      case false => 0\n    }\n\n    // create new Result object\n    val Result = g.Java.applyDynamic(\"type\")(\"com.getjenny.analyzer.expressions.Result\")\n    val result = Result.applyDynamic(\"apply\")(score, dataInternal)\n\n    g.result = result\n\n  }\n}",
	"query": "this is a test",
	"data": {
	    "traversedStates": ["some"],
        "extractedVariables": {}
    }
}
'

