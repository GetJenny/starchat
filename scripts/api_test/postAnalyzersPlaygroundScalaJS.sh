#!/usr/bin/env bash

# analyzer returns random score when traversed states is empty, else 0. Stores query length in extracted variables
PORT=${1:-8888}
INDEX_NAME=${2:-index_getjenny_english_0}
curl -H "Authorization: Basic $(echo -n 'admin:adminp4ssw0rd' | base64)" \
  -H "Content-Type: application/json" -X POST http://localhost:${PORT}/${INDEX_NAME}/analyzer/playground -d '
{
	"analyzer": "type/SCALAJS\nimport scala.scalajs.js.Dynamic.{global => g}\n\nobject ScalaJSAnalyzer {\n\n  val AnalyzersDataInternal = g.Java.applyDynamic(\"type\")(\"com.getjenny.analyzer.expressions.AnalyzersDataInternal\")\n  val Result = g.Java.applyDynamic(\"type\")(\"com.getjenny.analyzer.expressions.Result\")\n\n  def main(args: Array[String]): Unit = {\n    val pattern = \"\"\"my name is (\\w+)\"\"\".r\n\n    // get sentence and analyzersDataInternal from global scope\n    val sentence = g.sentence.asInstanceOf[String]\n\n    val name = pattern.findFirstMatchIn(sentence) match {\n      case Some(m) => m.group(1)\n      case None => \"\"\n    }\n\n    val analyzersDataInternal = g.analyzersDataInternal\n\n    val data = analyzersDataInternal.applyDynamic(\"data\")()\n    val context = analyzersDataInternal.applyDynamic(\"context\")()\n    val traversedStates = analyzersDataInternal.applyDynamic(\"traversedStates\")()\n    val (extractedVariables, score) = if(name == \"\"){\n      (analyzersDataInternal.applyDynamic(\"extractedVariables\")(), 0)\n    } else {\n      (analyzersDataInternal.applyDynamic(\"extractedVariables\")()\n        .applyDynamic(\"updated\")(\"name\", name), 1)\n    }\n\n    // create new AnalyzersDataInternal object\n    val dataInternal = AnalyzersDataInternal.applyDynamic(\"apply\")(context, traversedStates, extractedVariables, data)\n\n    // create new Result object\n    val result = Result.applyDynamic(\"apply\")(score, dataInternal)\n\n    g.result = result\n\n  }\n}\n",
	"query": "my name is Jenny",
	"data": {
	    "traversedStates": [],
        "extractedVariables": {}
    }
}
'

