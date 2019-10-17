
val AnalyzersDataInternal = g.Java.applyDynamic("type")("com.getjenny.analyzer.expressions.AnalyzersDataInternal")
val Result = g.Java.applyDynamic("type")("com.getjenny.analyzer.expressions.Result")

val pattern = """my name is (\w+)""".r

// apply regex
val name = pattern.findFirstMatchIn(sentence) match {
  case Some(m) => m.group(1)
  case None => ""
}

val data = analyzersDataInternal.applyDynamic("data")()
val context = analyzersDataInternal.applyDynamic("context")()
val traversedStates = analyzersDataInternal.applyDynamic("traversedStates")()

// if name found, store it!
val (extractedVariables, score) = if(name == ""){
  (analyzersDataInternal.applyDynamic("extractedVariables")(), 0)
} else {
  (analyzersDataInternal.applyDynamic("extractedVariables")()
    .applyDynamic("updated")("name", name), 1)
}

// create new AnalyzersDataInternal object
val dataInternal = AnalyzersDataInternal.applyDynamic("apply")(context, traversedStates, extractedVariables, data)

// create new Result object
Result.applyDynamic("apply")(score, dataInternal)

