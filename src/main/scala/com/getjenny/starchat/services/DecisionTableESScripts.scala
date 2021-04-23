package com.getjenny.starchat.services

import org.elasticsearch.script.{Script, ScriptType}
import scala.collection.JavaConverters._

trait DecisionTableESScripts {

  private[this] def nestedNgramSearchScriptBody(field: String): String = s"""
                // preparing dictionary
                HashSet tokenSet = new HashSet();
                for (int i = 0; i < doc['$field'].length; ++i) {
                  String value = doc['$field'][i];
                  tokenSet.add(value);
                }
                for (int i = 0; i < params['tokens'].length; ++i) {
                  String value = params['tokens'][i];
                  tokenSet.add(value);
                }
                HashMap userQueryMap = new LinkedHashMap();
                HashMap storedQueryMap = new LinkedHashMap();
                Iterator tokenSetIterator = tokenSet.iterator();
                while(tokenSetIterator.hasNext()){
                  String value = tokenSetIterator.next();
                  userQueryMap.put(value, 0.0d);
                  storedQueryMap.put(value, 0.0d);
                }
                // wordcount
                for (int i = 0; i < doc['$field'].length; ++i) {
                  String value = doc['$field'][i];
                  storedQueryMap.computeIfPresent(value, (k, v) -> (v === null) ? 0.0d : v + 1.0d);
                }
                for (int i = 0 ; i < params['tokens'].length; ++i) {
                  String value = params['tokens'][i];
                  userQueryMap.computeIfPresent(value, (k, v) -> (v === null) ? 0.0d : v + 1.0d);
                }
                // cosine distance
                double AiBi = 0.0d;
                double Ai = 0.0d;
                double Bi = 0.0d;
                Iterator it1 = userQueryMap.entrySet().iterator();
                Iterator it2 = storedQueryMap.entrySet().iterator();
                while (it1.hasNext() && it2.hasNext()) {
                    Map.Entry pair1 = (Map.Entry)it1.next();
                    Map.Entry pair2 = (Map.Entry)it2.next();
                    double v1 = pair1.getValue();
                    double v2 = pair2.getValue();
                    AiBi += v1 * v2;
                    Ai += v1 * v1;
                    Bi += v2 * v2;
                }
                return 1.0d - (1.0d - (AiBi/(Math.sqrt(Ai) * Math.sqrt(Bi))))/2.0d;
                """

  private[this] def reindexScriptBody(instance: String): String =
    s"""ctx._source.instance = "${instance}" ; ctx._id = "${instance}" + "|" + ctx._source.state ;"""

  def reindexScript(instance: String): Script = {
    new Script(reindexScriptBody(instance))
  }

  def nestedNgramSearchScript(field: String, params: Map[String, Object]): Script = {
    new Script(ScriptType.INLINE, "painless",
      nestedNgramSearchScriptBody(field), params.asJava)
  }

}

