package com.getjenny.starchat.services

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 01/07/16.
  */

import java.io.File

import akka.event.{Logging, LoggingAdapter}
import com.getjenny.starchat.SCActorSystem
import com.getjenny.starchat.analyzer.utils.TokenToVector
import com.getjenny.starchat.entities._
import com.getjenny.starchat.services.esclient.DecisionTableElasticClient
import com.getjenny.starchat.utils.Index
import org.apache.lucene.search.join._
import org.elasticsearch.action.get.{GetResponse, MultiGetItemResponse, MultiGetRequest, _}
import org.elasticsearch.action.index.{IndexRequest, IndexResponse}
import org.elasticsearch.action.search.{SearchRequest, SearchResponse, SearchType}
import org.elasticsearch.action.update.{UpdateRequest, UpdateResponse}
import org.elasticsearch.client.{RequestOptions, RestHighLevelClient}
import org.elasticsearch.common.unit.TimeValue
import org.elasticsearch.common.xcontent.XContentBuilder
import org.elasticsearch.common.xcontent.XContentFactory._
import org.elasticsearch.index.query.{BoolQueryBuilder, InnerHitBuilder, QueryBuilder, QueryBuilders}
import org.elasticsearch.rest.RestStatus
import org.elasticsearch.script.Script
import org.elasticsearch.search.SearchHit
import org.elasticsearch.search.aggregations.AggregationBuilders
import org.elasticsearch.search.aggregations.bucket.nested.ParsedNested
import org.elasticsearch.search.aggregations.bucket.terms.ParsedStringTerms
import org.elasticsearch.search.builder.SearchSourceBuilder
import scalaz.Scalaz._

import scala.collection.JavaConverters._
import scala.collection.immutable.{List, Map}
import scala.concurrent.Future
import com.getjenny.starchat.utils.Base64

case class DecisionTableServiceException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)

/**
  * Implements functions, eventually used by DecisionTableResource, for searching, get next response etc
  */
object DecisionTableService extends AbstractDataService {
  override val elasticClient: DecisionTableElasticClient.type = DecisionTableElasticClient
  private[this] val termService: TermService.type = TermService
  private[this] val log: LoggingAdapter = Logging(SCActorSystem.system, this.getClass.getCanonicalName)

  private[this] val queriesScoreMode: Map[String, ScoreMode] =
    Map[String, ScoreMode]("min" -> ScoreMode.Min,
      "max" -> ScoreMode.Max, "avg" -> ScoreMode.Avg, "total" -> ScoreMode.Total)

  private[this] def responseToDtDocumentDefault(searchResponse : SearchResponse):
  Option[List[SearchDTDocument]] = Some {
    searchResponse.getHits.getHits.toList.map {
      item: SearchHit =>
        val state : String = item.getId

        val version: Option[Long] = Some(item.getVersion)

        val source : Map[String, Any] = item.getSourceAsMap.asScala.toMap

        val executionOrder: Int = source.get("execution_order") match {
          case Some(t) => t.asInstanceOf[Int]
          case None => 0
        }

        val maxStateCount : Int = source.get("max_state_count") match {
          case Some(t) => t.asInstanceOf[Int]
          case None => 0
        }

        val analyzer : String = source.get("analyzer") match {
          case Some(t) => Base64.decode(t.asInstanceOf[String])
          case None => ""
        }

        val queries : List[String] = source.get("queries") match {
          case Some(t) =>
            val offsets = item.getInnerHits.get("queries").getHits.toList.map(innerHit => {
              innerHit.getNestedIdentity.getOffset
            })
            val query_array = t.asInstanceOf[java.util.ArrayList[java.util.HashMap[String, String]]].asScala.toList
              .map(q_e => q_e.get("query"))
            val queriesOrdered : List[String] = offsets.map(i => query_array(i))
            queriesOrdered
          case None => List.empty[String]
        }

        val bubble : String = source.get("bubble") match {
          case Some(t) => t.asInstanceOf[String]
          case None => ""
        }

        val action : String = source.get("action") match {
          case Some(t) => t.asInstanceOf[String]
          case None => ""
        }

        val actionInput : Map[String,String] = source.get("action_input") match {
          case Some(null) => Map[String, String]()
          case Some(t) => t.asInstanceOf[java.util.HashMap[String,String]].asScala.toMap
          case None => Map[String, String]()
        }

        val stateData : Map[String,String] = source.get("state_data") match {
          case Some(null) => Map[String, String]()
          case Some(t) => t.asInstanceOf[java.util.HashMap[String,String]].asScala.toMap
          case None => Map[String, String]()
        }

        val successValue : String = source.get("success_value") match {
          case Some(t) => t.asInstanceOf[String]
          case None => ""
        }

        val failureValue : String = source.get("failure_value") match {
          case Some(t) => t.asInstanceOf[String]
          case None => ""
        }

        val evaluationClass : String = source.get("evaluation_class") match {
          case Some(t) => t.asInstanceOf[String]
          case None => "default"
        }

        val document : DTDocument = DTDocument(state = state, executionOrder = executionOrder,
          maxStateCount = maxStateCount,
          analyzer = analyzer, queries = queries, bubble = bubble,
          action = action, actionInput = actionInput, stateData = stateData,
          successValue = successValue, failureValue = failureValue,
          evaluationClass = Some(evaluationClass), version = version
        )

        val searchDocument : SearchDTDocument = SearchDTDocument(score = item.getScore, document = document)
        searchDocument
    }
  }

  private[this] def responseToDtDocumentNGrams(indexName: String, analyzer: String,
                                               documentSearch: DTDocumentSearch)(searchResponse : SearchResponse):
  Option[List[SearchDTDocument]] = Some {

    val tokenizerRequest = TokenizerQueryRequest(tokenizer = analyzer, text = documentSearch.queries.getOrElse(""))
    val tokenizerResponse = termService.esTokenizer(indexName, tokenizerRequest)
    val queryTokens = tokenizerResponse.tokens.map(_.token)

    val searchAlgorithm = documentSearch.searchAlgorithm.getOrElse(SearchAlgorithm.NGRAM2)
    val sliding = searchAlgorithm match {
      case SearchAlgorithm.STEM_NGRAM2 | SearchAlgorithm.NGRAM2 => 2
      case SearchAlgorithm.STEM_NGRAM3 | SearchAlgorithm.NGRAM3 => 3
      case SearchAlgorithm.STEM_NGRAM4 | SearchAlgorithm.NGRAM4 => 4
      case _ => 2
    }

    val dtDocuments = searchResponse.getHits.getHits.toList.map {
      item: SearchHit =>
        val state : String = item.getId

        val version: Option[Long] = Some(item.getVersion)

        val source : Map[String, Any] = item.getSourceAsMap.asScala.toMap

        val executionOrder: Int = source.get("execution_order") match {
          case Some(t) => t.asInstanceOf[Int]
          case None => 0
        }

        val maxStateCount : Int = source.get("max_state_count") match {
          case Some(t) => t.asInstanceOf[Int]
          case None => 0
        }

        val analyzer : String = source.get("analyzer") match {
          case Some(t) => Base64.decode(t.asInstanceOf[String])
          case None => ""
        }

        val queriesAndNgrams : List[(String, List[String])] = source.get("queries") match {
          case Some(t) =>
            val offsetsAndNgrams = item.getInnerHits.get("queries").getHits.toList.map(innerHit => {
              innerHit.getNestedIdentity.getOffset
            })
            val queryArray = t.asInstanceOf[java.util.ArrayList[java.util.HashMap[String, String]]].asScala.toList
              .map(q_e => q_e.get("query"))
            offsetsAndNgrams.map{ e  =>
              val qNgrams = queryArray(e).toLowerCase().replaceAll("\\s", "").sliding(sliding).toList
              (queryArray(e), qNgrams)
            }
          case None => List.empty[(String, List[String])]
        }

        val (queries, ngrams) = queriesAndNgrams.unzip

        val bubble : String = source.get("bubble") match {
          case Some(t) => t.asInstanceOf[String]
          case None => ""
        }

        val action : String = source.get("action") match {
          case Some(t) => t.asInstanceOf[String]
          case None => ""
        }

        val actionInput : Map[String,String] = source.get("action_input") match {
          case Some(null) => Map[String, String]()
          case Some(t) => t.asInstanceOf[java.util.HashMap[String,String]].asScala.toMap
          case None => Map[String, String]()
        }

        val stateData : Map[String,String] = source.get("state_data") match {
          case Some(null) => Map[String, String]()
          case Some(t) => t.asInstanceOf[java.util.HashMap[String,String]].asScala.toMap
          case None => Map[String, String]()
        }

        val successValue : String = source.get("success_value") match {
          case Some(t) => t.asInstanceOf[String]
          case None => ""
        }

        val failureValue : String = source.get("failure_value") match {
          case Some(t) => t.asInstanceOf[String]
          case None => ""
        }

        val evaluationClass : String = source.get("evaluation_class") match {
          case Some(t) => t.asInstanceOf[String]
          case None => "default"
        }

        val document : DTDocument = DTDocument(state = state, executionOrder = executionOrder,
          maxStateCount = maxStateCount,
          analyzer = analyzer, queries = queries, bubble = bubble,
          action = action, actionInput = actionInput, stateData = stateData,
          successValue = successValue, failureValue = failureValue,
          evaluationClass = Some(evaluationClass), version = version
        )

        val searchDocument : SearchDTDocument = SearchDTDocument(score = item.getScore, document = document)
        (searchDocument, ngrams)
    }

    val ngramsIndex = (dtDocuments.flatMap{ case (_, ngrams) => ngrams }
      .flatten ++ queryTokens).distinct.zipWithIndex.toMap

    val queryVector = TokenToVector.tokensToVector(queryTokens, ngramsIndex)

    dtDocuments.map{ case (searchDocument, queriesNgrams) =>
      val score = queriesNgrams.map(ngrams => {
        1.0 - TokenToVector.cosineDist(queryVector, TokenToVector.tokensToVector(ngrams, ngramsIndex))
      }).max
      (searchDocument, score)
    }.map{ case(searchDtDocument, score) =>
      val document : DTDocument = searchDtDocument.document
      SearchDTDocument(score = score.toFloat, document = document)
    }
  }

  private[this] def documentSearchQueries(indexName: String,
                                          value: String, minScore: Float,
                                          boostExactMatchFactor: Float,
                                          documentSearch: DTDocumentSearch
                                         ): (QueryBuilder, SearchResponse => Option[List[SearchDTDocument]]) = {
    val searchAlgorithm = documentSearch.searchAlgorithm.getOrElse(SearchAlgorithm.DEFAULT)
    searchAlgorithm match {
      case SearchAlgorithm.AUTO | SearchAlgorithm.DEFAULT =>
        val (scriptBody, matchQueryEs, analyzer, algorithm) = if(documentSearch.queries.getOrElse("").length > 3) {
          (
            "return doc['queries.query.ngram_3'] ;",
            "queries.query.ngram_3",
            "ngram3",
            SearchAlgorithm.NGRAM3

          )
        } else {
          (
            "return doc['queries.query.ngram_2'] ;",
            "queries.query.ngram_2",
            "ngram2",
            SearchAlgorithm.NGRAM2
          )
        }

        val modDocumentSearch = documentSearch.copy(
          searchAlgorithm = Some(algorithm)
        )
        val script: Script = new Script(scriptBody)
        (QueryBuilders.nestedQuery(
          "queries",
          QueryBuilders.boolQuery()
            .must(QueryBuilders.matchQuery(matchQueryEs, value)),
          queriesScoreMode.getOrElse(elasticClient.queriesScoreMode, ScoreMode.Max)
        ).ignoreUnmapped(true)
          .innerHit(new InnerHitBuilder().addScriptField("terms", script).setSize(100)),
          responseToDtDocumentNGrams(indexName, analyzer, modDocumentSearch))
      case SearchAlgorithm.STEM_BOOST_EXACT =>
        (
          QueryBuilders.nestedQuery(
            "queries",
            QueryBuilders.boolQuery()
              .must(QueryBuilders.matchQuery("queries.query.stem", value))
              .should(QueryBuilders.matchPhraseQuery("queries.query.raw", value)
                .boost(1 + (minScore * boostExactMatchFactor))
              ),
            queriesScoreMode.getOrElse(elasticClient.queriesScoreMode, ScoreMode.Max)
          ).ignoreUnmapped(true).innerHit(new InnerHitBuilder().setSize(100)),
          responseToDtDocumentDefault)
      case SearchAlgorithm.SHINGLES2 =>
        (QueryBuilders.nestedQuery(
          "queries",
          QueryBuilders.boolQuery()
            .must(QueryBuilders.matchQuery("queries.query.shingles_2", value)),
          queriesScoreMode.getOrElse(elasticClient.queriesScoreMode, ScoreMode.Max)
        ).ignoreUnmapped(true).innerHit(new InnerHitBuilder().setSize(100)),
          responseToDtDocumentDefault)
      case SearchAlgorithm.SHINGLES3 =>
        (QueryBuilders.nestedQuery(
          "queries",
          QueryBuilders.boolQuery()
            .must(QueryBuilders.matchQuery("queries.query.shingles_3", value)),
          queriesScoreMode.getOrElse(elasticClient.queriesScoreMode, ScoreMode.Max)
        ).ignoreUnmapped(true).innerHit(new InnerHitBuilder().setSize(100)),
          responseToDtDocumentDefault)
      case SearchAlgorithm.SHINGLES4 =>
        (QueryBuilders.nestedQuery(
          "queries",
          QueryBuilders.boolQuery()
            .must(QueryBuilders.matchQuery("queries.query.shingles_4", value)),
          queriesScoreMode.getOrElse(elasticClient.queriesScoreMode, ScoreMode.Max)
        ).ignoreUnmapped(true).innerHit(new InnerHitBuilder().setSize(100)),
          responseToDtDocumentDefault)
      case SearchAlgorithm.STEM_SHINGLES2 =>
        (QueryBuilders.nestedQuery(
          "queries",
          QueryBuilders.boolQuery()
            .must(QueryBuilders.matchQuery("queries.query.stemmed_shingles_2", value)),
          queriesScoreMode.getOrElse(elasticClient.queriesScoreMode, ScoreMode.Max)
        ).ignoreUnmapped(true).innerHit(new InnerHitBuilder().setSize(100)),
          responseToDtDocumentDefault)
      case SearchAlgorithm.STEM_SHINGLES3 =>
        (QueryBuilders.nestedQuery(
          "queries",
          QueryBuilders.boolQuery()
            .must(QueryBuilders.matchQuery("queries.query.stemmed_shingles_3", value)),
          queriesScoreMode.getOrElse(elasticClient.queriesScoreMode, ScoreMode.Max)
        ).ignoreUnmapped(true).innerHit(new InnerHitBuilder().setSize(100)),
          responseToDtDocumentDefault)
      case SearchAlgorithm.STEM_SHINGLES4 =>
        (QueryBuilders.nestedQuery(
          "queries",
          QueryBuilders.boolQuery()
            .must(QueryBuilders.matchQuery("queries.query.stemmed_shingles_4", value)),
          queriesScoreMode.getOrElse(elasticClient.queriesScoreMode, ScoreMode.Max)
        ).ignoreUnmapped(true).innerHit(new InnerHitBuilder().setSize(100)),
          responseToDtDocumentDefault)
      case SearchAlgorithm.NGRAM2 =>
        val scriptBody = "return doc['queries.query.ngram_2'] ;"
        val script: Script = new Script(scriptBody)
        (QueryBuilders.nestedQuery(
          "queries",
          QueryBuilders.boolQuery()
            .must(QueryBuilders.matchQuery("queries.query.ngram_2", value)),
          queriesScoreMode.getOrElse(elasticClient.queriesScoreMode, ScoreMode.Max)
        ).ignoreUnmapped(true)
          .innerHit(new InnerHitBuilder().addScriptField("terms", script).setSize(100)),
          responseToDtDocumentNGrams(indexName, "ngram2", documentSearch))
      case SearchAlgorithm.STEM_NGRAM2 =>
        val scriptBody = "return doc['queries.query.stemmed_ngram_2'] ;"
        val script: Script = new Script(scriptBody)
        (QueryBuilders.nestedQuery(
          "queries",
          QueryBuilders.boolQuery()
            .must(QueryBuilders.matchQuery("queries.query.stemmed_ngram_2", value)),
          queriesScoreMode.getOrElse(elasticClient.queriesScoreMode, ScoreMode.Max)
        ).ignoreUnmapped(true)
          .innerHit(new InnerHitBuilder().addScriptField("terms", script).setSize(100)),
          responseToDtDocumentNGrams(indexName, "stemmed_ngram2", documentSearch))
      case SearchAlgorithm.NGRAM3 =>
        val scriptBody = "return doc['queries.query.ngram_3'] ;"
        val script: Script = new Script(scriptBody)
        (QueryBuilders.nestedQuery(
          "queries",
          QueryBuilders.boolQuery()
            .must(QueryBuilders.matchQuery("queries.query.ngram_3", value)),
          queriesScoreMode.getOrElse(elasticClient.queriesScoreMode, ScoreMode.Max)
        ).ignoreUnmapped(true)
          .innerHit(new InnerHitBuilder().addScriptField("terms", script).setSize(100)),
          responseToDtDocumentNGrams(indexName, "ngram3", documentSearch))
      case SearchAlgorithm.STEM_NGRAM3 =>
        val scriptBody = "return doc['queries.query.stemmed_ngram_3'] ;"
        val script: Script = new Script(scriptBody)
        (QueryBuilders.nestedQuery(
          "queries",
          QueryBuilders.boolQuery()
            .must(QueryBuilders.matchQuery("queries.query.stemmed_ngram_3", value)),
          queriesScoreMode.getOrElse(elasticClient.queriesScoreMode, ScoreMode.Max)
        ).ignoreUnmapped(true)
          .innerHit(new InnerHitBuilder().addScriptField("terms", script).setSize(100)),
          responseToDtDocumentNGrams(indexName, "stemmed_ngram3", documentSearch))
      case SearchAlgorithm.NGRAM4 =>
        val scriptBody = "return doc['queries.query.ngram_4'] ;"
        val script: Script = new Script(scriptBody)
        (QueryBuilders.nestedQuery(
          "queries",
          QueryBuilders.boolQuery()
            .must(QueryBuilders.matchQuery("queries.query.ngram_4", value)),
          queriesScoreMode.getOrElse(elasticClient.queriesScoreMode, ScoreMode.Max)
        ).ignoreUnmapped(true)
          .innerHit(new InnerHitBuilder().addScriptField("terms", script).setSize(100)),
          responseToDtDocumentNGrams(indexName, "ngram4", documentSearch))
      case SearchAlgorithm.STEM_NGRAM4 =>
        val scriptBody = "return doc['queries.query.stemmed_ngram_4'] ;"
        val script: Script = new Script(scriptBody)
        (QueryBuilders.nestedQuery(
          "queries",
          QueryBuilders.boolQuery()
            .must(QueryBuilders.matchQuery("queries.query.stemmed_ngram_4", value)),
          queriesScoreMode.getOrElse(elasticClient.queriesScoreMode, ScoreMode.Max)
        ).ignoreUnmapped(true)
          .innerHit(new InnerHitBuilder().addScriptField("terms", script).setSize(100)),
          responseToDtDocumentNGrams(indexName, "stemmed_ngram4", documentSearch))
      case _ => throw DecisionTableServiceException("Unknown SearchAlgorithm: " + searchAlgorithm)
    }
  }

  def search(indexName: String, documentSearch: DTDocumentSearch): Future[SearchDTDocumentsResults] = {
    val client: RestHighLevelClient = elasticClient.httpClient

    val sourceReq: SearchSourceBuilder = new SearchSourceBuilder()
      .version(true)

    val searchReq = new SearchRequest(Index.indexName(indexName, elasticClient.indexSuffix))
      .source(sourceReq)
      .searchType(SearchType.DFS_QUERY_THEN_FETCH)

    val minScore = documentSearch.minScore.getOrElse(
      Option{elasticClient.queryMinThreshold}.getOrElse(0.0f)
    )

    val boostExactMatchFactor = documentSearch.boostExactMatchFactor.getOrElse(
      Option{elasticClient.boostExactMatchFactor}.getOrElse(1.0f)
    )

    sourceReq.minScore(minScore)

    val boolQueryBuilder : BoolQueryBuilder = QueryBuilders.boolQuery()

    documentSearch.state match {
      case Some(value) => boolQueryBuilder.must(QueryBuilders.termQuery("state", value))
      case _ => ;
    }

    documentSearch.executionOrder match {
      case Some(value) =>
        boolQueryBuilder.must(QueryBuilders.matchQuery("execution_order", value))
      case _ => ;
    }

    val resToDtDocs: SearchResponse => Option[List[SearchDTDocument]] = documentSearch.queries match {
      case Some(value) =>
        val (nestedQuery, resToSearchDtDocs) =
          documentSearchQueries(indexName, value, minScore, boostExactMatchFactor, documentSearch)
        boolQueryBuilder.must(nestedQuery)
        resToSearchDtDocs
      case _ => responseToDtDocumentDefault
    }

    sourceReq.query(boolQueryBuilder)

    sourceReq.from(documentSearch.from.getOrElse(0))
      .size(documentSearch.size.getOrElse(10))

    val searchResponse : SearchResponse = client.search(searchReq, RequestOptions.DEFAULT)

    val documents : Option[List[SearchDTDocument]] = resToDtDocs(searchResponse)

    val filteredDoc : List[SearchDTDocument] = documents.getOrElse(List[SearchDTDocument]())

    val maxScore : Float = if(filteredDoc.nonEmpty) {
      filteredDoc.maxBy(_.score).score
    } else {
      0.0f
    }

    val total : Int = filteredDoc.length
    val searchResults : SearchDTDocumentsResults = SearchDTDocumentsResults(total = total, maxScore = maxScore,
      hits = filteredDoc)

    val searchResultsOption : Future[SearchDTDocumentsResults] = Future { searchResults }
    searchResultsOption
  }

  def searchDtQueries(indexName: String,
                      analyzerEvaluateRequest: AnalyzerEvaluateRequest): Future[SearchDTDocumentsResults] = {
    val dtDocumentSearch: DTDocumentSearch =
      DTDocumentSearch(
        from = Option {0},
        size = Option {10000},
        minScore = Option {
          elasticClient.queryMinThreshold
        },
        executionOrder = None: Option[Int],
        boostExactMatchFactor = Option {
          elasticClient.boostExactMatchFactor
        },
        state = None: Option[String],
        queries = Option {
          analyzerEvaluateRequest.query
        },
        searchAlgorithm = analyzerEvaluateRequest.searchAlgorithm,
        evaluationClass = analyzerEvaluateRequest.evaluationClass
      )

    this.search(indexName, dtDocumentSearch)
  }

  def resultsToMap(results: SearchDTDocumentsResults): Map[String, Any] = {
    Map("dt_queries_search_result" ->
      results.hits.map {
        doc =>
          (doc.document.state, (doc.score, doc))
      }.toMap
    )
  }

  def createFuture(indexName: String, document: DTDocument, refresh: Int): Future[IndexDocumentResult] = Future {
    create(indexName, document, refresh)
  }

  def create(indexName: String, document: DTDocument, refresh: Int): IndexDocumentResult = {
    val builder : XContentBuilder = jsonBuilder().startObject()

    builder.field("state", document.state)
    builder.field("execution_order", document.executionOrder)
    builder.field("max_state_count", document.maxStateCount)
    builder.field("analyzer", Base64.encode(document.analyzer))

    val array = builder.startArray("queries")
    document.queries.foreach(q => {
      array.startObject().field("query", q).endObject()
    })
    array.endArray()

    builder.field("bubble", document.bubble)
    builder.field("action", document.action)

    val actionInputBuilder : XContentBuilder = builder.startObject("action_input")
    for ((k,v) <- document.actionInput) actionInputBuilder.field(k,v)
    actionInputBuilder.endObject()

    val stateDataBuilder : XContentBuilder = builder.startObject("state_data")
    for ((k,v) <- document.stateData) stateDataBuilder.field(k,v)
    stateDataBuilder.endObject()

    builder.field("success_value", document.successValue)
    builder.field("failure_value", document.failureValue)
    val evaluationClass = document.evaluationClass match {
      case Some(t) => t
      case _ => "default"
    }
    builder.field("evaluation_class", evaluationClass)
    builder.endObject()

    val client: RestHighLevelClient = elasticClient.httpClient

    val indexReq = new IndexRequest()
      .index(Index.indexName(indexName, elasticClient.indexSuffix))
      .id(document.state)
      .source(builder)

    val response: IndexResponse = client.index(indexReq, RequestOptions.DEFAULT)

    if (refresh =/= 0) {
      val refreshIndex = elasticClient.refresh(Index.indexName(indexName, elasticClient.indexSuffix))
      if(refreshIndex.failedShardsN > 0) {
        throw new Exception("DecisionTable : index refresh failed: (" + indexName + ")")
      }
    }

    val docResult: IndexDocumentResult = IndexDocumentResult(index = response.getIndex,
      id = response.getId,
      version = response.getVersion,
      created = response.status === RestStatus.CREATED
    )

    docResult
  }

  def update(indexName: String, document: DTDocumentUpdate, refresh: Int):
  Future[UpdateDocumentResult] = Future {
    val builder : XContentBuilder = jsonBuilder().startObject()

    document.analyzer match {
      case Some(t) => builder.field("analyzer", Base64.encode(t))
      case None => ;
    }

    document.executionOrder match {
      case Some(t) => builder.field("execution_order", t)
      case None => ;
    }
    document.maxStateCount match {
      case Some(t) => builder.field("max_state_count", t)
      case None => ;
    }
    document.queries match {
      case Some(t) =>
        val array = builder.startArray("queries")
        t.foreach(q => {
          array.startObject().field("query", q).endObject()
        })
        array.endArray()
      case None => ;
    }
    document.bubble match {
      case Some(t) => builder.field("bubble", t)
      case None => ;
    }
    document.action match {
      case Some(t) => builder.field("action", t)
      case None => ;
    }
    document.actionInput match {
      case Some(t) =>
        if(t.nonEmpty) {
          val actionInputBuilder : XContentBuilder = builder.startObject("action_input")
          for ((k,v) <- t) actionInputBuilder.field(k,v)
          actionInputBuilder.endObject()
        } else {
          builder.nullField("action_input")
        }
      case None => ;
    }
    document.stateData match {
      case Some(t) =>
        val stateDataBuilder : XContentBuilder = builder.startObject("state_data")
        for ((k,v) <- t) stateDataBuilder.field(k,v)
        stateDataBuilder.endObject()
      case None => ;
    }
    document.successValue match {
      case Some(t) => builder.field("success_value", t)
      case None => ;
    }
    document.failureValue match {
      case Some(t) => builder.field("failure_value", t)
      case None => ;
    }
    document.evaluationClass match {
      case Some(t) => builder.field("evaluation_class", t)
      case None => ;
    }

    builder.endObject()

    val client: RestHighLevelClient = elasticClient.httpClient

    val updateReq = new UpdateRequest()
      .index(Index.indexName(indexName, elasticClient.indexSuffix))
      .doc(builder)
      .id(document.state)

    val response: UpdateResponse = client.update(updateReq, RequestOptions.DEFAULT)

    if (refresh =/= 0) {
      val refreshIndex = elasticClient.refresh(Index.indexName(indexName, elasticClient.indexSuffix))
      if(refreshIndex.failedShardsN > 0) {
        throw new Exception("DecisionTable : index refresh failed: (" + indexName + ")")
      }
    }

    val docResult: UpdateDocumentResult = UpdateDocumentResult(index = response.getIndex,
      id = response.getId,
      version = response.getVersion,
      created = response.status === RestStatus.CREATED
    )

    docResult
  }

  def getDTDocuments(indexName: String): Future[SearchDTDocumentsResults] = Future {
    val client: RestHighLevelClient = elasticClient.httpClient

    val sourceReq: SearchSourceBuilder = new SearchSourceBuilder()
      .query(QueryBuilders.matchAllQuery)
      .size(10000)
      .version(true)

    val searchReq = new SearchRequest(Index.indexName(indexName, elasticClient.indexSuffix))
      .source(sourceReq)
      .scroll(new TimeValue(60000))

    var scrollResp: SearchResponse = client.search(searchReq, RequestOptions.DEFAULT)

    //get a map of stateId -> AnalyzerItem (only if there is smt in the field "analyzer")
    val decisionTableContent : List[SearchDTDocument] = scrollResp.getHits.getHits.toList.map{ item =>
      val state : String = item.getId
      val version: Option[Long] = Some(item.getVersion)
      val source : Map[String, Any] = item.getSourceAsMap.asScala.toMap

      val executionOrder : Int = source.get("execution_order") match {
        case Some(t) => t.asInstanceOf[Int]
        case None => 0
      }

      val maxStateCount : Int = source.get("max_state_count") match {
        case Some(t) => t.asInstanceOf[Int]
        case None => 0
      }

      val analyzer : String = source.get("analyzer") match {
        case Some(t) => Base64.decode(t.asInstanceOf[String])
        case None => ""
      }

      val queries : List[String] = source.get("queries") match {
        case Some(t) => t.asInstanceOf[java.util.ArrayList[java.util.HashMap[String, String]]]
          .asScala.map(query =>
          query.asScala.get("query")).filter(_.nonEmpty).toList.map(_.get)
        case None => List[String]()
      }

      val bubble : String = source.get("bubble") match {
        case Some(t) => t.asInstanceOf[String]
        case None => ""
      }

      val action : String = source.get("action") match {
        case Some(t) => t.asInstanceOf[String]
        case None => ""
      }

      val actionInput : Map[String,String] = source.get("action_input") match {
        case Some(null) => Map[String, String]()
        case Some(t) => t.asInstanceOf[java.util.HashMap[String,String]].asScala.toMap
        case None => Map[String, String]()
      }

      val stateData : Map[String,String] = source.get("state_data") match {
        case Some(null) => Map[String, String]()
        case Some(t) => t.asInstanceOf[java.util.HashMap[String,String]].asScala.toMap
        case None => Map[String,String]()
      }

      val successValue : String = source.get("success_value") match {
        case Some(t) => t.asInstanceOf[String]
        case None => ""
      }

      val failureValue : String = source.get("failure_value") match {
        case Some(t) => t.asInstanceOf[String]
        case None => ""
      }

      val evaluationClass : String = source.get("evaluation_class") match {
        case Some(t) => t.asInstanceOf[String]
        case None => "default"
      }

      val document : DTDocument = DTDocument(state = state, executionOrder = executionOrder,
        maxStateCount = maxStateCount,
        analyzer = analyzer, queries = queries, bubble = bubble,
        action = action, actionInput = actionInput, stateData = stateData,
        successValue = successValue, failureValue = failureValue,
        evaluationClass = Some(evaluationClass), version = version
      )

      val searchDocument : SearchDTDocument = SearchDTDocument(score = .0f, document = document)
      searchDocument
    }.sortBy(_.document.state)

    val maxScore : Float = .0f
    val total : Int = decisionTableContent.length
    SearchDTDocumentsResults(total = total, maxScore = maxScore, hits = decisionTableContent)
  }

  def read(indexName: String, ids: List[String]): Future[SearchDTDocumentsResults] = Future {
    val client: RestHighLevelClient = elasticClient.httpClient

    val multiGetReq = new MultiGetRequest()

    // a list of specific ids was requested
    ids.foreach{id =>
      multiGetReq.add(
        new MultiGetRequest.Item(Index.indexName(indexName, elasticClient.indexSuffix), id)
      )
    }

    val response: MultiGetResponse = client.mget(multiGetReq, RequestOptions.DEFAULT)

    val documents: Option[List[SearchDTDocument]] = Option {
      response.getResponses
        .toList.filter((p: MultiGetItemResponse) => p.getResponse.isExists).map{ e =>

        val item: GetResponse = e.getResponse

        val version: Option[Long] = Some(item.getVersion)

        val state: String = item.getId

        val source: Map[String, Any] = item.getSource.asScala.toMap

        val executionOrder: Int = source.get("execution_order") match {
          case Some(t) => t.asInstanceOf[Int]
          case None => 0
        }

        val maxStateCount: Int = source.get("max_state_count") match {
          case Some(t) => t.asInstanceOf[Int]
          case None => 0
        }

        val analyzer: String = source.get("analyzer") match {
          case Some(t) => Base64.decode(t.asInstanceOf[String])
          case None => ""
        }

        val queries: List[String] = source.get("queries") match {
          case Some(t) => t.asInstanceOf[java.util.ArrayList[java.util.HashMap[String, String]]]
            .asScala.map { res => Some(res.getOrDefault("query", None.orNull)) }
            .filter(_.nonEmpty).map(_.get).toList
          case None => List[String]()
        }

        val bubble: String = source.get("bubble") match {
          case Some(t) => t.asInstanceOf[String]
          case None => ""
        }

        val action: String = source.get("action") match {
          case Some(t) => t.asInstanceOf[String]
          case None => ""
        }

        val actionInput: Map[String, String] = source.get("action_input") match {
          case Some(null) => Map[String, String]()
          case Some(t) => t.asInstanceOf[java.util.HashMap[String,String]].asScala.toMap
          case None => Map[String, String]()
        }

        val stateData: Map[String, String] = source.get("state_data") match {
          case Some(null) => Map[String, String]()
          case Some(t) => t.asInstanceOf[java.util.HashMap[String, String]].asScala.toMap
          case None => Map[String, String]()
        }

        val successValue: String = source.get("success_value") match {
          case Some(t) => t.asInstanceOf[String]
          case None => ""
        }

        val failureValue: String = source.get("failure_value") match {
          case Some(t) => t.asInstanceOf[String]
          case None => ""
        }

        val evaluationClass : String = source.get("evaluation_class") match {
          case Some(t) => t.asInstanceOf[String]
          case None => "default"
        }

        val document: DTDocument = DTDocument(state = state, executionOrder = executionOrder,
          maxStateCount = maxStateCount,
          analyzer = analyzer, queries = queries, bubble = bubble,
          action = action, actionInput = actionInput, stateData = stateData,
          successValue = successValue, failureValue = failureValue,
          evaluationClass = Some(evaluationClass), version = version
        )

        val searchDocument: SearchDTDocument = SearchDTDocument(score = .0f, document = document)
        searchDocument
      }
    }

    val filteredDoc: List[SearchDTDocument] = documents.getOrElse(List[SearchDTDocument]())

    val maxScore: Float = .0f
    val total: Int = filteredDoc.length
    SearchDTDocumentsResults(total = total, maxScore = maxScore, hits = filteredDoc)
  }

  def indexCSVFileIntoDecisionTable(indexName: String, file: File, skipLines: Int = 1, separator: Char = ','):
  Future[IndexDocumentListResult] = Future {
    val documents: IndexedSeq[DTDocument] = FileToDocuments.getDTDocumentsFromCSV(log = log, file = file,
      skipLines = skipLines, separator = separator)

    val indexDocumentListResult = documents.map(dtDocument => {
      create(indexName, dtDocument, 0)
    }).toList
    IndexDocumentListResult(data = indexDocumentListResult)
  }

  def indexJSONFileIntoDecisionTable(indexName: String, file: File):
  Future[IndexDocumentListResult] = Future {
    val documents: IndexedSeq[DTDocument] = FileToDocuments.getDTDocumentsFromJSON(log = log, file = file)

    val indexDocumentListResult = documents.map(dtDocument => {
      create(indexName, dtDocument, 0)
    }).toList
    IndexDocumentListResult(data = indexDocumentListResult)
  }


  def wordFrequenciesInQueries(indexName:String): Map[String,Double] =  {
    val client: RestHighLevelClient = elasticClient.httpClient

    val sourceReq: SearchSourceBuilder = new SearchSourceBuilder()
      .size(0)
      .minScore(0.0f)

    val searchReq = new SearchRequest(Index.indexName(indexName, elasticClient.indexSuffix))
      .source(sourceReq)
      .searchType(SearchType.DFS_QUERY_THEN_FETCH)
      .requestCache(true)



    /*   Query to fetch for word freq in queries
    {
       "size": 0,
       "aggregations" : {
           "wordInQueries": {
               "nested": { "path": "queries" },
               "aggregations": {
                   "queries_children": {
                       "terms" : { "field": "queries.query.base" }
                   }
               }

           }
        }
    } */



    sourceReq.aggregation(
      AggregationBuilders.nested("queries", "queries")
        .subAggregation(
          AggregationBuilders.terms("queries_children").field("queries.query.base").minDocCount(1)
        )
    )

    val searchResp: SearchResponse = client.search(searchReq, RequestOptions.DEFAULT)

    val parsedNested: ParsedNested = searchResp.getAggregations.get("queries")
    val nQueries: Double = parsedNested.getDocCount
    val parsedStringTerms: ParsedStringTerms = parsedNested.getAggregations.get("queries_children")
    if (nQueries > 0) {
      parsedStringTerms.getBuckets.asScala.map {
        bucket => bucket.getKeyAsString -> bucket.getDocCount / nQueries
      }.toMap
    }
    else
      Map[String,Double]()



  }


  def wordFrequenciesInQueriesByState(indexName: String): List[DTStateWordFreqsItem] = {
    val client: RestHighLevelClient = elasticClient.httpClient

    val sourceReq: SearchSourceBuilder = new SearchSourceBuilder()
      .size(0)
      .minScore(0.0f)

    val searchReq = new SearchRequest(Index.indexName(indexName, elasticClient.indexSuffix))
      .source(sourceReq)
      .searchType(SearchType.DFS_QUERY_THEN_FETCH)
      .requestCache(true)



    /*   Query to fetch for each state word freq in queries
    {
       "size": 0,
       "query": {
          "bool": {
            "must": [
              {
                "nested": {
                  "path": "queries",
                  "query": {
                    "bool": {
                      "filter": {
                        "exists": {
                          "field": "queries"
                        }
                      }
                    }
                  }
                }
              }
            ]
          }
        },
        "aggs": {
        "states": {
          "terms": {
            "field": "state",
            "size": 10000000
          },
          "aggs": {
            "queries": {
              "nested": { "path": "queries" },
              "aggregations": {
                "queries_children": {
                  "terms" : { "field": "queries.query.base" }
                }
              }
            }
          }
        }
      }
    } */

    val stateAggsName = "StatesWordStats"

    // Filter all states with queries
    sourceReq.query(
      QueryBuilders.boolQuery().must(
        QueryBuilders.nestedQuery("queries",
          QueryBuilders.boolQuery().filter(QueryBuilders.existsQuery("queries")), ScoreMode.None))
    )


    // Calculate for each state with queries the words freq histogram.
    sourceReq.aggregation(
      AggregationBuilders.terms(stateAggsName).field("state").size(65536).minDocCount(1)
        .subAggregation(
          AggregationBuilders.nested("queries", "queries")
            .subAggregation(
              AggregationBuilders.terms("queries_children").field("queries.query.base").minDocCount(1)
            )
        )
    )

    val searchResp: SearchResponse = client.search(searchReq, RequestOptions.DEFAULT)


    val pst: ParsedStringTerms = searchResp.getAggregations.get(stateAggsName)
    pst.getBuckets.asScala.map {
      stateBucket => {
        val state = stateBucket.getKeyAsString
        val parsedNested: ParsedNested = stateBucket.getAggregations.get("queries")
        val nQueries: Double = parsedNested.getDocCount
        val parsedStringTerms: ParsedStringTerms = parsedNested.getAggregations.get("queries_children")
        if (nQueries > 0) {
          val wordFreqs = parsedStringTerms.getBuckets.asScala.map {
            bucket => bucket.getKeyAsString -> bucket.getDocCount / nQueries // normalize on nQueries
          }.toMap
          // add to map for each state the histogram wordFreqs
          DTStateWordFreqsItem(state, wordFreqs)
        }
        else
          DTStateWordFreqsItem(state, Map[String,Double]())
      }
    }.toList

  }


  def totalQueriesMatchingRegEx(indexName: String, rx: String): Long = {
    val client: RestHighLevelClient = elasticClient.httpClient

    val sourceReq: SearchSourceBuilder = new SearchSourceBuilder()
      .size(10000)
      .minScore(0.0f)

    val searchReq = new SearchRequest(Index.indexName(indexName, elasticClient.indexSuffix))
      .source(sourceReq)
      .searchType(SearchType.DFS_QUERY_THEN_FETCH)
      .requestCache(true)

    /*   Query to fetch inner hits for the queries which satisfy regex expression
    {
      "size": 10000,
      "query": {
        "nested": {
          "path": "queries",
          "query": {
            "regexp": {
              "queries.query.base": {
                "value": "acc.*",
                "flags": "ALL",
                "max_determinized_states": 10000,
                "rewrite": "constant_score"
              }
            }
          },
          "inner_hits": {
            "size": 100
          }
        }
      }
    }
    */

    sourceReq.query(QueryBuilders
      .nestedQuery("queries",
        QueryBuilders.regexpQuery("queries.query.base", rx).maxDeterminizedStates(10000), ScoreMode.None)
      .innerHit(new InnerHitBuilder().setSize(100)) // Increase in Index Definition
    )


    val searchResp: SearchResponse = client.search(searchReq, RequestOptions.DEFAULT)

    val hits: List[SearchHit] = searchResp.getHits.getHits.toList
    hits.foldLeft(0L) {
      (sum, element) => {
        sum + element.getInnerHits().get("queries").getTotalHits().value
      }
    }

  }


  def totalQueriesOfStateMatchingRegEx(indexName: String, rx: String, stateName: String): Long = {
    val client: RestHighLevelClient = elasticClient.httpClient

    val sourceReq: SearchSourceBuilder = new SearchSourceBuilder()
      .size(10000)
      .minScore(0.0f)

    val searchReq = new SearchRequest(Index.indexName(indexName, elasticClient.indexSuffix))
      .source(sourceReq)
      .searchType(SearchType.DFS_QUERY_THEN_FETCH)
      .requestCache(true)

    /*   Query to fetch inner hits for the queries which satisfy regex expression
    "size": 10000,
      "query":
      {
         "bool" : {
            "must": {
              "match": {
                "state": "quickstart"
                }
             },
            "filter": {
              "nested": {
                "path": "queries",
                "query": {
                  "regexp": {
                    "queries.query.base": {
                      "value": "start.*",
                      "flags": "ALL",
                      "max_determinized_states": 10000,
                      "rewrite": "constant_score"
                    }
                  }
                },
                  "inner_hits": {"size": 100}
              }
            }
      }
      }
      }
    */

    sourceReq.query(QueryBuilders.boolQuery()
      .must(QueryBuilders.matchQuery("state", stateName))
      .filter(QueryBuilders.nestedQuery("queries",
        QueryBuilders.regexpQuery("queries.query.base", rx).maxDeterminizedStates(10000), ScoreMode.None)
        .innerHit(new InnerHitBuilder().setSize(100))) // Increase in Index Definition
    )


    val searchResp: SearchResponse = client.search(searchReq, RequestOptions.DEFAULT)

    val hits: List[SearchHit] = searchResp.getHits.getHits.toList
    hits.foldLeft(0L) {
      (sum, element) => {
        sum + element.getInnerHits().get("queries").getTotalHits().value
      }
    }

  }

}
