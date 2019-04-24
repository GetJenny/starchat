package com.getjenny.starchat.services

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 01/07/16.
  */

import java.time.{ZoneId, ZoneOffset}

import akka.event.{Logging, LoggingAdapter}
import com.getjenny.analyzer.util.{RandomNumbers, Time}
import com.getjenny.starchat.SCActorSystem
import com.getjenny.starchat.entities.{LabelCountHistogramItem, _}
import com.getjenny.starchat.services.esclient.QuestionAnswerElasticClient
import com.getjenny.starchat.utils.Index
import org.apache.lucene.search.join._
import org.elasticsearch.action.bulk.BulkRequest
import org.elasticsearch.action.get.{GetResponse, MultiGetItemResponse, MultiGetRequest}
import org.elasticsearch.action.index.{IndexRequest, IndexResponse}
import org.elasticsearch.action.search.{SearchRequest, SearchResponse, SearchScrollRequest, SearchType}
import org.elasticsearch.action.update.UpdateRequest
import org.elasticsearch.client.{RequestOptions, RestHighLevelClient}
import org.elasticsearch.common.unit.TimeValue
import org.elasticsearch.common.xcontent.XContentBuilder
import org.elasticsearch.common.xcontent.XContentFactory._
import org.elasticsearch.index.query.functionscore._
import org.elasticsearch.index.query.{BoolQueryBuilder, InnerHitBuilder, QueryBuilder, QueryBuilders}
import org.elasticsearch.rest.RestStatus
import org.elasticsearch.script._
import org.elasticsearch.search.aggregations.AggregationBuilders
import org.elasticsearch.search.aggregations.bucket.filter.ParsedFilter
import org.elasticsearch.search.aggregations.bucket.histogram.{DateHistogramInterval, Histogram, ParsedDateHistogram}
import org.elasticsearch.search.aggregations.bucket.terms.ParsedStringTerms
import org.elasticsearch.search.aggregations.metrics.{Avg, Cardinality, Sum}
import org.elasticsearch.search.builder.SearchSourceBuilder
import org.elasticsearch.search.sort.{FieldSortBuilder, ScoreSortBuilder, SortOrder}
import scalaz.Scalaz._
import java.time.ZonedDateTime
import scala.collection.JavaConverters._
import scala.collection.immutable.{List, Map}
import scala.collection.mutable
import scala.concurrent.Future

case class QuestionAnswerServiceException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)

trait QuestionAnswerService extends AbstractDataService {
  override val elasticClient: QuestionAnswerElasticClient

  val manausTermsExtractionService: ManausTermsExtractionService.type = ManausTermsExtractionService
  val log: LoggingAdapter = Logging(SCActorSystem.system, this.getClass.getCanonicalName)

  val nested_score_mode: Map[String, ScoreMode] = Map[String, ScoreMode]("min" -> ScoreMode.Min,
    "max" -> ScoreMode.Max, "avg" -> ScoreMode.Avg, "total" -> ScoreMode.Total)

  private[this] val intervalRe = """(?:([1-9][0-9]*)([ms|m|h|d|w|M|q|y]{1}))""".r

  var cacheStealTimeMillis: Int

  private[this] def calcDictSize(indexName: String): DictSize = {
    val client: RestHighLevelClient = elasticClient.httpClient

    val questionAgg = AggregationBuilders.cardinality("question_term_count").field("question.base")
    val answerAgg = AggregationBuilders.cardinality("answer_term_count").field("answer.base")

    val scriptBody = "def qnList = new ArrayList(doc[\"question.base\"]) ; " +
      "List anList = doc[\"answer.base\"] ; qnList.addAll(anList) ; return qnList ;"
    val script: Script = new Script(scriptBody)
    val totalAgg = AggregationBuilders.cardinality("total_term_count").script(script)

    val sourceReq: SearchSourceBuilder = new SearchSourceBuilder()
      .query(QueryBuilders.matchAllQuery)
      .size(0)
      .aggregation(questionAgg)
      .aggregation(answerAgg)
      .aggregation(totalAgg)

    val searchReq = new SearchRequest(Index.indexName(indexName, elasticClient.indexSuffix))
      .source(sourceReq)
      .searchType(SearchType.DFS_QUERY_THEN_FETCH)
      .requestCache(true)

    val searchResp: SearchResponse = client.search(searchReq, RequestOptions.DEFAULT)

    val totalHits = searchResp.getHits.getTotalHits.value

    val questionAggRes: Cardinality = searchResp.getAggregations.get("question_term_count")
    val answerAggRes: Cardinality = searchResp.getAggregations.get("answer_term_count")
    val totalAggRes: Cardinality = searchResp.getAggregations.get("total_term_count")

    DictSize(numDocs = totalHits,
      question = questionAggRes.getValue,
      answer = answerAggRes.getValue,
      total = totalAggRes.getValue
    )
  }

  var dictSizeCacheMaxSize: Int
  private[this] val dictSizeCache: mutable.LinkedHashMap[String, (Long, DictSize)] =
    mutable.LinkedHashMap[String, (Long, DictSize)]()
  def dictSize(indexName: String, stale: Long = cacheStealTimeMillis): DictSize = {
    val key = indexName
    dictSizeCache.get(key) match {
      case Some((lastUpdateTs,dictSize)) =>
        val cacheStaleTime = math.abs(Time.timestampMillis - lastUpdateTs)
        if(cacheStaleTime < stale) {
          dictSize
        } else {
          val result = calcDictSize(indexName = indexName)
          if (dictSizeCache.size >= dictSizeCacheMaxSize) {
            dictSizeCache.head match { case (oldestTerm, (_, _)) => dictSizeCache -= oldestTerm}
          }
          dictSizeCache.remove(key)
          dictSizeCache.update(key, (Time.timestampMillis, result))
          result
        }
      case _ =>
        val result = calcDictSize(indexName = indexName)
        if (dictSizeCache.size >= dictSizeCacheMaxSize) {
          dictSizeCache.head match { case (oldestTerm, (_, _)) => dictSizeCache -= oldestTerm}
        }
        dictSizeCache.update(key, (Time.timestampMillis, result))
        result
    }
  }

  def dictSizeFuture(indexName: String, stale: Long = cacheStealTimeMillis): Future[DictSize] = Future {
    dictSize(indexName = indexName, stale = stale)
  }

  private[this] def calcTotalTerms(indexName: String): TotalTerms = {
    val client: RestHighLevelClient = elasticClient.httpClient

    val questionAgg = AggregationBuilders.sum("question_term_count").field("question.base_length")
    val answerAgg = AggregationBuilders.sum("answer_term_count").field("answer.base_length")

    val sourceReq: SearchSourceBuilder = new SearchSourceBuilder()
      .query(QueryBuilders.matchAllQuery)
      .size(0)
      .aggregation(questionAgg)
      .aggregation(answerAgg)

    val searchReq = new SearchRequest(Index.indexName(indexName, elasticClient.indexSuffix))
      .source(sourceReq)
      .searchType(SearchType.DFS_QUERY_THEN_FETCH)
      .requestCache(true)

    val searchResp: SearchResponse = client.search(searchReq, RequestOptions.DEFAULT)

    val totalHits = searchResp.getHits.getTotalHits.value

    val questionAggRes: Sum = searchResp.getAggregations.get("question_term_count")
    val answerAggRes: Sum = searchResp.getAggregations.get("answer_term_count")

    TotalTerms(numDocs = totalHits,
      question = questionAggRes.getValue.toLong,
      answer = answerAggRes.getValue.toLong)
  }

  var totalTermsCacheMaxSize: Int
  private[this] val totalTermsCache: mutable.LinkedHashMap[String, (Long, TotalTerms)] =
    mutable.LinkedHashMap[String, (Long, TotalTerms)]()
  def totalTerms(indexName: String, stale: Long = cacheStealTimeMillis): TotalTerms = {
    val key = indexName
    totalTermsCache.get(key) match {
      case Some((lastUpdateTs,dictSize)) =>
        val cacheStaleTime = math.abs(Time.timestampMillis - lastUpdateTs)
        if(cacheStaleTime < stale) {
          dictSize
        } else {
          val result = calcTotalTerms(indexName = indexName)
          if (totalTermsCache.size >= totalTermsCacheMaxSize) {
            totalTermsCache.head match { case (oldestTerm, (_, _)) => totalTermsCache -= oldestTerm}
          }
          totalTermsCache.remove(key)
          totalTermsCache.update(key, (Time.timestampMillis, result))
          result
        }
      case _ =>
        val result = calcTotalTerms(indexName = indexName)
        if (totalTermsCache.size >= totalTermsCacheMaxSize) {
          totalTermsCache.head match { case (oldestTerm, (_, _)) => totalTermsCache -= oldestTerm}
        }
        totalTermsCache.update(key, (Time.timestampMillis, result))
        result
    }
  }

  def totalTermsFuture(indexName: String, stale: Long = cacheStealTimeMillis): Future[TotalTerms] = Future {
    totalTerms(indexName = indexName, stale = stale)
  }

  def calcTermCount(indexName: String,
                    field: TermCountFields.Value = TermCountFields.question, term: String): TermCount = {
    val client: RestHighLevelClient = elasticClient.httpClient

    val script: Script = new Script("_score")

    val agg = AggregationBuilders.sum("countTerms").script(script)

    val esFieldName: String = field match {
      case TermCountFields.question => "question.freq"
      case TermCountFields.answer => "answer.freq"
    }

    val boolQueryBuilder : BoolQueryBuilder = QueryBuilders.boolQuery()
      .must(QueryBuilders.matchQuery(esFieldName, term))

    val sourceReq: SearchSourceBuilder = new SearchSourceBuilder()
      .query(boolQueryBuilder)
      .size(0)
      .aggregation(agg)

    val searchReq = new SearchRequest(Index.indexName(indexName, elasticClient.indexSuffix))
      .source(sourceReq)
      .searchType(SearchType.DFS_QUERY_THEN_FETCH)
      .requestCache(true)

    val searchResp: SearchResponse = client.search(searchReq, RequestOptions.DEFAULT)

    val totalHits = searchResp.getHits.getTotalHits.value

    val aggRes: Sum = searchResp.getAggregations.get("countTerms")

    TermCount(numDocs = totalHits,
      count = aggRes.getValue.toLong)
  }

  var countTermCacheMaxSize: Int
  private[this] val countTermCache: mutable.LinkedHashMap[String, (Long, TermCount)] =
    mutable.LinkedHashMap[String, (Long, TermCount)]()
  def termCount(indexName: String, field: TermCountFields.Value, term: String,
                stale: Long = cacheStealTimeMillis): TermCount = {
    val key = indexName + field + term
    countTermCache.get(key) match {
      case Some((lastUpdateTs,dictSize)) =>
        val cacheStaleTime = math.abs(Time.timestampMillis - lastUpdateTs)
        if(cacheStaleTime < stale) {
          dictSize
        } else {
          val result = calcTermCount(indexName = indexName, field = field, term = term)
          if (countTermCache.size > countTermCacheMaxSize) {
            countTermCache.head match { case (oldestTerm, (_, _)) => countTermCache -= oldestTerm}
          }
          countTermCache.remove(key)
          countTermCache.update(key, (Time.timestampMillis, result))
          result
        }
      case _ =>
        val result = calcTermCount(indexName = indexName, field = field, term = term)
        if (countTermCache.size > countTermCacheMaxSize) {
          countTermCache.head match { case (oldestTerm, (_, _)) => countTermCache -= oldestTerm}
        }
        countTermCache.update(key, (Time.timestampMillis, result))
        result
    }
  }

  def termCountFuture(indexName: String, field: TermCountFields.Value, term: String,
                      stale: Long = cacheStealTimeMillis): Future[TermCount] = Future {
    termCount(indexName, field, term, stale)
  }

  def countersCacheParameters(parameters: CountersCacheParameters): CountersCacheParameters = {
    parameters.dictSizeCacheMaxSize match {
      case Some(v) => this.dictSizeCacheMaxSize = v
      case _ => ;
    }

    parameters.totalTermsCacheMaxSize match {
      case Some(v) => this.totalTermsCacheMaxSize = v
      case _ => ;
    }

    parameters.countTermCacheMaxSize match {
      case Some(v) => this.countTermCacheMaxSize = v
      case _ => ;
    }

    parameters.cacheStealTimeMillis match {
      case Some(v) => this.cacheStealTimeMillis = v
      case _ => ;
    }

    CountersCacheParameters(
      dictSizeCacheMaxSize = Some(dictSizeCacheMaxSize),
      totalTermsCacheMaxSize = Some(totalTermsCacheMaxSize),
      countTermCacheMaxSize = Some(countTermCacheMaxSize),
      cacheStealTimeMillis = Some(cacheStealTimeMillis)
    )
  }

  def countersCacheParameters: (CountersCacheParameters, CountersCacheSize) = {
    (CountersCacheParameters(
      dictSizeCacheMaxSize = Some(dictSizeCacheMaxSize),
      totalTermsCacheMaxSize = Some(totalTermsCacheMaxSize),
      countTermCacheMaxSize = Some(countTermCacheMaxSize),
      cacheStealTimeMillis = Some(cacheStealTimeMillis)
    ),
      CountersCacheSize(
        dictSizeCacheSize = dictSizeCache.size,
        totalTermsCacheSize = totalTermsCache.size,
        countTermCacheSize = countTermCache.size
      ))
  }


  def countersCacheReset: (CountersCacheParameters, CountersCacheSize) = {
    dictSizeCache.clear()
    countTermCache.clear()
    totalTermsCache.clear()
    countersCacheParameters
  }

  def documentFromMap(indexName: String, id: String, source : Map[String, Any]): QADocument = {
    val conversation : String = source.get("conversation") match {
      case Some(t) => t.asInstanceOf[String]
      case _ => throw QuestionAnswerServiceException("Missing conversation ID for " +
        "index:docId(" + indexName + ":"  + id + ")")
    }

    val indexInConversation : Int = source.get("index_in_conversation") match {
      case Some(t) => t.asInstanceOf[Int]
      case _ => throw QuestionAnswerServiceException("Missing index in conversation for " +
        "index:docId(" + indexName + ":"  + id + ")")
    }

    val status : Option[Int] = source.get("status") match {
      case Some(t) => Some(t.asInstanceOf[Int])
      case _ => Some(0)
    }

    val timestamp : Option[Long] = source.get("timestamp") match {
      case Some(t) => Option { t.asInstanceOf[Long] }
      case _ => None : Option[Long]
    }

    // begin core data
    val question : Option[String] = source.get("question") match {
      case Some(t) => Some(t.asInstanceOf[String])
      case _ => None
    }

    val questionNegative : Option[List[String]] = source.get("question_negative") match {
      case Some(t) =>
        val res = t.asInstanceOf[java.util.ArrayList[java.util.HashMap[String, String]]]
          .asScala.map(_.asScala.get("query")).filter(_.nonEmpty).map(_.get).toList
        Option { res }
      case _ => None: Option[List[String]]
    }

    val questionScoredTerms: Option[List[(String, Double)]] = source.get("question_scored_terms") match {
      case Some(t) => Option {
        t.asInstanceOf[java.util.ArrayList[java.util.HashMap[String, Any]]].asScala
          .map(pair =>
            (pair.getOrDefault("term", "").asInstanceOf[String],
              pair.getOrDefault("score", 0.0).asInstanceOf[Double]))
          .toList
      }
      case _ => None : Option[List[(String, Double)]]
    }

    val answer : Option[String] = source.get("answer") match {
      case Some(t) => Some(t.asInstanceOf[String])
      case _ => None
    }

    val answerScoredTerms: Option[List[(String, Double)]] = source.get("answer_scored_terms") match {
      case Some(t) => Option {
        t.asInstanceOf[java.util.ArrayList[java.util.HashMap[String, Any]]].asScala
          .map(pair =>
            (pair.getOrDefault("term", "").asInstanceOf[String],
              pair.getOrDefault("score", 0.0).asInstanceOf[Double]))
          .toList
      }
      case _ => None : Option[List[(String, Double)]]
    }

    val topics : Option[String] = source.get("topics") match {
      case Some(t) => Option { t.asInstanceOf[String] }
      case _ => None : Option[String]
    }

    val verified : Option[Boolean] = source.get("verified") match {
      case Some(t) => Some(t.asInstanceOf[Boolean])
      case _ => Some(false)
    }

    val done : Option[Boolean] = source.get("done") match {
      case Some(t) => Some(t.asInstanceOf[Boolean])
      case _ => Some(false)
    }

    val coreDataOut: Option[QADocumentCore] = Some {
      QADocumentCore(
        question = question,
        questionNegative = questionNegative,
        questionScoredTerms = questionScoredTerms,
        answer = answer,
        answerScoredTerms = answerScoredTerms,
        topics = topics,
        verified = verified,
        done = done
      )
    }
    // begin core data

    // begin annotations
    val dclass : Option[String] = source.get("dclass") match {
      case Some(t) => Option { t.asInstanceOf[String] }
      case _ => None : Option[String]
    }

    val doctype : Option[Doctypes.Value] = source.get("doctype") match {
      case Some(t) => Some{Doctypes.value(t.asInstanceOf[String])}
      case _ => Some{Doctypes.NORMAL}
    }

    val state : Option[String] = source.get("state") match {
      case Some(t) => Option { t.asInstanceOf[String] }
      case _ => None : Option[String]
    }

    val agent : Option[Agent.Value] = source.get("agent") match {
      case Some(t) => Some(Agent.value(t.asInstanceOf[String]))
      case _ => Some(Agent.STARCHAT)
    }

    val escalated : Option[Escalated.Value] = source.get("escalated") match {
      case Some(t) => Some(Escalated.value(t.asInstanceOf[String]))
      case _ => Some(Escalated.UNSPECIFIED)
    }

    val answered : Option[Answered.Value] = source.get("answered") match {
      case Some(t) => Some(Answered.value(t.asInstanceOf[String]))
      case _ => Some(Answered.ANSWERED)
    }

    val triggered : Option[Triggered.Value] = source.get("triggered") match {
      case Some(t) => Some(Triggered.value(t.asInstanceOf[String]))
      case _ => Some(Triggered.UNSPECIFIED)
    }

    val followup : Option[Followup.Value] = source.get("followup") match {
      case Some(t) => Some(Followup.value(t.asInstanceOf[String]))
      case _ => Some(Followup.UNSPECIFIED)
    }

    val feedbackConv : Option[String] = source.get("feedbackConv") match {
      case Some(t) => Option { t.asInstanceOf[String] }
      case _ => None : Option[String]
    }

    val feedbackConvScore : Option[Double] = source.get("feedbackConvScore") match {
      case Some(t) => Option { t.asInstanceOf[Double] }
      case _ => None : Option[Double]
    }

    val algorithmConvScore : Option[Double] = source.get("algorithmConvScore") match {
      case Some(t) => Option { t.asInstanceOf[Double] }
      case _ => None : Option[Double]
    }

    val feedbackAnswerScore : Option[Double] = source.get("feedbackAnswerScore") match {
      case Some(t) => Option { t.asInstanceOf[Double] }
      case _ => None : Option[Double]
    }

    val algorithmAnswerScore : Option[Double] = source.get("algorithmAnswerScore") match {
      case Some(t) => Option { t.asInstanceOf[Double] }
      case _ => None : Option[Double]
    }

    val responseScore : Option[Double] = source.get("responseScore") match {
      case Some(t) => Option { t.asInstanceOf[Double] }
      case _ => None : Option[Double]
    }

    val start : Option[Boolean] = source.get("start") match {
      case Some(t) => Some(t.asInstanceOf[Boolean])
      case _ => Some(false)
    }

    val annotationsOut: Option[QADocumentAnnotations] = Some {
      QADocumentAnnotations(
        dclass = dclass,
        doctype = doctype,
        state = state,
        agent = agent,
        escalated = escalated,
        answered = answered,
        triggered = triggered,
        followup = followup,
        feedbackConv = feedbackConv,
        feedbackConvScore = feedbackConvScore,
        algorithmConvScore = algorithmConvScore,
        feedbackAnswerScore = feedbackAnswerScore,
        algorithmAnswerScore = algorithmAnswerScore,
        responseScore = responseScore,
        start = start
      )
    }
    // end annotations

    QADocument(
      id = id,
      conversation = conversation,
      indexInConversation = indexInConversation,
      status = status,
      coreData = coreDataOut,
      annotations = annotationsOut,
      timestamp = timestamp
    )
  }

  private[this] def queryBuilder(documentSearch: QADocumentSearch): BoolQueryBuilder = {
    val boolQueryBuilder : BoolQueryBuilder = QueryBuilders.boolQuery()

    documentSearch.conversation match {
      case Some(convIds) =>
        val convIdBoolQ = QueryBuilders.boolQuery()
        convIds.foreach { cId => convIdBoolQ.should(QueryBuilders.termQuery("conversation", cId)) }
        boolQueryBuilder.must(convIdBoolQ)
      case _ => ;
    }

    documentSearch.indexInConversation match {
      case Some(value) =>
        boolQueryBuilder.must(QueryBuilders.matchQuery("index_in_conversation", value))
      case _ => ;
    }

    documentSearch.status match {
      case Some(status) => boolQueryBuilder.filter(QueryBuilders.termQuery("status", status))
      case _ => ;
    }

    documentSearch.timestampGte match {
      case Some(ts) =>
        boolQueryBuilder.filter(
          QueryBuilders.rangeQuery("timestamp")
            .gte(ts))
      case _ => ;
    }

    documentSearch.timestampLte match {
      case Some(ts) =>
        boolQueryBuilder.filter(
          QueryBuilders.rangeQuery("timestamp")
            .lte(ts))
      case _ => ;
    }

    documentSearch.random.filter(identity) match {
      case Some(true) =>
        val randomBuilder = new RandomScoreFunctionBuilder().seed(RandomNumbers.integer)
        val functionScoreQuery: QueryBuilder = QueryBuilders.functionScoreQuery(randomBuilder)
        boolQueryBuilder.must(functionScoreQuery)
      case _ => ;
    }

    val coreDataIn = documentSearch.coreData.getOrElse(QADocumentCore())
    val annotationsIn = documentSearch.annotations.getOrElse(QADocumentAnnotationsSearch())

    // begin core data
    coreDataIn.question match {
      case Some(questionQuery) =>

        val questionPositiveQuery: QueryBuilder = QueryBuilders.boolQuery()
          .must(QueryBuilders.matchQuery("question.stem", questionQuery))
          .should(QueryBuilders.matchPhraseQuery("question.raw", questionQuery)
            .boost(elasticClient.questionExactMatchBoost))

        val questionNegativeNestedQuery: QueryBuilder = QueryBuilders.nestedQuery(
          "question_negative",
          QueryBuilders.matchQuery("question_negative.query.base", questionQuery)
            .minimumShouldMatch(elasticClient.questionNegativeMinimumMatch),
          ScoreMode.Total
        ).ignoreUnmapped(true)
          .innerHit(new InnerHitBuilder().setSize(100))

        boolQueryBuilder.should(
          QueryBuilders.boostingQuery(questionPositiveQuery,
            questionNegativeNestedQuery
          ).negativeBoost(elasticClient.questionNegativeBoost)
        )
      case _ => ;
    }

    coreDataIn.questionScoredTerms match {
      case Some(questionScoredTerms) =>
        val queryTerms = QueryBuilders.boolQuery()
          .should(QueryBuilders.matchQuery("question_scored_terms.term", questionScoredTerms))
        val script: Script = new Script("doc[\"question_scored_terms.score\"].value")
        val scriptFunction = new ScriptScoreFunctionBuilder(script)
        val functionScoreQuery: QueryBuilder = QueryBuilders.functionScoreQuery(queryTerms, scriptFunction)

        val nestedQuery: QueryBuilder = QueryBuilders.nestedQuery(
          "question_scored_terms",
          functionScoreQuery,
          nested_score_mode.getOrElse(elasticClient.queriesScoreMode, ScoreMode.Total)
        ).ignoreUnmapped(true).innerHit(new InnerHitBuilder().setSize(100))
        boolQueryBuilder.should(nestedQuery)
      case _ => ;
    }

    coreDataIn.answer match {
      case Some(value) =>
        boolQueryBuilder.must(QueryBuilders.matchQuery("answer.stem", value))
      case _ => ;
    }

    coreDataIn.answerScoredTerms match {
      case Some(answerScoredTerms) =>
        val queryTerms = QueryBuilders.boolQuery()
          .should(QueryBuilders.matchQuery("answer_scored_terms.term", answerScoredTerms))
        val script: Script = new Script("doc[\"answer_scored_terms.score\"].value")
        val scriptFunction = new ScriptScoreFunctionBuilder(script)
        val functionScoreQuery: QueryBuilder = QueryBuilders.functionScoreQuery(queryTerms, scriptFunction)

        val nestedQuery: QueryBuilder = QueryBuilders.nestedQuery(
          "answer_scored_terms",
          functionScoreQuery,
          nested_score_mode.getOrElse(elasticClient.queriesScoreMode, ScoreMode.Total)
        ).ignoreUnmapped(true).innerHit(new InnerHitBuilder().setSize(100))
        boolQueryBuilder.should(nestedQuery)
      case _ => ;
    }

    coreDataIn.topics match {
      case Some(topics) => boolQueryBuilder.filter(QueryBuilders.termQuery("topics.base", topics))
      case _ => ;
    }

    coreDataIn.verified match {
      case Some(verified) => boolQueryBuilder.filter(QueryBuilders.termQuery("verified", verified))
      case _ => ;
    }

    coreDataIn.done match {
      case Some(done) => boolQueryBuilder.filter(QueryBuilders.termQuery("done", done))
      case _ => ;
    }
    // end core data

    // begin annotations
    annotationsIn.dclass match {
      case Some(dclass) => boolQueryBuilder.filter(QueryBuilders.termQuery("dclass", dclass))
      case _ => ;
    }

    annotationsIn.doctype match {
      case Some(queryParam) =>
        val orQuery = QueryBuilders.boolQuery()
        queryParam.foreach(i => orQuery.should(QueryBuilders.termQuery("doctype", i.toString)))
        boolQueryBuilder.filter(orQuery)
      case _ => ;
    }

    annotationsIn.state match {
      case Some(state) => boolQueryBuilder.filter(QueryBuilders.termQuery("state", state))
      case _ => ;
    }

    annotationsIn.agent match {
      case Some(queryParam) =>
        val orQuery = QueryBuilders.boolQuery()
        queryParam.foreach(i => orQuery.should(QueryBuilders.termQuery("agent", i.toString)))
        boolQueryBuilder.filter(orQuery)
      case _ => ;
    }

    annotationsIn.escalated match {
      case Some(queryParam) =>
        val orQuery = QueryBuilders.boolQuery()
        queryParam.foreach(i => orQuery.should(QueryBuilders.termQuery("escalated", i.toString)))
        boolQueryBuilder.filter(orQuery)
      case _ => ;
    }

    annotationsIn.answered match {
      case Some(queryParam) =>
        val orQuery = QueryBuilders.boolQuery()
        queryParam.foreach(i => orQuery.should(QueryBuilders.termQuery("answered", i.toString)))
        boolQueryBuilder.filter(orQuery)
      case _ => ;
    }

    annotationsIn.triggered match {
      case Some(queryParam) =>
        val orQuery = QueryBuilders.boolQuery()
        queryParam.foreach(i => orQuery.should(QueryBuilders.termQuery("triggered", i.toString)))
        boolQueryBuilder.filter(orQuery)
      case _ => ;
    }

    annotationsIn.followup match {
      case Some(queryParam) =>
        val orQuery = QueryBuilders.boolQuery()
        queryParam.foreach(i => orQuery.should(QueryBuilders.termQuery("followup", i.toString)))
        boolQueryBuilder.filter(orQuery)
      case _ => ;
    }

    annotationsIn.feedbackConv match {
      case Some(feedbackConv) => boolQueryBuilder.filter(QueryBuilders.termQuery("feedbackConv", feedbackConv))
      case _ => ;
    }

    annotationsIn.feedbackScoreConvGte match {
      case Some(ts) =>
        boolQueryBuilder.filter(
          QueryBuilders.rangeQuery("feedbackConvScore")
            .gte(ts))
      case _ => ;
    }

    annotationsIn.feedbackScoreConvGte match {
      case Some(ts) =>
        boolQueryBuilder.filter(
          QueryBuilders.rangeQuery("feedbackConvScore")
            .lte(ts))
      case _ => ;
    }

    annotationsIn.algorithmScoreConvGte match {
      case Some(ts) =>
        boolQueryBuilder.filter(
          QueryBuilders.rangeQuery("algorithmConvScore")
            .gte(ts))
      case _ => ;
    }

    annotationsIn.algorithmScoreConvLte match {
      case Some(ts) =>
        boolQueryBuilder.filter(
          QueryBuilders.rangeQuery("algorithmConvScore")
            .lte(ts))
      case _ => ;
    }

    annotationsIn.feedbackScoreAnswerGte match {
      case Some(ts) =>
        boolQueryBuilder.filter(
          QueryBuilders.rangeQuery("feedbackAnswerScore")
            .gte(ts))
      case _ => ;
    }

    annotationsIn.feedbackScoreAnswerLte match {
      case Some(ts) =>
        boolQueryBuilder.filter(
          QueryBuilders.rangeQuery("feedbackAnswerScore")
            .lte(ts))
      case _ => ;
    }

    annotationsIn.algorithmScoreAnswerGte match {
      case Some(ts) =>
        boolQueryBuilder.filter(
          QueryBuilders.rangeQuery("algorithmAnswerScore")
            .gte(ts))
      case _ => ;
    }

    annotationsIn.algorithmScoreAnswerLte match {
      case Some(ts) =>
        boolQueryBuilder.filter(
          QueryBuilders.rangeQuery("algorithmAnswerScore")
            .lte(ts))
      case _ => ;
    }

    annotationsIn.responseScoreGte match {
      case Some(ts) =>
        boolQueryBuilder.filter(
          QueryBuilders.rangeQuery("responseScore")
            .gte(ts))
      case _ => ;
    }

    annotationsIn.responseScoreLte match {
      case Some(ts) =>
        boolQueryBuilder.filter(
          QueryBuilders.rangeQuery("responseScore")
            .lte(ts))
      case _ => ;
    }

    annotationsIn.start match {
      case Some(start) => boolQueryBuilder.filter(QueryBuilders.termQuery("start", start))
      case _ => ;
    }
    // end annotations

    boolQueryBuilder
  }

  def search(indexName: String, documentSearch: QADocumentSearch): Option[SearchQADocumentsResults] = {
    val client: RestHighLevelClient = elasticClient.httpClient

    val sourceReq: SearchSourceBuilder = new SearchSourceBuilder()
      .from(documentSearch.from.getOrElse(0))
      .size(documentSearch.size.getOrElse(10))
      .minScore(documentSearch.minScore.getOrElse(Option{elasticClient.queryMinThreshold}.getOrElse(0.0f)))

    documentSearch.sortByConvIdIdx match {
      case Some(true) =>
        sourceReq.sort(new FieldSortBuilder("conversation").order(SortOrder.DESC))
          .sort(new FieldSortBuilder("index_in_conversation").order(SortOrder.DESC))
          .sort(new FieldSortBuilder("timestamp").order(SortOrder.DESC))
      case _ => sourceReq.sort(new ScoreSortBuilder().order(SortOrder.DESC))
    }

    val searchReq = new SearchRequest(Index.indexName(indexName, elasticClient.indexSuffix))
      .source(sourceReq)
      .searchType(SearchType.DFS_QUERY_THEN_FETCH)

    val boolQueryBuilder : BoolQueryBuilder = queryBuilder(documentSearch)

    sourceReq.query(boolQueryBuilder)

    val searchResp: SearchResponse = client.search(searchReq, RequestOptions.DEFAULT)

    val documents : Option[List[SearchQADocument]] = Option {
      searchResp.getHits.getHits.toList.map { item =>
        val id : String = item.getId

        val source : Map[String, Any] = item.getSourceAsMap.asScala.toMap

        val document = documentFromMap(indexName, id, source)

        val searchDocument : SearchQADocument = SearchQADocument(score = item.getScore, document = document)
        searchDocument
      }
    }

    val filteredDoc : List[SearchQADocument] =
      documents.getOrElse(List.empty[SearchQADocument])

    val maxScore : Float = searchResp.getHits.getMaxScore
    val totalHits = searchResp.getHits.getTotalHits.value
    val total : Int = filteredDoc.length
    val searchResults : SearchQADocumentsResults = SearchQADocumentsResults(totalHits = totalHits,
      hitsCount = total, maxScore = maxScore, hits = filteredDoc)

    Some(searchResults)
  }

  def conversations(indexName: String, ids: DocsIds): Conversations = {
    val documentSearch = QADocumentSearch(
      from = Some(0),
      size = Some(10000),
      conversation = Some(ids.ids)
    )
    search(indexName = indexName, documentSearch = documentSearch) match {
      case Some(searchRes) =>
        val conversations = searchRes.hits.groupBy(_.document.conversation)
          .map { case(_: String, docs: List[SearchQADocument]) =>
            Conversation(
              count = docs.length,
              docs = docs.map((_: SearchQADocument).document)
                .sortBy(document => document.indexInConversation)
            )
          }.toList
        Conversations(total = conversations.length, conversations = conversations)
      case _ => Conversations()
    }
  }

  def analytics(indexName: String, request: QAAggregatedAnalyticsRequest): QAAggregatedAnalytics = {
    val client: RestHighLevelClient = elasticClient.httpClient
    val firstIndexInConv: Long = 1

    val sourceReq: SearchSourceBuilder = new SearchSourceBuilder()
      .size(0)
      .minScore(0.0f)

    val searchReq = new SearchRequest(Index.indexName(indexName, elasticClient.indexSuffix))
      .source(sourceReq)
      .searchType(SearchType.DFS_QUERY_THEN_FETCH)
      .requestCache(true)

    val boolQueryBuilder : BoolQueryBuilder = QueryBuilders.boolQuery()

    val dateHistInterval = if(intervalRe.pattern.matcher(request.interval.getOrElse("1M")).matches()) {
      new DateHistogramInterval(request.interval.getOrElse("1M"))
    } else throw QuestionAnswerServiceException("time interval is not well formed")

    val minDocInBuckets = request.minDocInBuckets match {
      case Some(n) => n
      case _ => 1
    }

    request.timestampGte match {
      case Some(ts) =>
        boolQueryBuilder.filter(
          QueryBuilders.rangeQuery("timestamp")
            .gte(ts))
      case _ => ;
    }

    request.timestampLte match {
      case Some(ts) =>
        boolQueryBuilder.filter(
          QueryBuilders.rangeQuery("timestamp")
            .lte(ts))
      case _ => ;
    }

    sourceReq.query(boolQueryBuilder)

    sourceReq.aggregation(AggregationBuilders.cardinality("totalDocuments")
      .field("_id").precisionThreshold(40000))

    sourceReq.aggregation(AggregationBuilders.cardinality("totalConversations")
      .field("conversation").precisionThreshold(4000))

    val dateHistTimezone = request.timezone match {
      case Some(tz) => ZoneId.ofOffset("UTC", ZoneOffset.of(tz))
      case _ => ZoneId.ofOffset("UTC", ZoneOffset.of("+00:00"))
    }

    request.aggregations match {
      case Some(aggregationsReq) =>
        val reqAggs = aggregationsReq.toSet
        if (reqAggs.contains(QAAggregationsTypes.avgFeedbackConvScore)) {
          sourceReq.aggregation(
            AggregationBuilders.avg("avgFeedbackConvScore").field("feedbackConvScore"))
        }
        if (reqAggs.contains(QAAggregationsTypes.avgFeedbackAnswerScore)) {
          sourceReq.aggregation(
            AggregationBuilders.avg("avgFeedbackAnswerScore").field("feedbackAnswerScore"))
        }
        if (reqAggs.contains(QAAggregationsTypes.avgAlgorithmConvScore)) {
          sourceReq.aggregation(
            AggregationBuilders.avg("avgAlgorithmConvScore").field("algorithmConvScore"))
        }
        if (reqAggs.contains(QAAggregationsTypes.avgAlgorithmAnswerScore)) {
          sourceReq.aggregation(
            AggregationBuilders.avg("avgAlgorithmAnswerScore").field("algorithmAnswerScore"))
        }
        if (reqAggs.contains(QAAggregationsTypes.scoreHistogram)) {
          sourceReq.aggregation(
            AggregationBuilders
              .histogram("scoreHistogram").field("feedbackConvScore")
              .interval(1.0d).minDocCount(minDocInBuckets)
          )
        }
        if (reqAggs.contains(QAAggregationsTypes.scoreHistogramNotTransferred)) {
          sourceReq.aggregation(
            AggregationBuilders.filter("scoreHistogramNotTransferred",
              QueryBuilders.boolQuery().mustNot(
                QueryBuilders.termQuery("escalated", Escalated.TRANSFERRED.toString)
              )
            ).subAggregation(
              AggregationBuilders
                .histogram("scoreHistogramNotTransferred").field("feedbackConvScore")
                .interval(1.0d).minDocCount(minDocInBuckets)
            )
          )
        }
        if (reqAggs.contains(QAAggregationsTypes.scoreHistogramTransferred)) {
          sourceReq.aggregation(
            AggregationBuilders.filter("scoreHistogramTransferred",
              QueryBuilders.boolQuery()
                .must(QueryBuilders.termQuery("escalated", Escalated.TRANSFERRED.toString))
            ).subAggregation(
              AggregationBuilders
                .histogram("scoreHistogramTransferred").field("feedbackConvScore")
                .interval(1.0d).minDocCount(minDocInBuckets)
            )
          )
        }
        if (reqAggs.contains(QAAggregationsTypes.conversationsHistogram)) {
          sourceReq.aggregation(
            AggregationBuilders.filter("conversationsHistogram",
              QueryBuilders.termQuery("index_in_conversation", firstIndexInConv)).subAggregation(
              AggregationBuilders
                .dateHistogram("conversationsHistogram").field("timestamp")
                .dateHistogramInterval(dateHistInterval).minDocCount(minDocInBuckets)
                .timeZone(dateHistTimezone).format("yyyy-MM-dd : HH:mm:ss")
            )
          )
        }
        if (reqAggs.contains(QAAggregationsTypes.conversationsNotTransferredHistogram)) {
          sourceReq.aggregation(
            AggregationBuilders.filter("conversationsNotTransferredHistogram",
              QueryBuilders.boolQuery()
                .mustNot(QueryBuilders.termQuery("escalated", Escalated.TRANSFERRED.toString))
                .must(QueryBuilders.termQuery("index_in_conversation", firstIndexInConv)))
              .subAggregation(
                AggregationBuilders
                  .dateHistogram("conversationsNotTransferredHistogram").field("timestamp")
                  .dateHistogramInterval(dateHistInterval).minDocCount(minDocInBuckets)
                  .timeZone(dateHistTimezone).format("yyyy-MM-dd : HH:mm:ss")
              )
          )
        }
        if (reqAggs.contains(QAAggregationsTypes.conversationsTransferredHistogram)) {
          sourceReq.aggregation(
            AggregationBuilders.filter("conversationsTransferredHistogram",
              QueryBuilders.boolQuery()
                .must(QueryBuilders.termQuery("escalated", Escalated.TRANSFERRED.toString))
                .must(QueryBuilders.termQuery("index_in_conversation", firstIndexInConv)))
              .subAggregation(
                AggregationBuilders
                  .dateHistogram("conversationsTransferredHistogram").field("timestamp")
                  .dateHistogramInterval(dateHistInterval).minDocCount(minDocInBuckets)
                  .timeZone(dateHistTimezone).format("yyyy-MM-dd : HH:mm:ss")
              )
          )
        }
        if (reqAggs.contains(QAAggregationsTypes.qaPairHistogram)) {
          sourceReq.aggregation(
            AggregationBuilders.filter("qaPairHistogram",
              QueryBuilders.boolQuery()
                .must(QueryBuilders.termQuery("doctype", Doctypes.NORMAL.toString))
                .must(QueryBuilders.termQuery("agent", Agent.STARCHAT.toString)))
              .subAggregation(
                AggregationBuilders
                  .dateHistogram("qaPairHistogram").field("timestamp")
                  .dateHistogramInterval(dateHistInterval).minDocCount(minDocInBuckets)
                  .timeZone(dateHistTimezone).format("yyyy-MM-dd : HH:mm:ss")
              )
          )
        }
        if (reqAggs.contains(QAAggregationsTypes.qaPairAnsweredHistogram)) {
          sourceReq.aggregation(
            AggregationBuilders.filter("qaPairAnsweredHistogram",
              QueryBuilders.boolQuery()
                .must(QueryBuilders.termQuery("answered", Answered.ANSWERED.toString))
                .must(QueryBuilders.termQuery("doctype", Doctypes.NORMAL.toString))
                .must(QueryBuilders.termQuery("agent", Agent.STARCHAT.toString)))
              .subAggregation(
                AggregationBuilders
                  .dateHistogram("qaPairAnsweredHistogram").field("timestamp")
                  .dateHistogramInterval(dateHistInterval).minDocCount(minDocInBuckets)
                  .timeZone(dateHistTimezone).format("yyyy-MM-dd : HH:mm:ss")
              )
          )
        }
        if (reqAggs.contains(QAAggregationsTypes.qaPairUnansweredHistogram)) {
          sourceReq.aggregation(
            AggregationBuilders.filter("qaPairUnansweredHistogram",
              QueryBuilders.boolQuery()
                .must(QueryBuilders.termQuery("answered", Answered.UNANSWERED.toString))
                .must(QueryBuilders.termQuery("doctype", Doctypes.NORMAL.toString))
                .must(QueryBuilders.termQuery("agent", Agent.STARCHAT.toString)))
              .subAggregation(
                AggregationBuilders
                  .dateHistogram("qaPairUnansweredHistogram").field("timestamp")
                  .dateHistogramInterval(dateHistInterval).minDocCount(minDocInBuckets)
                  .timeZone(dateHistTimezone).format("yyyy-MM-dd : HH:mm:ss")
              )
          )
        }
        if (reqAggs.contains(QAAggregationsTypes.qaMatchedStatesHistogram)) {
          sourceReq.aggregation(
            AggregationBuilders.filter("qaMatchedStatesHistogram",
              QueryBuilders.boolQuery()
                .must(QueryBuilders.termQuery("doctype", Doctypes.NORMAL.toString))
                .must(QueryBuilders.termQuery("agent", Agent.STARCHAT.toString)))
              .subAggregation(
                AggregationBuilders.terms("qaMatchedStatesHistogram").field("state")
              )
          )
        }
        if (reqAggs.contains(QAAggregationsTypes.avgFeedbackNotTransferredConvScoreOverTime)) {
          sourceReq.aggregation(
            AggregationBuilders.filter("avgFeedbackNotTransferredConvScoreOverTime",
              QueryBuilders.boolQuery()
                .mustNot(QueryBuilders.termQuery("escalated", Escalated.TRANSFERRED.toString))
            ).subAggregation(
              AggregationBuilders
                .dateHistogram("avgFeedbackNotTransferredConvScoreOverTime").field("timestamp")
                .dateHistogramInterval(dateHistInterval).minDocCount(minDocInBuckets)
                .timeZone(dateHistTimezone).format("yyyy-MM-dd : HH:mm:ss")
                .subAggregation(AggregationBuilders.avg("avgScore").field("feedbackConvScore"))
            )
          )
        }
        if (reqAggs.contains(QAAggregationsTypes.avgFeedbackTransferredConvScoreOverTime)) {
          sourceReq.aggregation(
            AggregationBuilders.filter("avgFeedbackTransferredConvScoreOverTime",
              QueryBuilders.boolQuery()
                .must(QueryBuilders.termQuery("escalated", Escalated.TRANSFERRED.toString))
            ).subAggregation(
              AggregationBuilders
                .dateHistogram("avgFeedbackTransferredConvScoreOverTime").field("timestamp")
                .dateHistogramInterval(dateHistInterval).minDocCount(minDocInBuckets)
                .timeZone(dateHistTimezone).format("yyyy-MM-dd : HH:mm:ss")
                .subAggregation(AggregationBuilders.avg("avgScore").field("feedbackConvScore"))
            )
          )
        }
        if (reqAggs.contains(QAAggregationsTypes.avgAlgorithmNotTransferredConvScoreOverTime)) {
          sourceReq.aggregation(
            AggregationBuilders.filter("avgAlgorithmNotTransferredConvScoreOverTime",
              QueryBuilders.boolQuery()
                .mustNot(QueryBuilders.termQuery("escalated", Escalated.TRANSFERRED.toString))
            ).subAggregation(
              AggregationBuilders
                .dateHistogram("avgAlgorithmNotTransferredConvScoreOverTime").field("timestamp")
                .dateHistogramInterval(dateHistInterval).minDocCount(minDocInBuckets)
                .timeZone(dateHistTimezone).format("yyyy-MM-dd : HH:mm:ss")
                .subAggregation(AggregationBuilders.avg("avgScore").field("algorithmConvScore"))
            )
          )
        }
        if (reqAggs.contains(QAAggregationsTypes.avgAlgorithmTransferredConvScoreOverTime)) {
          sourceReq.aggregation(
            AggregationBuilders.filter("avgAlgorithmTransferredConvScoreOverTime",
              QueryBuilders.boolQuery()
                .must(QueryBuilders.termQuery("escalated", Escalated.TRANSFERRED.toString))
            ).subAggregation(
              AggregationBuilders
                .dateHistogram("avgAlgorithmTransferredConvScoreOverTime").field("timestamp")
                .dateHistogramInterval(dateHistInterval).minDocCount(minDocInBuckets)
                .timeZone(dateHistTimezone).format("yyyy-MM-dd : HH:mm:ss")
                .subAggregation(AggregationBuilders.avg("avgScore").field("algorithmConvScore"))
            )
          )
        }
        if (reqAggs.contains(QAAggregationsTypes.avgFeedbackConvScoreOverTime)) {
          sourceReq.aggregation(
            AggregationBuilders
              .dateHistogram("avgFeedbackConvScoreOverTime").field("timestamp")
              .dateHistogramInterval(dateHistInterval).minDocCount(minDocInBuckets)
              .timeZone(dateHistTimezone).format("yyyy-MM-dd : HH:mm:ss")
              .subAggregation(AggregationBuilders.avg("avgScore").field("feedbackConvScore"))
          )
        }
        if (reqAggs.contains(QAAggregationsTypes.avgAlgorithmAnswerScoreOverTime)) {
          sourceReq.aggregation(
            AggregationBuilders
              .dateHistogram("avgAlgorithmAnswerScoreOverTime").field("timestamp")
              .dateHistogramInterval(dateHistInterval).minDocCount(minDocInBuckets)
              .timeZone(dateHistTimezone).format("yyyy-MM-dd : HH:mm:ss")
              .subAggregation(AggregationBuilders.avg("avgScore").field("algorithmAnswerScore"))
          )
        }
        if (reqAggs.contains(QAAggregationsTypes.avgFeedbackAnswerScoreOverTime)) {
          sourceReq.aggregation(
            AggregationBuilders
              .dateHistogram("avgFeedbackAnswerScoreOverTime").field("timestamp")
              .dateHistogramInterval(dateHistInterval).minDocCount(minDocInBuckets)
              .timeZone(dateHistTimezone).format("yyyy-MM-dd : HH:mm:ss")
              .subAggregation(AggregationBuilders.avg("avgScore").field("feedbackAnswerScore"))
          )
        }
        if (reqAggs.contains(QAAggregationsTypes.avgAlgorithmConvScoreOverTime)) {
          sourceReq.aggregation(
            AggregationBuilders
              .dateHistogram("avgAlgorithmConvScoreOverTime").field("timestamp")
              .dateHistogramInterval(dateHistInterval).minDocCount(minDocInBuckets)
              .timeZone(dateHistTimezone).format("yyyy-MM-dd : HH:mm:ss")
              .subAggregation(AggregationBuilders.avg("avgScore").field("algorithmConvScore"))
          )
        }
      case _ => List.empty[QAAggregationsTypes.Value]
    }

    val searchResp: SearchResponse = client.search(searchReq, RequestOptions.DEFAULT)

    val totalDocuments: Cardinality = searchResp.getAggregations.get("totalDocuments")

    val totalConversations: Cardinality = searchResp.getAggregations.get("totalConversations")

    val res = request.aggregations match {
      case Some(aggregationsReq) =>
        val reqAggs = aggregationsReq.toSet
        val avgFeedbackConvScore: Option[Double] = if (reqAggs.contains(QAAggregationsTypes.avgFeedbackConvScore)) {
          val avg: Avg = searchResp.getAggregations.get("avgFeedbackConvScore")
          Some(avg.getValue)
        } else None
        val avgFeedbackAnswerScore: Option[Double] = if (reqAggs.contains(QAAggregationsTypes.avgFeedbackAnswerScore)) {
          val avg: Avg = searchResp.getAggregations.get("avgFeedbackAnswerScore")
          Some(avg.getValue)
        } else None
        val avgAlgorithmConvScore: Option[Double] = if (reqAggs.contains(QAAggregationsTypes.avgAlgorithmConvScore)) {
          val avg: Avg = searchResp.getAggregations.get("avgAlgorithmConvScore")
          Some(avg.getValue)
        } else None
        val avgAlgorithmAnswerScore: Option[Double] = if (reqAggs.contains(QAAggregationsTypes.avgAlgorithmAnswerScore)) {
          val avg: Avg = searchResp.getAggregations.get("avgAlgorithmAnswerScore")
          Some(avg.getValue)
        } else None
        val scoreHistogram: Option[List[ScoreHistogramItem]] = if (reqAggs.contains(QAAggregationsTypes.scoreHistogram)) {
          val h: Histogram = searchResp.getAggregations.get("scoreHistogram")
          Some {
            h.getBuckets.asScala.map { bucket =>
              ScoreHistogramItem(
                key = bucket.getKeyAsString,
                docCount = bucket.getDocCount
              )
            }.toList
          }
        } else None
        val scoreHistogramNotTransferred: Option[List[ScoreHistogramItem]] = if (reqAggs.contains(QAAggregationsTypes.scoreHistogramNotTransferred)) {
          val pf: ParsedFilter = searchResp.getAggregations.get("scoreHistogramNotTransferred")
          val h: Histogram = pf.getAggregations.get("scoreHistogramNotTransferred")
          Some {
            h.getBuckets.asScala.map { bucket =>
              ScoreHistogramItem(
                key = bucket.getKeyAsString,
                docCount = bucket.getDocCount
              )
            }.toList
          }
        } else None
        val scoreHistogramTransferred: Option[List[ScoreHistogramItem]] = if (reqAggs.contains(QAAggregationsTypes.scoreHistogramTransferred)) {
          val pf: ParsedFilter = searchResp.getAggregations.get("scoreHistogramTransferred")
          val h: Histogram = pf.getAggregations.get("scoreHistogramTransferred")
          Some {
            h.getBuckets.asScala.map { bucket =>
              ScoreHistogramItem(
                key = bucket.getKeyAsString,
                docCount = bucket.getDocCount
              )
            }.toList
          }
        } else None
        val conversationsHistogram: Option[List[CountOverTimeHistogramItem]] = if (reqAggs.contains(QAAggregationsTypes.conversationsHistogram)) {
          val pf: ParsedFilter = searchResp.getAggregations.get("conversationsHistogram")
          val h: ParsedDateHistogram = pf.getAggregations.get("conversationsHistogram")
          Some {
            h.getBuckets.asScala.map { bucket =>
              CountOverTimeHistogramItem(
                key = bucket.getKey.asInstanceOf[ZonedDateTime].toInstant.toEpochMilli,
                keyAsString = bucket.getKeyAsString,
                docCount = bucket.getDocCount
              )
            }.toList
          }
        } else None
        val conversationsNotTransferredHistogram: Option[List[CountOverTimeHistogramItem]] = if (reqAggs.contains(QAAggregationsTypes.conversationsNotTransferredHistogram)) {
          val pf: ParsedFilter = searchResp.getAggregations.get("conversationsNotTransferredHistogram")
          val h: ParsedDateHistogram = pf.getAggregations.get("conversationsNotTransferredHistogram")
          Some {
            h.getBuckets.asScala.map { bucket =>
              CountOverTimeHistogramItem(
                key = bucket.getKey.asInstanceOf[ZonedDateTime].toInstant.toEpochMilli,
                keyAsString = bucket.getKeyAsString,
                docCount = bucket.getDocCount
              )
            }.toList
          }
        } else None
        val conversationsTransferredHistogram: Option[List[CountOverTimeHistogramItem]] = if (reqAggs.contains(QAAggregationsTypes.conversationsTransferredHistogram)) {
          val pf: ParsedFilter = searchResp.getAggregations.get("conversationsTransferredHistogram")
          val h: ParsedDateHistogram = pf.getAggregations.get("conversationsTransferredHistogram")
          Some {
            h.getBuckets.asScala.map { bucket =>
              CountOverTimeHistogramItem(
                key = bucket.getKey.asInstanceOf[ZonedDateTime].toInstant.toEpochMilli,
                keyAsString = bucket.getKeyAsString,
                docCount = bucket.getDocCount
              )
            }.toList
          }
        } else None
        val qaPairHistogram: Option[List[CountOverTimeHistogramItem]] = if (reqAggs.contains(QAAggregationsTypes.qaPairHistogram)) {
          val pf: ParsedFilter = searchResp.getAggregations.get("qaPairHistogram")
          val h: ParsedDateHistogram = pf.getAggregations.get("qaPairHistogram")
          Some {
            h.getBuckets.asScala.map { bucket =>
              CountOverTimeHistogramItem(
                key = bucket.getKey.asInstanceOf[ZonedDateTime].toInstant.toEpochMilli,
                keyAsString = bucket.getKeyAsString,
                docCount = bucket.getDocCount
              )
            }.toList
          }
        } else None
        val qaPairAnsweredHistogram: Option[List[CountOverTimeHistogramItem]] = if (reqAggs.contains(QAAggregationsTypes.qaPairAnsweredHistogram)) {
          val pf: ParsedFilter = searchResp.getAggregations.get("qaPairAnsweredHistogram")
          val h: ParsedDateHistogram = pf.getAggregations.get("qaPairAnsweredHistogram")
          Some {
            h.getBuckets.asScala.map { bucket =>
              CountOverTimeHistogramItem(
                key = bucket.getKey.asInstanceOf[ZonedDateTime].toInstant.toEpochMilli,
                keyAsString = bucket.getKeyAsString,
                docCount = bucket.getDocCount
              )
            }.toList
          }
        } else None
        val qaPairUnansweredHistogram: Option[List[CountOverTimeHistogramItem]] = if (reqAggs.contains(QAAggregationsTypes.qaPairUnansweredHistogram)) {
          val pf: ParsedFilter = searchResp.getAggregations.get("qaPairUnansweredHistogram")
          val h: ParsedDateHistogram = pf.getAggregations.get("qaPairUnansweredHistogram")
          Some {
            h.getBuckets.asScala.map { bucket =>
              CountOverTimeHistogramItem(
                key = bucket.getKey.asInstanceOf[ZonedDateTime].toInstant.toEpochMilli,
                keyAsString = bucket.getKeyAsString,
                docCount = bucket.getDocCount
              )
            }.toList
          }
        } else None
        val qaMatchedStatesHistogram: Option[List[LabelCountHistogramItem]] = if (reqAggs.contains(QAAggregationsTypes.qaMatchedStatesHistogram)) {
          val pf: ParsedFilter = searchResp.getAggregations.get("qaMatchedStatesHistogram")
          val h: ParsedStringTerms = pf.getAggregations.get("qaMatchedStatesHistogram")
          Some {
            h.getBuckets.asScala.map { bucket =>
              LabelCountHistogramItem(
                key = bucket.getKey.asInstanceOf[String],
                docCount = bucket.getDocCount
              )
            }.toList
          }
        } else None
        val avgFeedbackNotTransferredConvScoreOverTime: Option[List[AvgScoresHistogramItem]] = if (reqAggs.contains(QAAggregationsTypes.avgFeedbackNotTransferredConvScoreOverTime)) {
          val pf: ParsedFilter = searchResp.getAggregations.get("avgFeedbackNotTransferredConvScoreOverTime")
          val h: ParsedDateHistogram = pf.getAggregations.get("avgFeedbackNotTransferredConvScoreOverTime")
          Some {
            h.getBuckets.asScala.map { bucket =>
              val avg: Avg = bucket.getAggregations.get("avgScore")
              AvgScoresHistogramItem(
                key = bucket.getKey.asInstanceOf[ZonedDateTime].toInstant.toEpochMilli,
                keyAsString = bucket.getKeyAsString,
                docCount = bucket.getDocCount,
                avgScore = avg.getValue
              )
            }.toList
          }
        } else None
        val avgFeedbackTransferredConvScoreOverTime: Option[List[AvgScoresHistogramItem]] = if (reqAggs.contains(QAAggregationsTypes.avgFeedbackTransferredConvScoreOverTime)) {
          val pf: ParsedFilter = searchResp.getAggregations.get("avgFeedbackTransferredConvScoreOverTime")
          val h: ParsedDateHistogram = pf.getAggregations.get("avgFeedbackTransferredConvScoreOverTime")
          Some {
            h.getBuckets.asScala.map { bucket =>
              val avg: Avg = bucket.getAggregations.get("avgScore")
              AvgScoresHistogramItem(
                key = bucket.getKey.asInstanceOf[ZonedDateTime].toInstant.toEpochMilli,
                keyAsString = bucket.getKeyAsString,
                docCount = bucket.getDocCount,
                avgScore = avg.getValue
              )
            }.toList
          }
        } else None
        val avgAlgorithmNotTransferredConvScoreOverTime: Option[List[AvgScoresHistogramItem]] = if (reqAggs.contains(QAAggregationsTypes.avgAlgorithmNotTransferredConvScoreOverTime)) {
          val pf: ParsedFilter = searchResp.getAggregations.get("avgAlgorithmNotTransferredConvScoreOverTime")
          val h: ParsedDateHistogram = pf.getAggregations.get("avgAlgorithmNotTransferredConvScoreOverTime")
          Some {
            h.getBuckets.asScala.map { bucket =>
              val avg: Avg = bucket.getAggregations.get("avgScore")
              AvgScoresHistogramItem(
                key = bucket.getKey.asInstanceOf[ZonedDateTime].toInstant.toEpochMilli,
                keyAsString = bucket.getKeyAsString,
                docCount = bucket.getDocCount,
                avgScore = avg.getValue
              )
            }.toList
          }
        } else None
        val avgAlgorithmTransferredConvScoreOverTime: Option[List[AvgScoresHistogramItem]] = if (reqAggs.contains(QAAggregationsTypes.avgAlgorithmTransferredConvScoreOverTime)) {
          val pf: ParsedFilter = searchResp.getAggregations.get("avgAlgorithmTransferredConvScoreOverTime")
          val h: ParsedDateHistogram = pf.getAggregations.get("avgAlgorithmTransferredConvScoreOverTime")
          Some {
            h.getBuckets.asScala.map { bucket =>
              val avg: Avg = bucket.getAggregations.get("avgScore")
              AvgScoresHistogramItem(
                key = bucket.getKey.asInstanceOf[ZonedDateTime].toInstant.toEpochMilli,
                keyAsString = bucket.getKeyAsString,
                docCount = bucket.getDocCount,
                avgScore = avg.getValue
              )
            }.toList
          }
        } else None
        val avgFeedbackConvScoreOverTime: Option[List[AvgScoresHistogramItem]] = if (reqAggs.contains(QAAggregationsTypes.avgFeedbackConvScoreOverTime)) {
          val h: ParsedDateHistogram = searchResp.getAggregations.get("avgFeedbackConvScoreOverTime")
          Some {
            h.getBuckets.asScala.map { bucket =>
              val avg: Avg = bucket.getAggregations.get("avgScore")
              AvgScoresHistogramItem(
                key = bucket.getKey.asInstanceOf[ZonedDateTime].toInstant.toEpochMilli,
                keyAsString = bucket.getKeyAsString,
                docCount = bucket.getDocCount,
                avgScore = avg.getValue
              )
            }.toList
          }
        } else None
        val avgAlgorithmAnswerScoreOverTime: Option[List[AvgScoresHistogramItem]] = if (reqAggs.contains(QAAggregationsTypes.avgAlgorithmAnswerScoreOverTime)) {
          val h: ParsedDateHistogram = searchResp.getAggregations.get("avgAlgorithmAnswerScoreOverTime")
          Some {
            h.getBuckets.asScala.map { bucket =>
              val avg: Avg = bucket.getAggregations.get("avgScore")
              AvgScoresHistogramItem(
                key = bucket.getKey.asInstanceOf[ZonedDateTime].toInstant.toEpochMilli,
                keyAsString = bucket.getKeyAsString,
                docCount = bucket.getDocCount,
                avgScore = avg.getValue
              )
            }.toList
          }
        } else None
        val avgFeedbackAnswerScoreOverTime: Option[List[AvgScoresHistogramItem]] = if (reqAggs.contains(QAAggregationsTypes.avgFeedbackAnswerScoreOverTime)) {
          val h: ParsedDateHistogram = searchResp.getAggregations.get("avgFeedbackAnswerScoreOverTime")
          Some {
            h.getBuckets.asScala.map { bucket =>
              val avg: Avg = bucket.getAggregations.get("avgScore")
              AvgScoresHistogramItem(
                key = bucket.getKey.asInstanceOf[ZonedDateTime].toInstant.toEpochMilli,
                keyAsString = bucket.getKeyAsString,
                docCount = bucket.getDocCount,
                avgScore = avg.getValue
              )
            }.toList
          }
        } else None
        val avgAlgorithmConvScoreOverTime: Option[List[AvgScoresHistogramItem]] = if (reqAggs.contains(QAAggregationsTypes.avgAlgorithmConvScoreOverTime)) {
          val h: ParsedDateHistogram = searchResp.getAggregations.get("avgAlgorithmConvScoreOverTime")
          Some {
            h.getBuckets.asScala.map { bucket =>
              val avg: Avg = bucket.getAggregations.get("avgScore")
              AvgScoresHistogramItem(
                key = bucket.getKey.asInstanceOf[ZonedDateTime].toInstant.toEpochMilli,
                keyAsString = bucket.getKeyAsString,
                docCount = bucket.getDocCount,
                avgScore = avg.getValue
              )
            }.toList
          }
        } else None

        val labelCountHistograms: Map[String, List[LabelCountHistogramItem]] = Map(
          "qaMatchedStatesHistogram" -> qaMatchedStatesHistogram,
        ).filter{case (_, v) => v.nonEmpty}.map{case(k, v) => (k, v.get)}

        val scoreHistograms: Map[String, List[ScoreHistogramItem]] = Map(
          "scoreHistogram" -> scoreHistogram,
          "scoreHistogramNotTransferred" -> scoreHistogramNotTransferred,
          "scoreHistogramTransferred" -> scoreHistogramTransferred
        ).filter{case (_, v) => v.nonEmpty}.map{case(k, v) => (k, v.get)}

        val countOverTimeHistograms: Map[String, List[CountOverTimeHistogramItem]] = Map(
          "conversationsHistogram" -> conversationsHistogram,
          "conversationsNotTransferredHistogram" -> conversationsNotTransferredHistogram,
          "conversationsTransferredHistogram" -> conversationsTransferredHistogram,
          "qaPairHistogram" -> qaPairHistogram,
          "qaPairAnsweredHistogram" -> qaPairAnsweredHistogram,
          "qaPairUnansweredHistogram" -> qaPairUnansweredHistogram
        ).filter{case (_, v) => v.nonEmpty}.map{case(k, v) => (k, v.get)}

        val scoresOverTime: Map[String, List[AvgScoresHistogramItem]] = Map(
          "avgFeedbackNotTransferredConvScoreOverTime" -> avgFeedbackNotTransferredConvScoreOverTime,
          "avgFeedbackTransferredConvScoreOverTime" -> avgFeedbackTransferredConvScoreOverTime,
          "avgAlgorithmNotTransferredConvScoreOverTime" -> avgAlgorithmNotTransferredConvScoreOverTime,
          "avgAlgorithmTransferredConvScoreOverTime" -> avgAlgorithmTransferredConvScoreOverTime,
          "avgFeedbackConvScoreOverTime" -> avgFeedbackConvScoreOverTime,
          "avgAlgorithmAnswerScoreOverTime" -> avgAlgorithmAnswerScoreOverTime,
          "avgFeedbackAnswerScoreOverTime" -> avgFeedbackAnswerScoreOverTime,
          "avgAlgorithmConvScoreOverTime" -> avgAlgorithmConvScoreOverTime
        ).filter{case (_, v) => v.nonEmpty}.map{case(k, v) => (k, v.get)}

        QAAggregatedAnalytics(totalDocuments = totalDocuments.getValue,
          totalConversations = totalConversations.getValue,
          avgFeedbackConvScore = avgFeedbackConvScore,
          avgFeedbackAnswerScore = avgFeedbackAnswerScore,
          avgAlgorithmConvScore = avgAlgorithmConvScore,
          avgAlgorithmAnswerScore = avgAlgorithmAnswerScore,
          labelCountHistograms = if(labelCountHistograms.nonEmpty) Some(labelCountHistograms) else None,
          scoreHistograms = if(scoreHistograms.nonEmpty) Some(scoreHistograms) else None,
          countOverTimeHistograms = if(countOverTimeHistograms.nonEmpty) Some(countOverTimeHistograms) else None,
          scoresOverTime = if(scoresOverTime.nonEmpty) Some(scoresOverTime) else None
        )
      case _ =>
        QAAggregatedAnalytics(totalDocuments = totalDocuments.getValue,
          totalConversations = totalConversations.getValue)
    }

    res
  }

  def create(indexName: String, document: QADocument, refresh: Int): Future[Option[IndexDocumentResult]] = Future {
    val builder: XContentBuilder = jsonBuilder().startObject()

    builder.field("id", document.id)
    builder.field("conversation", document.conversation)

    if (document.indexInConversation <= 0) throw QuestionAnswerServiceException("indexInConversation cannot be < 1")
    builder.field("index_in_conversation", document.indexInConversation)

    document.status match {
      case Some(t) => builder.field("status", t)
      case _ => ;
    }

    document.timestamp match {
      case Some(t) => builder.field("timestamp", t)
      case _ => builder.field("timestamp", Time.timestampMillis)
    }

    // begin core data
    document.coreData match {
      case Some(coreData) =>
        coreData.question match {
          case Some(t) =>
            builder.field("question", t)
          case _ => ;
        }
        coreData.questionNegative match {
          case Some(t) =>
            val array = builder.startArray("question_negative")
            t.foreach(q => {
              array.startObject().field("query", q).endObject()
            })
            array.endArray()
          case _ => ;
        }
        coreData.questionScoredTerms match {
          case Some(t) =>
            val array = builder.startArray("question_scored_terms")
            t.foreach { case (term, score) =>
              array.startObject().field("term", term)
                .field("score", score).endObject()
            }
            array.endArray()
          case _ => ;
        }
        coreData.answer match {
          case Some(t) => builder.field("answer", t)
          case _ => ;
        }
        coreData.answerScoredTerms match {
          case Some(t) =>
            val array = builder.startArray("answer_scored_terms")
            t.foreach { case (term, score) =>
              array.startObject().field("term", term)
                .field("score", score).endObject()
            }
            array.endArray()
          case _ => ;
        }
        coreData.topics match {
          case Some(t) => builder.field("topics", t)
          case _ => ;
        }
        coreData.verified match {
          case Some(t) => builder.field("verified", t)
          case _ => builder.field("verified", false)

        }
        coreData.done match {
          case Some(t) => builder.field("done", t)
          case _ => builder.field("done", false)
        }
      case _ => QADocumentCore()
    }
    // end core data

    // begin annotations
    document.annotations match {
      case Some(annotations) =>
        annotations.dclass match {
          case Some(t) => builder.field("dclass", t)
          case _ => ;
        }
        annotations.doctype match {
          case Some(t) => builder.field("doctype", t.toString)
          case _ => builder.field("doctype", "NORMAL");
        }
        annotations.state match {
          case Some(t) => builder.field("state", t)
          case _ => ;
        }
        annotations.agent match {
          case Some(t) => builder.field("agent", t.toString)
          case _ => builder.field("agent", "STARCHAT") ;
        }
        annotations.escalated match {
          case Some(t) => builder.field("escalated", t.toString)
          case _ => builder.field("escalated", "UNSPECIFIED") ;
        }
        annotations.answered match {
          case Some(t) => builder.field("answered", t.toString)
          case _ => builder.field("answered", "ANSWERED") ;
        }
        annotations.triggered match {
          case Some(t) => builder.field("triggered", t.toString)
          case _ => builder.field("triggered", "UNSPECIFIED");
        }
        annotations.followup match {
          case Some(t) => builder.field("followup", t.toString)
          case _ => builder.field("followup", "UNSPECIFIED");
        }
        annotations.feedbackConv match {
          case Some(t) => builder.field("feedbackConv", t)
          case _ => ;
        }
        annotations.feedbackConvScore match {
          case Some(t) => builder.field("feedbackConvScore", t)
          case _ => ;
        }
        annotations.algorithmConvScore match {
          case Some(t) => builder.field("algorithmConvScore", t)
          case _ => ;
        }
        annotations.feedbackAnswerScore match {
          case Some(t) => builder.field("feedbackAnswerScore", t)
          case _ => ;
        }
        annotations.algorithmAnswerScore match {
          case Some(t) => builder.field("algorithmAnswerScore", t)
          case _ => ;
        }
        annotations.responseScore match {
          case Some(t) => builder.field("responseScore", t)
          case _ => ;
        }
        annotations.start match {
          case Some(t) => builder.field("start", t)
          case _ => builder.field("start", false);
        }
      case _ => QADocumentAnnotations()
    }
    // end annotations

    builder.endObject()

    val client: RestHighLevelClient = elasticClient.httpClient

    val indexReq = new IndexRequest()
      .index(Index.indexName(indexName, elasticClient.indexSuffix))
      .id(document.id)
      .source(builder)

    val response: IndexResponse = client.index(indexReq, RequestOptions.DEFAULT)

    if (refresh =/= 0) {
      val refresh_index = elasticClient.refresh(Index.indexName(indexName, elasticClient.indexSuffix))
      if(refresh_index.failedShardsN > 0) {
        throw QuestionAnswerServiceException("index refresh failed: (" + indexName + ")")
      }
    }

    val doc_result: IndexDocumentResult = IndexDocumentResult(index = response.getIndex,
      id = response.getId,
      version = response.getVersion,
      created = response.status === RestStatus.CREATED
    )

    Option {doc_result}
  }

  private[this] def updateBuilder(document: QADocumentUpdate): XContentBuilder = {
    val builder : XContentBuilder = jsonBuilder().startObject()

    document.conversation match {
      case Some(t) => builder.field("conversation", t)
      case _ => ;
    }

    document.indexInConversation match {
      case Some(t) =>
        if (t <= 0) throw QuestionAnswerServiceException("indexInConversation cannot be < 1")
        builder.field("indexInConversation", t)
      case _ => ;
    }

    document.status match {
      case Some(t) => builder.field("status", t)
      case _ => ;
    }

    document.timestamp match {
      case Some(t) => builder.field("timestamp", t)
      case _ => ;
    }

    // begin core data
    document.coreData match {
      case Some(coreData) =>
        coreData.question match {
          case Some(t) => builder.field("question", t)
          case _ => ;
        }
        coreData.questionNegative match {
          case Some(t) =>
            val array = builder.startArray("question_negative")
            t.foreach(q => {
              array.startObject().field("query", q).endObject()
            })
            array.endArray()
          case _ => ;
        }
        coreData.questionScoredTerms match {
          case Some(t) =>
            val array = builder.startArray("question_scored_terms")
            t.foreach{case(term, score) =>
              array.startObject().field("term", term)
                .field("score", score).endObject()
            }
            array.endArray()
          case _ => ;
        }
        coreData.answer match {
          case Some(t) => builder.field("answer", t)
          case _ => ;
        }
        coreData.answerScoredTerms match {
          case Some(t) =>
            val array = builder.startArray("answer_scored_terms")
            t.foreach{case(term, score) =>
              array.startObject().field("term", term)
                .field("score", score).endObject()
            }
            array.endArray()
          case _ => ;
        }
        coreData.topics match {
          case Some(t) => builder.field("topics", t)
          case _ => ;
        }
        coreData.verified match {
          case Some(t) => builder.field("verified", t)
          case _ => ;
        }
        coreData.done match {
          case Some(t) => builder.field("done", t)
          case _ => ;
        }
      case _ => QADocumentCore()
    }
    // end core data

    // begin annotations
    document.annotations match {
      case Some(annotations) =>
        annotations.dclass match {
          case Some(t) => builder.field("dclass", t)
          case _ => ;
        }
        builder.field("doctype", annotations.doctype.toString)
        annotations.state match {
          case Some(t) =>
            builder.field("state", t)
          case _ => ;
        }
        annotations.agent match {
          case Some(t) =>
            builder.field("agent", t.toString)
          case _ => ;
        }
        annotations.escalated match {
          case Some(t) =>
            builder.field("escalated", t.toString)
          case _ => ;
        }
        annotations.answered match {
          case Some(t) =>
            builder.field("answered", t.toString)
          case _ => ;
        }
        annotations.triggered match {
          case Some(t) =>
            builder.field("triggered", t.toString)
          case _ => ;
        }
        annotations.followup match {
          case Some(t) =>
            builder.field("followup", t.toString)
          case _ => ;
        }
        annotations.feedbackConv match {
          case Some(t) => builder.field("feedbackConv", t)
          case _ => ;
        }
        annotations.feedbackConvScore match {
          case Some(t) => builder.field("feedbackConvScore", t)
          case _ => ;
        }
        annotations.algorithmConvScore match {
          case Some(t) => builder.field("algorithmConvScore", t)
          case _ => ;
        }
        annotations.feedbackAnswerScore match {
          case Some(t) => builder.field("feedbackAnswerScore", t)
          case _ => ;
        }
        annotations.algorithmAnswerScore match {
          case Some(t) => builder.field("algorithmAnswerScore", t)
          case _ => ;
        }
        annotations.responseScore match {
          case Some(t) => builder.field("responseScore", t)
          case _ => ;
        }
        annotations.start match {
          case Some(t) => builder.field("start", t)
          case _ => ;
        }
      case _ => ;
    }
    // end annotations

    builder.endObject()
    builder
  }

  def update(indexName: String, document: QADocumentUpdate, refresh: Int): UpdateDocumentsResult = {
    val builder = updateBuilder(document)

    val client: RestHighLevelClient = elasticClient.httpClient

    val bulkRequest = new BulkRequest
    document.id.map { id =>
      val updateReq = new UpdateRequest()
        .index(Index.indexName(indexName, elasticClient.indexSuffix))
        .doc(builder)
        .id(id)
      bulkRequest.add(updateReq)
    }

    val bulkResponse = client.bulk(bulkRequest, RequestOptions.DEFAULT)

    if (refresh =/= 0) {
      val refresh_index = elasticClient.refresh(Index.indexName(indexName, elasticClient.indexSuffix))
      if(refresh_index.failedShardsN > 0) {
        throw QuestionAnswerServiceException("index refresh failed: (" + indexName + ")")
      }
    }

    val listOfDocRes: List[UpdateDocumentResult] = bulkResponse.getItems.map(response => {
      UpdateDocumentResult(index = response.getIndex,
        id = response.getId,
        version = response.getVersion,
        created = response.status === RestStatus.CREATED
      )
    }).toList

    UpdateDocumentsResult(data = listOfDocRes)
  }

  def updateByQuery(indexName: String, updateReq: UpdateQAByQueryReq, refresh: Int): UpdateDocumentsResult = {
    val searchRes: Option[SearchQADocumentsResults] =
      search(indexName = indexName, documentSearch = updateReq.documentSearch)
    searchRes match {
      case Some(r) =>
        val id = r.hits.map(_.document.id)
        if(id.nonEmpty) {
          val updateDoc = updateReq.document.copy(id = id)
          update(indexName = indexName, document = updateDoc, refresh = refresh)
        } else {
          UpdateDocumentsResult(data = List.empty[UpdateDocumentResult])
        }
      case _ => UpdateDocumentsResult(data = List.empty[UpdateDocumentResult])
    }
  }

  def read(indexName: String, ids: List[String]): Option[SearchQADocumentsResults] = {
    val client: RestHighLevelClient = elasticClient.httpClient

    val multigetReq = new MultiGetRequest()
    ids.foreach{ id =>
      multigetReq.add(
        new MultiGetRequest.Item(Index
          .indexName(indexName, elasticClient.indexSuffix), id)
      )
    }

    val documents : Option[List[SearchQADocument]] = Some {
      client.mget(multigetReq, RequestOptions.DEFAULT).getResponses.toList
        .filter((p: MultiGetItemResponse) => p.getResponse.isExists).map { e =>

        val item: GetResponse = e.getResponse

        val id : String = item.getId

        val source : Map[String, Any] = item.getSource.asScala.toMap

        val document = documentFromMap(indexName, id, source)

        SearchQADocument(score = .0f, document = document)
      }
    }

    val filteredDoc : List[SearchQADocument] = documents.getOrElse(List[SearchQADocument]())

    val maxScore : Float = .0f
    val total : Int = filteredDoc.length
    val searchResults : SearchQADocumentsResults = SearchQADocumentsResults(totalHits = total, hitsCount = total, maxScore = maxScore,
      hits = filteredDoc)

    val searchResultsOption : Option[SearchQADocumentsResults] = Option { searchResults }
    searchResultsOption
  }

  def readFuture(indexName: String, ids: List[String]): Future[Option[SearchQADocumentsResults]] = Future {
    read(indexName, ids)
  }

  def updateFuture(indexName: String, document: QADocumentUpdate, refresh: Int):
  Future[UpdateDocumentsResult] = Future {
    update(indexName = indexName, document = document, refresh = refresh)
  }

  def allDocuments(indexName: String, keepAlive: Long = 60000, size: Int = 100): Iterator[QADocument] = {
    val client: RestHighLevelClient = elasticClient.httpClient

    val sourceReq: SearchSourceBuilder = new SearchSourceBuilder()
      .query(QueryBuilders.matchAllQuery)
      .size(size)

    val searchReq = new SearchRequest(Index.indexName(indexName, elasticClient.indexSuffix))
      .source(sourceReq)
      .scroll(new TimeValue(keepAlive))

    var scrollResp: SearchResponse = client.search(searchReq, RequestOptions.DEFAULT)
    val scrollId = scrollResp.getScrollId

    val iterator = Iterator.continually{
      val documents = scrollResp.getHits.getHits.toList.map { item =>
        val id : String = item.getId

        val source : Map[String, Any] = item.getSourceAsMap.asScala.toMap

        documentFromMap(indexName, id, source)
      }

      var scrollRequest: SearchScrollRequest = new SearchScrollRequest(scrollId)
      scrollRequest.scroll(new TimeValue(keepAlive))
      scrollResp = client.scroll(scrollRequest, RequestOptions.DEFAULT)
      (documents, documents.nonEmpty)
    }.takeWhile{case (_, docNonEmpty) => docNonEmpty}
    iterator
      .flatMap{case (doc, _) => doc}
  }

  private[this] def extractionReq(text: String, er: UpdateQATermsRequest) = TermsExtractionRequest(text = text,
    tokenizer = Some("space_punctuation"),
    commonOrSpecificSearchPrior = Some(CommonOrSpecificSearch.COMMON),
    commonOrSpecificSearchObserved = Some(CommonOrSpecificSearch.IDXSPECIFIC),
    observedDataSource = Some(ObservedDataSources.KNOWLEDGEBASE),
    fieldsPrior = Some(TermCountFields.all),
    fieldsObserved = Some(TermCountFields.all),
    minWordsPerSentence = Some(10),
    pruneTermsThreshold = Some(100000),
    misspellMaxOccurrence = Some(5),
    activePotentialDecay = Some(10),
    activePotential = Some(true),
    totalInfo = Some(false))

  def updateTextTerms(indexName: String,
                      extractionRequest: UpdateQATermsRequest
                     ): List[UpdateDocumentResult] = {

    val ids: List[String] = List(extractionRequest.id)
    val q = this.read(indexName, ids)
    val hits = q.getOrElse(SearchQADocumentsResults())
    hits.hits.filter(_.document.coreData.nonEmpty).map { hit =>
      hit.document.coreData match {
        case Some(coreData) =>
          val extractionReqQ = extractionReq(text = coreData.question.getOrElse(""), er = extractionRequest)
          val (_, termsQ) = manausTermsExtractionService
            .textTerms(indexName = indexName ,extractionRequest = extractionReqQ)
          val extractionReqA = extractionReq(text = coreData.answer.getOrElse(""), er = extractionRequest)
          val (_, termsA) = manausTermsExtractionService
            .textTerms(indexName = indexName ,extractionRequest = extractionReqA)
          val scoredTermsUpdateReq = QADocumentUpdate(
            id = ids,
            coreData = Some(
              QADocumentCore(
                questionScoredTerms = Some(termsQ.toList),
                answerScoredTerms = Some(termsA.toList)
              )
            )
          )
          val res = update(indexName = indexName, document = scoredTermsUpdateReq, refresh = 0)
          res.data.headOption.getOrElse(
            UpdateDocumentResult(index = indexName, id = hit.document.id, version = -1, created = false)
          )
        case _ =>
          UpdateDocumentResult(index = indexName, id = hit.document.id, version = -1, created = false)
      }
    }
  }

  def updateTextTermsFuture(indexName: String,
                            extractionRequest: UpdateQATermsRequest):
  Future[List[UpdateDocumentResult]] = Future {
    updateTextTerms(indexName, extractionRequest)
  }

  def updateAllTextTerms(indexName: String,
                         extractionRequest: UpdateQATermsRequest,
                         keepAlive: Long = 3600000): Iterator[UpdateDocumentResult] = {
    allDocuments(indexName = indexName, keepAlive = keepAlive).filter(_.coreData.nonEmpty).map { item =>
      item.coreData match {
        case Some(coreData) =>
          val extractionReqQ = extractionReq(text = coreData.question.getOrElse(""), er = extractionRequest)
          val extractionReqA = extractionReq(text = coreData.answer.getOrElse(""), er = extractionRequest)
          val (_, termsQ) = manausTermsExtractionService
            .textTerms(indexName = indexName ,extractionRequest = extractionReqQ)
          val (_, termsA) = manausTermsExtractionService
            .textTerms(indexName = indexName ,extractionRequest = extractionReqA)

          val scoredTermsUpdateReq = QADocumentUpdate(
            id = List[String](item.id),
            coreData = Some(
              QADocumentCore(
                questionScoredTerms = Some(termsQ.toList),
                answerScoredTerms = Some(termsA.toList)
              )
            )
          )

          val res = update(indexName = indexName, document = scoredTermsUpdateReq, refresh = 0)
          res.data.headOption.getOrElse(
            UpdateDocumentResult(index = indexName, id = item.id, version = -1, created = false)
          )
        case _ =>
          UpdateDocumentResult(index = indexName, id = item.id, version = -1, created = false)
      }
    }
  }
}
