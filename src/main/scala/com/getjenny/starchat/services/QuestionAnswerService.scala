package com.getjenny.starchat.services

/**
 * Created by Angelo Leto <angelo@getjenny.com> on 01/07/16.
 */

import java.time.{ZoneId, ZoneOffset}

import akka.event.{Logging, LoggingAdapter}
import com.getjenny.analyzer.util.{RandomNumbers, Time}
import com.getjenny.starchat.SCActorSystem
import com.getjenny.starchat.entities.io._
import com.getjenny.starchat.entities.persistents._
import com.getjenny.starchat.services.esclient.QuestionAnswerElasticClient
import com.getjenny.starchat.services.esclient.crud.IndexLanguageCrud
import org.apache.lucene.search.join._
import org.elasticsearch.action.search.SearchType
import org.elasticsearch.index.query.functionscore._
import org.elasticsearch.index.query.{BoolQueryBuilder, InnerHitBuilder, QueryBuilder, QueryBuilders}
import org.elasticsearch.script.Script
import org.elasticsearch.search.aggregations.bucket.histogram.DateHistogramInterval
import org.elasticsearch.search.aggregations.{AggregationBuilder, AggregationBuilders}
import org.elasticsearch.search.sort.{FieldSortBuilder, ScoreSortBuilder, SortOrder}
import scalaz.Scalaz._

import scala.collection.immutable.{List, Map}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class QuestionAnswerServiceException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)

trait QuestionAnswerService extends AbstractDataService with QuestionAnswerESScripts {
  val log: LoggingAdapter = Logging(SCActorSystem.system, this.getClass.getCanonicalName)

  override val elasticClient: QuestionAnswerElasticClient
  val manausTermsExtractionService: ManausTermsExtractionService.type = ManausTermsExtractionService

  val nested_score_mode: Map[String, ScoreMode] = Map[String, ScoreMode]("min" -> ScoreMode.Min,
    "max" -> ScoreMode.Max, "avg" -> ScoreMode.Avg, "total" -> ScoreMode.Total)

  private[this] val instanceRegistryService: InstanceRegistryService.type = InstanceRegistryService
  private[this] val intervalRe = """(?:([1-9][0-9]*)([ms|m|h|d|w|M|q|y]{1}))""".r

  var cacheStealTimeMillis: Int
  var dictSizeCacheMaxSize: Int
  var totalTermsCacheMaxSize: Int

  private[this] val dictSizeCache: mutable.LinkedHashMap[String, (Long, DictSize)] =
    mutable.LinkedHashMap[String, (Long, DictSize)]()

  private[this] val totalTermsCache: mutable.LinkedHashMap[String, (Long, TotalTerms)] =
    mutable.LinkedHashMap[String, (Long, TotalTerms)]()

  /** Calculate the dictionary size for one index i.e. the number of unique terms
   * in the fields question, answer and in the union of both the fields
   *
   * @param indexName the index name
   * @return a data structure with the unique terms counts
   */
  private[this] def calcDictSize(indexName: String): DictSize = {
    val indexLanguageCrud = IndexLanguageCrud(elasticClient, indexName)

    val questionAgg = AggregationBuilders.cardinality("question_term_count").field("question.base")
    val answerAgg = AggregationBuilders.cardinality("answer_term_count").field("answer.base")
    val scriptBody = "def qnList = new ArrayList(doc[\"question.base\"]) ; " +
      "List anList = doc[\"answer.base\"] ; qnList.addAll(anList) ; return qnList ;"
    val totalAgg = AggregationBuilders.cardinality("total_term_count").script(new Script(scriptBody))

    val query = QueryBuilders.matchAllQuery

    indexLanguageCrud.read(query,
      aggregation = List(questionAgg, answerAgg, totalAgg),
      searchType = SearchType.DFS_QUERY_THEN_FETCH,
      maxItems = Option(0),
      requestCache = Option(true), entityManager = DictSizeEntityManager).head
  }

  /** get the Stale age for the anonymization of the logs
   * @param indexName the index name
   * @return the value fetched from the persisten storage
   */
  def getStaleAge(indexName: String): Long = {
    instanceRegistryService.getInstance(indexName).staleAge.getOrElse(0L)
  }

  /** set the Stale age for the anonymization of the logs
   * @param indexName the index name
   * @param age the value in seconds to be set
   * @return the value set on the persisten storage
   */
  def setStaleAge(indexName: String, age: Long = 0L): Long = {
    val update = instanceRegistryService.updateInstance(indexName: String,
      timestamp = None,
      enabled = None,
      delete = None,
      deleted = None,
      incremental = None,
      staleAge = age.some,
      refreshPolicy = RefreshPolicy.`true`)
    if (update.getShardInfo.getFailed =/= 0)
      throw InstanceRegistryException("Failed updating the staleAge parameter")
    age
  }

  /** Return the the dictionary size for one index i.e. the number of unique terms
   * in the fields question, answer and in the union of both the fields
   * The function returns cached results.
   *
   * @param indexName the index name
   * @param stale     the max cache age in milliseconds
   * @return a data structure with the unique terms counts
   */
  def dictSize(indexName: String, stale: Long = cacheStealTimeMillis): DictSize = {
    val key = indexName

    def removeOldest(): Unit = {
      if (dictSizeCache.size >= dictSizeCacheMaxSize) {
        dictSizeCache.head match {
          case (oldestTerm, (_, _)) => dictSizeCache -= oldestTerm
        }
      }
    }

    dictSizeCache.get(key) match {
      case Some((lastUpdateTs, dictSize)) =>
        val cacheStaleTime = math.abs(Time.timestampMillis - lastUpdateTs)
        if (cacheStaleTime < stale) {
          dictSize
        } else {
          val result = calcDictSize(indexName = indexName)
          removeOldest()
          dictSizeCache.remove(key)
          dictSizeCache.update(key, (Time.timestampMillis, result))
          result
        }
      case _ =>
        val result = calcDictSize(indexName = indexName)
        removeOldest()
        dictSizeCache.update(key, (Time.timestampMillis, result))
        result
    }
  }

  /** Calculate the total number of terms in the fields question and answer, including duplicates.
   *
   * @param indexName the index name
   * @return a data structure with the terms counting and the total number of documents
   */
  private[this] def calcTotalTerms(indexName: String): TotalTerms = {
    val indexLanguageCrud = IndexLanguageCrud(elasticClient, indexName)

    val questionAgg = AggregationBuilders.sum("question_term_count").field("question.base_length")
    val answerAgg = AggregationBuilders.sum("answer_term_count").field("answer.base_length")

    val query = QueryBuilders.matchAllQuery

    indexLanguageCrud.read(query, aggregation = List(questionAgg, answerAgg),
      maxItems = Option(0),
      searchType = SearchType.DFS_QUERY_THEN_FETCH,
      requestCache = Option(true), entityManager = TotalTermsEntityManager).head
  }

  /** Returns the total number of terms in the fields question and answer, including duplicates.
   *
   * @param indexName the index name
   * @param stale     the max cache age in milliseconds
   * @return a data structure with the terms counting and the total number of documents
   */
  def totalTerms(indexName: String, stale: Long = cacheStealTimeMillis): TotalTerms = {
    val key = indexName

    def removeOldest(): Unit = {
      if (totalTermsCache.size >= totalTermsCacheMaxSize) {
        totalTermsCache.head match {
          case (oldestTerm, (_, _)) => totalTermsCache -= oldestTerm
        }
      }
    }

    totalTermsCache.get(key) match {
      case Some((lastUpdateTs, dictSize)) =>
        val cacheStaleTime = math.abs(Time.timestampMillis - lastUpdateTs)
        if (cacheStaleTime < stale) {
          dictSize
        } else {
          val result = calcTotalTerms(indexName = indexName)
          removeOldest()
          totalTermsCache.remove(key)
          totalTermsCache.update(key, (Time.timestampMillis, result))
          result
        }
      case _ =>
        val result = calcTotalTerms(indexName = indexName)
        removeOldest()
        totalTermsCache.update(key, (Time.timestampMillis, result))
        result
    }
  }

  /** calculate the occurrence of a term in the document fields questions or answer and the number of document
   * in which the term occur
   *
   * @param indexName index name
   * @param field     the field: question, answer or all for both
   * @param term      the term to search
   * @return the occurrence of term in the documents and the number of documents
   */
  def calcTermCount(indexName: String,
                    field: TermCountFields.Value = TermCountFields.question, term: String): TermCount = {
    val indexLanguageCrud = IndexLanguageCrud(elasticClient, indexName)

    val agg = AggregationBuilders.sum("countTerms")
      .script(new Script("_score"))

    val esFieldName: String = field match {
      case TermCountFields.question => "question.freq"
      case TermCountFields.answer => "answer.freq"
    }

    val query = QueryBuilders.matchQuery(esFieldName, term)

    indexLanguageCrud.read(query, aggregation = List(agg),
      searchType = SearchType.DFS_QUERY_THEN_FETCH, requestCache = Option(true),
      entityManager = TermCountEntityManager).head
  }

  var countTermCacheMaxSize: Int
  private[this] val countTermCache: mutable.LinkedHashMap[String, (Long, TermCount)] =
    mutable.LinkedHashMap[String, (Long, TermCount)]()

  /** Return the occurrence of a term in the document fields questions or answer and the number of document
   * in which the term occur
   *
   * @param indexName index name
   * @param field     the field: question, answer or all for both
   * @param term      the term to search
   * @param stale     the max cache age in milliseconds
   * @return the occurrence of term in the documents and the number of documents
   */
  def termCount(indexName: String, field: TermCountFields.Value, term: String,
                stale: Long = cacheStealTimeMillis): TermCount = {
    val key = indexName + field + term

    countTermCache.get(key)
      .filter { case (lastUpdateTs, _) => math.abs(Time.timestampMillis - lastUpdateTs) < stale }
      .map { case (_, dictSize) => dictSize }
      .getOrElse {
        val result = calcTermCount(indexName = indexName, field = field, term = term)
        if (countTermCache.size > countTermCacheMaxSize) {
          countTermCache.head match {
            case (oldestTerm, (_, _)) => countTermCache -= oldestTerm
          }
        }
        countTermCache.remove(key)
        countTermCache.update(key, (Time.timestampMillis, result))
        result
      }
  }

  /** set the number of terms counter's cached entries
   *
   * @param parameters the max number of cache entries
   * @return the parameters set
   */
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
      )
    )
  }

  def countersCacheReset: (CountersCacheParameters, CountersCacheSize) = {
    dictSizeCache.clear()
    countTermCache.clear()
    totalTermsCache.clear()
    countersCacheParameters
  }

  private[this] def queryBuilder(documentSearch: QADocumentSearch): BoolQueryBuilder = {
    val boolQueryBuilder: BoolQueryBuilder = QueryBuilders.boolQuery()

    documentSearch.conversation match {
      case Some(convIds) =>
        val orQuery = QueryBuilders.boolQuery()
        convIds.foreach { cId => orQuery.should(QueryBuilders.termQuery("conversation", cId)) }
        boolQueryBuilder.must(orQuery)
      case _ =>
    }

    documentSearch.indexInConversation match {
      case Some(value) =>
        boolQueryBuilder.filter(QueryBuilders.matchQuery("index_in_conversation", value))
      case _ =>
    }

    documentSearch.status match {
      case Some(status) => boolQueryBuilder.filter(QueryBuilders.termQuery("status", status))
      case _ =>
    }

    documentSearch.timestampGte match {
      case Some(ts) =>
        boolQueryBuilder.filter(
          QueryBuilders.rangeQuery("timestamp")
            .gte(ts))
      case _ =>
    }

    documentSearch.timestampLte match {
      case Some(ts) =>
        boolQueryBuilder.filter(
          QueryBuilders.rangeQuery("timestamp")
            .lte(ts))
      case _ =>
    }

    documentSearch.random.filter(identity) match {
      case Some(true) =>
        val randomBuilder = new RandomScoreFunctionBuilder().seed(RandomNumbers.integer)
        val functionScoreQuery: QueryBuilder = QueryBuilders.functionScoreQuery(randomBuilder)
        boolQueryBuilder.filter(functionScoreQuery)
      case _ =>
    }

    val coreDataIn = documentSearch.coreData.getOrElse(QADocumentCore())
    val annotationsIn = documentSearch.annotations.getOrElse(QADocumentAnnotationsSearch())
    val aggsAnnotationsIn = documentSearch.aggAnnotations.getOrElse(AggAnnotationsSearch())

    // begin core data
    coreDataIn.question match {
      case Some(questionQuery) =>
        questionQuery match {
          case "" => // empty field
            val questionPositiveQuery: QueryBuilder = QueryBuilders.boolQuery ()
              .mustNot(QueryBuilders.wildcardQuery("question.raw", "?*"))
              .boost(elasticClient.questionExactMatchBoost)
            boolQueryBuilder.must(questionPositiveQuery)
          case "*" => // existing and non-empty field
            val questionPositiveQuery: QueryBuilder = QueryBuilders.boolQuery ()
              .must(QueryBuilders.existsQuery("question.raw"))
              .must(QueryBuilders.wildcardQuery("question.raw", "?*"))
              .boost(elasticClient.questionExactMatchBoost)
            boolQueryBuilder.must(questionPositiveQuery)
          case _ => // all the other cases
            val questionPositiveQuery: QueryBuilder = QueryBuilders.boolQuery ()
              .must(QueryBuilders.existsQuery("question.raw"))
              .must (QueryBuilders.matchQuery ("question.stem", questionQuery) )
              .should (QueryBuilders.matchPhraseQuery ("question.raw", questionQuery)
                .boost (elasticClient.questionExactMatchBoost) )
            val questionNegativeNestedQuery: QueryBuilder = QueryBuilders.nestedQuery (
              "question_negative",
              QueryBuilders.matchQuery ("question_negative.query.base", questionQuery)
                .minimumShouldMatch (elasticClient.questionNegativeMinimumMatch),
              ScoreMode.Total
            ).ignoreUnmapped (true)
              .innerHit (new InnerHitBuilder ().setSize (100) )
            boolQueryBuilder.should (
              QueryBuilders.boostingQuery (questionPositiveQuery,
                questionNegativeNestedQuery
              ).negativeBoost (elasticClient.questionNegativeBoost)
            )
        }
      case _ =>
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
      case _ =>
    }

    coreDataIn.answer match {
      case Some(value) =>
        boolQueryBuilder.must(QueryBuilders.matchQuery("answer.stem", value))
      case _ =>
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
      case _ =>
    }

    coreDataIn.topics match {
      case Some(topics) => boolQueryBuilder.must(QueryBuilders.termQuery("topics.base", topics))
      case _ =>
    }

    coreDataIn.verified match {
      case Some(verified) => boolQueryBuilder.filter(QueryBuilders.termQuery("verified", verified))
      case _ =>
    }

    coreDataIn.done match {
      case Some(done) => boolQueryBuilder.filter(QueryBuilders.termQuery("done", done))
      case _ => ;
    }
    // end core data

    // begin aggs annotations
    aggsAnnotationsIn.convIdxCounterGte match {
      case Some(t) => boolQueryBuilder.filter(
        QueryBuilders.rangeQuery("starchatAnnotations.convIdxCounter").gte(t))
      case _ =>
    }
    aggsAnnotationsIn.convIdxCounterLte match {
      case Some(t) => boolQueryBuilder.filter(
        QueryBuilders.rangeQuery("starchatAnnotations.convIdxCounter").lte(t))
      case _ =>
    }
    // end aggs annotations

    // begin annotations
    annotationsIn.dclass match {
      case Some(dclass) => boolQueryBuilder.filter(QueryBuilders.termQuery("dclass", dclass))
      case _ =>
    }

    annotationsIn.doctype match {
      case Some(queryParam) =>
        val orQuery = QueryBuilders.boolQuery()
        queryParam.foreach(i => orQuery.should(QueryBuilders.termQuery("doctype", i.toString)))
        boolQueryBuilder.filter(orQuery)
      case _ =>
    }

    annotationsIn.state match {
      case Some(state) => boolQueryBuilder.filter(QueryBuilders.termQuery("state", state))
      case _ =>
    }

    annotationsIn.agent match {
      case Some(queryParam) =>
        val orQuery = QueryBuilders.boolQuery()
        queryParam.foreach(i => orQuery.should(QueryBuilders.termQuery("agent", i.toString)))
        boolQueryBuilder.filter(orQuery)
      case _ =>
    }

    annotationsIn.escalated match {
      case Some(queryParam) =>
        val orQuery = QueryBuilders.boolQuery()
        queryParam.foreach(i => orQuery.should(QueryBuilders.termQuery("escalated", i.toString)))
        boolQueryBuilder.filter(orQuery)
      case _ =>
    }

    annotationsIn.answered match {
      case Some(queryParam) =>
        val orQuery = QueryBuilders.boolQuery()
        queryParam.foreach(i => orQuery.should(QueryBuilders.termQuery("answered", i.toString)))
        boolQueryBuilder.filter(orQuery)
      case _ =>
    }

    annotationsIn.triggered match {
      case Some(queryParam) =>
        val orQuery = QueryBuilders.boolQuery()
        queryParam.foreach(i => orQuery.should(QueryBuilders.termQuery("triggered", i.toString)))
        boolQueryBuilder.filter(orQuery)
      case _ =>
    }

    annotationsIn.followup match {
      case Some(queryParam) =>
        val orQuery = QueryBuilders.boolQuery()
        queryParam.foreach(i => orQuery.should(QueryBuilders.termQuery("followup", i.toString)))
        boolQueryBuilder.filter(orQuery)
      case _ =>
    }

    annotationsIn.feedbackConv match {
      case Some(feedbackConv) =>
        feedbackConv match {
          case "" => // empty field
            val feedbackConvFilterQuery: QueryBuilder = QueryBuilders.boolQuery()
              .mustNot(QueryBuilders.wildcardQuery("feedbackConv.raw", "?*"))
            boolQueryBuilder.filter(feedbackConvFilterQuery)
          case "*" => // existing and non-empty field
            val feedbackConvFilterQuery: QueryBuilder = QueryBuilders.boolQuery()
              .must(QueryBuilders.existsQuery("feedbackConv.raw"))
              .must(QueryBuilders.wildcardQuery("feedbackConv.raw", "?*"))
            boolQueryBuilder.filter(feedbackConvFilterQuery)
          case _ => // all the other cases
            boolQueryBuilder.must(QueryBuilders.matchQuery("feedbackConv", feedbackConv))
        }
      case _ =>
    }

    annotationsIn.feedbackScoreConvGte match {
      case Some(ts) =>
        boolQueryBuilder.filter(
          QueryBuilders.rangeQuery("feedbackConvScore")
            .gte(ts))
      case _ =>
    }

    annotationsIn.feedbackScoreConvLte match {
      case Some(ts) =>
        boolQueryBuilder.filter(
          QueryBuilders.rangeQuery("feedbackConvScore")
            .lte(ts))
      case _ =>
    }

    annotationsIn.algorithmScoreConvGte match {
      case Some(ts) =>
        boolQueryBuilder.filter(
          QueryBuilders.rangeQuery("algorithmConvScore")
            .gte(ts))
      case _ =>
    }

    annotationsIn.algorithmScoreConvLte match {
      case Some(ts) =>
        boolQueryBuilder.filter(
          QueryBuilders.rangeQuery("algorithmConvScore")
            .lte(ts))
      case _ =>
    }

    annotationsIn.feedbackScoreAnswerGte match {
      case Some(ts) =>
        boolQueryBuilder.filter(
          QueryBuilders.rangeQuery("feedbackAnswerScore")
            .gte(ts))
      case _ =>
    }

    annotationsIn.feedbackScoreAnswerLte match {
      case Some(ts) =>
        boolQueryBuilder.filter(
          QueryBuilders.rangeQuery("feedbackAnswerScore")
            .lte(ts))
      case _ =>
    }

    annotationsIn.algorithmScoreAnswerGte match {
      case Some(ts) =>
        boolQueryBuilder.filter(
          QueryBuilders.rangeQuery("algorithmAnswerScore")
            .gte(ts))
      case _ =>
    }

    annotationsIn.algorithmScoreAnswerLte match {
      case Some(ts) =>
        boolQueryBuilder.filter(
          QueryBuilders.rangeQuery("algorithmAnswerScore")
            .lte(ts))
      case _ =>
    }

    annotationsIn.responseScoreGte match {
      case Some(ts) =>
        boolQueryBuilder.filter(
          QueryBuilders.rangeQuery("responseScore")
            .gte(ts))
      case _ =>
    }

    annotationsIn.responseScoreLte match {
      case Some(ts) =>
        boolQueryBuilder.filter(
          QueryBuilders.rangeQuery("responseScore")
            .lte(ts))
      case _ =>
    }

    annotationsIn.start match {
      case Some(start) => boolQueryBuilder.filter(QueryBuilders.termQuery("start", start))
      case _ =>
    }
    // end annotations

    boolQueryBuilder
  }

  def search(indexName: String, documentSearch: QADocumentSearch): Option[SearchQADocumentsResults] = {
    val indexLanguageCrud = IndexLanguageCrud(elasticClient, indexName)

    @deprecated("sortByConvIdIdx attribute will be removed, see: sortBy instead", "StarChat v6.0.0")
    val sort = documentSearch.sortByConvIdIdx match {
      case Some(true) => List(new FieldSortBuilder("conversation").order(SortOrder.DESC),
        new FieldSortBuilder("index_in_conversation").order(SortOrder.DESC),
        new FieldSortBuilder("timestamp").order(SortOrder.DESC))
      case _ =>
        documentSearch.sortBy match {
          case Some(sortByList) => sortByList.map {
            case QASearchSortBy.CONVERSATION =>
              new FieldSortBuilder("conversation").order(SortOrder.DESC)
            case QASearchSortBy.IDX_IN_CONVERSATION =>
              new FieldSortBuilder("index_in_conversation").order(SortOrder.DESC)
            case QASearchSortBy.TIMESTAMP =>
              new FieldSortBuilder("timestamp").order(SortOrder.DESC)
            case QASearchSortBy.SCORE =>
              new ScoreSortBuilder ().order(SortOrder.DESC)
            case _ =>
              throw QuestionAnswerServiceException("Bad QA SortBy value: " + sortByList)
          }
          case _ =>
            List(new ScoreSortBuilder().order(SortOrder.DESC))
        }
    }

    val query = queryBuilder(documentSearch)
    indexLanguageCrud.read(query,
      from = documentSearch.from,
      maxItems = documentSearch.size.orElse(Option(10)),
      minScore = documentSearch.minScore.orElse(Option(elasticClient.queryMinThreshold)),
      sort = sort,
      searchType = SearchType.DFS_QUERY_THEN_FETCH,
      entityManager = new SearchQADocumentEntityManager(indexName)).headOption
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
          .map { case (_: String, docs: List[SearchQADocument]) =>
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
    val indexLanguageCrud = IndexLanguageCrud(elasticClient, indexName)

    val firstIndexInConv: Long = 1
    val query: BoolQueryBuilder = QueryBuilders.boolQuery()

    val dateHistInterval = if (intervalRe.pattern.matcher(request.interval.getOrElse("1M")).matches()) {
      new DateHistogramInterval(request.interval.getOrElse("1M"))
    } else throw QuestionAnswerServiceException("time interval is not well formed")

    val minDocInBuckets = request.minDocInBuckets match {
      case Some(n) => n
      case _ => 1
    }

    request.timestampGte match {
      case Some(ts) =>
        query.filter(
          QueryBuilders.rangeQuery("timestamp")
            .gte(ts))
      case _ => ;
    }

    request.timestampLte match {
      case Some(ts) =>
        query.filter(
          QueryBuilders.rangeQuery("timestamp")
            .lte(ts))
      case _ => ;
    }

    val aggregationList = createAggregations(request, firstIndexInConv, dateHistInterval, minDocInBuckets)

    indexLanguageCrud.read(query,
      searchType = SearchType.DFS_QUERY_THEN_FETCH,
      requestCache = Some(true),
      aggregation = aggregationList,
      maxItems = Option(0),
      minScore = Option(0.0f), entityManager = new QAAggregatedAnalyticsEntityManager(request.aggregations)).head
  }

  private[this] def createAggregations(request: QAAggregatedAnalyticsRequest, firstIndexInConv: Long,
                                       dateHistInterval: DateHistogramInterval, minDocInBuckets: Long): List[AggregationBuilder] = {
    val aggregationBuilderList = new ListBuffer[AggregationBuilder]()

    aggregationBuilderList +=
      AggregationBuilders.filter("filteredCounts",
        QueryBuilders.boolQuery().mustNot(
          QueryBuilders.boolQuery()
            .must(QueryBuilders.termQuery("index_in_conversation", firstIndexInConv))
            .must(QueryBuilders.rangeQuery("starchatAnnotations.convIdxCounter").lte(1)))
      ).subAggregation(
        AggregationBuilders.cardinality("totalDocuments")
          .field("_id").precisionThreshold(40000)
      ).subAggregation(
        AggregationBuilders.cardinality("totalConversations")
          .field("conversation").precisionThreshold(4000)
      )

    val dateHistTimezone = request.timezone match {
      case Some(tz) => ZoneId.ofOffset("UTC", ZoneOffset.of(tz))
      case _ => ZoneId.ofOffset("UTC", ZoneOffset.of("+00:00"))
    }

    request.aggregations match {
      case Some(aggregationsReq) =>
        val reqAggs = aggregationsReq.toSet
        if (reqAggs.contains(QAAggregationsTypes.avgFeedbackConvScore)) {
          aggregationBuilderList += AggregationBuilders
            .avg("avgFeedbackConvScore").field("feedbackConvScore")
        }
        if (reqAggs.contains(QAAggregationsTypes.avgFeedbackAnswerScore)) {
          aggregationBuilderList += AggregationBuilders
            .avg("avgFeedbackAnswerScore").field("feedbackAnswerScore")
        }
        if (reqAggs.contains(QAAggregationsTypes.avgAlgorithmConvScore)) {
          aggregationBuilderList += AggregationBuilders
            .avg("avgAlgorithmConvScore").field("algorithmConvScore")
        }
        if (reqAggs.contains(QAAggregationsTypes.avgAlgorithmAnswerScore)) {
          aggregationBuilderList += AggregationBuilders.avg("avgAlgorithmAnswerScore")
            .field("algorithmAnswerScore")
        }
        if (reqAggs.contains(QAAggregationsTypes.scoreHistogram)) {
          aggregationBuilderList += AggregationBuilders
            .histogram("scoreHistogram").field("feedbackConvScore")
            .interval(1.0d).minDocCount(minDocInBuckets)
        }
        if (reqAggs.contains(QAAggregationsTypes.scoreHistogramNotTransferred)) {
          aggregationBuilderList += AggregationBuilders.filter("scoreHistogramNotTransferred",
            QueryBuilders.boolQuery().mustNot(
              QueryBuilders.termQuery("escalated", Escalated.TRANSFERRED.toString)
            )
          ).subAggregation(
            AggregationBuilders
              .histogram("scoreHistogramNotTransferred").field("feedbackConvScore")
              .interval(1.0d).minDocCount(minDocInBuckets)
          )
        }
        if (reqAggs.contains(QAAggregationsTypes.scoreHistogramTransferred)) {
          aggregationBuilderList += AggregationBuilders.filter("scoreHistogramTransferred",
            QueryBuilders.boolQuery()
              .must(QueryBuilders.termQuery("escalated", Escalated.TRANSFERRED.toString))
          ).subAggregation(
            AggregationBuilders
              .histogram("scoreHistogramTransferred").field("feedbackConvScore")
              .interval(1.0d).minDocCount(minDocInBuckets)
          )
        }
        if (reqAggs.contains(QAAggregationsTypes.conversationsHistogram)) {
          aggregationBuilderList +=
            AggregationBuilders.filter("conversationsHistogram",
              QueryBuilders.boolQuery()
                .must(QueryBuilders.termQuery("index_in_conversation", firstIndexInConv))
                .must(QueryBuilders.rangeQuery("starchatAnnotations.convIdxCounter").gt(1))
            ).subAggregation(
              AggregationBuilders
                .dateHistogram("conversationsHistogram").field("timestamp")
                .calendarInterval(dateHistInterval).minDocCount(minDocInBuckets)
                .timeZone(dateHistTimezone).format("yyyy-MM-dd : HH:mm:ss")
            )
        }
        if (reqAggs.contains(QAAggregationsTypes.conversationsNotTransferredHistogram)) {
          aggregationBuilderList +=
            AggregationBuilders.filter("conversationsNotTransferredHistogram",
              QueryBuilders.boolQuery()
                .mustNot(QueryBuilders.termQuery("escalated", Escalated.TRANSFERRED.toString))
                .must(QueryBuilders.termQuery("index_in_conversation", firstIndexInConv))
                .must(QueryBuilders.rangeQuery("starchatAnnotations.convIdxCounter").gt(1))
            ).subAggregation(
              AggregationBuilders
                .dateHistogram("conversationsNotTransferredHistogram").field("timestamp")
                .calendarInterval(dateHistInterval).minDocCount(minDocInBuckets)
                .timeZone(dateHistTimezone).format("yyyy-MM-dd : HH:mm:ss")
            )
        }
        if (reqAggs.contains(QAAggregationsTypes.conversationsTransferredHistogram)) {
          aggregationBuilderList +=
            AggregationBuilders.filter("conversationsTransferredHistogram",
              QueryBuilders.boolQuery()
                .must(QueryBuilders.termQuery("escalated", Escalated.TRANSFERRED.toString))
                .must(QueryBuilders.termQuery("index_in_conversation", firstIndexInConv))
                .must(QueryBuilders.rangeQuery("starchatAnnotations.convIdxCounter").gt(1))
            ).subAggregation(
              AggregationBuilders
                .dateHistogram("conversationsTransferredHistogram").field("timestamp")
                .calendarInterval(dateHistInterval).minDocCount(minDocInBuckets)
                .timeZone(dateHistTimezone).format("yyyy-MM-dd : HH:mm:ss")
            )
        }
        if (reqAggs.contains(QAAggregationsTypes.qaPairHistogram)) {
          aggregationBuilderList +=
            AggregationBuilders.filter("qaPairHistogram",
              QueryBuilders.boolQuery()
                .must(QueryBuilders.termQuery("triggered", Triggered.UNSPECIFIED.toString))
                .must(QueryBuilders.rangeQuery("starchatAnnotations.convIdxCounter").gt(1))
                .must(QueryBuilders.termQuery("doctype", Doctypes.NORMAL.toString))
                .must(QueryBuilders.termQuery("agent", Agent.STARCHAT.toString))
            ).subAggregation(
              AggregationBuilders
                .dateHistogram("qaPairHistogram").field("timestamp")
                .calendarInterval(dateHistInterval).minDocCount(minDocInBuckets)
                .timeZone(dateHistTimezone).format("yyyy-MM-dd : HH:mm:ss")
            )
        }
        if (reqAggs.contains(QAAggregationsTypes.qaPairAnsweredHistogram)) {
          aggregationBuilderList +=
            AggregationBuilders.filter("qaPairAnsweredHistogram",
              QueryBuilders.boolQuery()
                .must(QueryBuilders.rangeQuery("starchatAnnotations.convIdxCounter").gt(1))
                .must(QueryBuilders.termQuery("triggered", Triggered.UNSPECIFIED.toString))
                .must(QueryBuilders.termQuery("answered", Answered.ANSWERED.toString))
                .must(QueryBuilders.termQuery("doctype", Doctypes.NORMAL.toString))
                .must(QueryBuilders.termQuery("agent", Agent.STARCHAT.toString))
            ).subAggregation(
              AggregationBuilders
                .dateHistogram("qaPairAnsweredHistogram").field("timestamp")
                .calendarInterval(dateHistInterval).minDocCount(minDocInBuckets)
                .timeZone(dateHistTimezone).format("yyyy-MM-dd : HH:mm:ss")
            )
        }
        if (reqAggs.contains(QAAggregationsTypes.qaPairAnsweredFalsePositiveHistogram)) {
          aggregationBuilderList +=
            AggregationBuilders.filter("qaPairAnsweredFalsePositiveHistogram",
              QueryBuilders.boolQuery()
                .must(QueryBuilders.rangeQuery("starchatAnnotations.convIdxCounter").gt(1))
                .must(QueryBuilders.termQuery("triggered", Triggered.UNSPECIFIED.toString))
                .must(QueryBuilders.termQuery("answered", Answered.ANSWERED_FALSE_POSITIVE.toString))
                .must(QueryBuilders.termQuery("doctype", Doctypes.NORMAL.toString))
                .must(QueryBuilders.termQuery("agent", Agent.STARCHAT.toString))
            ).subAggregation(
              AggregationBuilders
                .dateHistogram("qaPairAnsweredFalsePositiveHistogram").field("timestamp")
                .calendarInterval(dateHistInterval).minDocCount(minDocInBuckets)
                .timeZone(dateHistTimezone).format("yyyy-MM-dd : HH:mm:ss")
            )
        }
        if (reqAggs.contains(QAAggregationsTypes.qaPairUnansweredHistogram)) {
          aggregationBuilderList +=
            AggregationBuilders.filter("qaPairUnansweredHistogram",
              QueryBuilders.boolQuery()
                .must(QueryBuilders.rangeQuery("starchatAnnotations.convIdxCounter").gt(1))
                .must(QueryBuilders.termQuery("triggered", Triggered.UNSPECIFIED.toString))
                .must(QueryBuilders.termQuery("answered", Answered.UNANSWERED.toString))
                .must(QueryBuilders.termQuery("doctype", Doctypes.NORMAL.toString))
                .must(QueryBuilders.termQuery("agent", Agent.STARCHAT.toString))
            ).subAggregation(
              AggregationBuilders
                .dateHistogram("qaPairUnansweredHistogram").field("timestamp")
                .calendarInterval(dateHistInterval).minDocCount(minDocInBuckets)
                .timeZone(dateHistTimezone).format("yyyy-MM-dd : HH:mm:ss")
            )
        }
        ///BEGIN TRIGGERED HISTOGRAM
        if (reqAggs.contains(QAAggregationsTypes.qaPairTriggeredHistogram)) {
          aggregationBuilderList +=
            AggregationBuilders.filter("qaPairTriggeredHistogram",
              QueryBuilders.boolQuery()
                .mustNot(QueryBuilders.termQuery("triggered", Triggered.UNSPECIFIED.toString))
                .must(QueryBuilders.rangeQuery("starchatAnnotations.convIdxCounter").gt(1))
                .must(QueryBuilders.termQuery("doctype", Doctypes.NORMAL.toString))
                .must(QueryBuilders.termQuery("agent", Agent.STARCHAT.toString))
            ).subAggregation(
              AggregationBuilders
                .dateHistogram("qaPairTriggeredHistogram").field("timestamp")
                .calendarInterval(dateHistInterval).minDocCount(minDocInBuckets)
                .timeZone(dateHistTimezone).format("yyyy-MM-dd : HH:mm:ss")
            )
        }
        if (reqAggs.contains(QAAggregationsTypes.qaPairAnsweredTriggeredHistogram)) {
          aggregationBuilderList +=
            AggregationBuilders.filter("qaPairAnsweredTriggeredHistogram",
              QueryBuilders.boolQuery()
                .must(QueryBuilders.rangeQuery("starchatAnnotations.convIdxCounter").gt(1))
                .mustNot(QueryBuilders.termQuery("triggered", Triggered.UNSPECIFIED.toString))
                .must(QueryBuilders.termQuery("answered", Answered.ANSWERED.toString))
                .must(QueryBuilders.termQuery("doctype", Doctypes.NORMAL.toString))
                .must(QueryBuilders.termQuery("agent", Agent.STARCHAT.toString))
            ).subAggregation(
              AggregationBuilders
                .dateHistogram("qaPairAnsweredTriggeredHistogram").field("timestamp")
                .calendarInterval(dateHistInterval).minDocCount(minDocInBuckets)
                .timeZone(dateHistTimezone).format("yyyy-MM-dd : HH:mm:ss")
            )
        }
        if (reqAggs.contains(QAAggregationsTypes.qaPairAnsweredFalsePositiveTriggeredHistogram)) {
          aggregationBuilderList +=
            AggregationBuilders.filter("qaPairAnsweredFalsePositiveTriggeredHistogram",
              QueryBuilders.boolQuery()
                .must(QueryBuilders.rangeQuery("starchatAnnotations.convIdxCounter").gt(1))
                .mustNot(QueryBuilders.termQuery("triggered", Triggered.UNSPECIFIED.toString))
                .must(QueryBuilders.termQuery("answered", Answered.ANSWERED_FALSE_POSITIVE.toString))
                .must(QueryBuilders.termQuery("doctype", Doctypes.NORMAL.toString))
                .must(QueryBuilders.termQuery("agent", Agent.STARCHAT.toString))
            ).subAggregation(
              AggregationBuilders
                .dateHistogram("qaPairAnsweredFalsePositiveTriggeredHistogram").field("timestamp")
                .calendarInterval(dateHistInterval).minDocCount(minDocInBuckets)
                .timeZone(dateHistTimezone).format("yyyy-MM-dd : HH:mm:ss")
            )
        }
        ///END TRIGGERED HISTOGRAM
        if (reqAggs.contains(QAAggregationsTypes.qaMatchedStatesHistogram)) {
          aggregationBuilderList +=
            AggregationBuilders.filter("qaMatchedStatesHistogram",
              QueryBuilders.boolQuery()
                .must(QueryBuilders.termQuery("doctype", Doctypes.NORMAL.toString))
                .must(QueryBuilders.termQuery("agent", Agent.STARCHAT.toString))
                .mustNot(
                  QueryBuilders.boolQuery()
                    .must(QueryBuilders.termQuery("index_in_conversation", firstIndexInConv))
                    .must(QueryBuilders.rangeQuery("starchatAnnotations.convIdxCounter").lte(1))
                )
            ).subAggregation(
              AggregationBuilders.terms("qaMatchedStatesHistogram").field("state")
            )
        }
        if (reqAggs.contains(QAAggregationsTypes.qaMatchedStatesWithScoreHistogram)) {
          aggregationBuilderList +=
            AggregationBuilders.filter("qaMatchedStatesWithScoreHistogram",
              QueryBuilders.boolQuery()
                .must(QueryBuilders.rangeQuery("feedbackAnswerScore").gte(0.0))
                .must(QueryBuilders.termQuery("doctype", Doctypes.NORMAL.toString))
                .must(QueryBuilders.termQuery("agent", Agent.STARCHAT.toString))
                .mustNot(
                  QueryBuilders.boolQuery()
                    .must(QueryBuilders.termQuery("index_in_conversation", firstIndexInConv))
                    .must(QueryBuilders.rangeQuery("starchatAnnotations.convIdxCounter").lte(1))
                )
            ).subAggregation(
              AggregationBuilders.terms("qaMatchedStatesWithScoreHistogram").field("state")
            )
        }
        if (reqAggs.contains(QAAggregationsTypes.avgFeedbackNotTransferredConvScoreOverTime)) {
          aggregationBuilderList +=
            AggregationBuilders.filter("avgFeedbackNotTransferredConvScoreOverTime",
              QueryBuilders.boolQuery()
                .must(QueryBuilders.rangeQuery("starchatAnnotations.convIdxCounter").gt(1))
                .mustNot(QueryBuilders.termQuery("escalated", Escalated.TRANSFERRED.toString))
            ).subAggregation(
              AggregationBuilders
                .dateHistogram("avgFeedbackNotTransferredConvScoreOverTime").field("timestamp")
                .calendarInterval(dateHistInterval).minDocCount(minDocInBuckets)
                .timeZone(dateHistTimezone).format("yyyy-MM-dd : HH:mm:ss")
                .subAggregation(AggregationBuilders.avg("avgScore").field("feedbackConvScore"))
            )
        }
        if (reqAggs.contains(QAAggregationsTypes.avgFeedbackTransferredConvScoreOverTime)) {
          aggregationBuilderList +=
            AggregationBuilders.filter("avgFeedbackTransferredConvScoreOverTime",
              QueryBuilders.boolQuery()
                .must(QueryBuilders.rangeQuery("starchatAnnotations.convIdxCounter").gt(1))
                .must(QueryBuilders.termQuery("escalated", Escalated.TRANSFERRED.toString))
            ).subAggregation(
              AggregationBuilders
                .dateHistogram("avgFeedbackTransferredConvScoreOverTime").field("timestamp")
                .calendarInterval(dateHistInterval).minDocCount(minDocInBuckets)
                .timeZone(dateHistTimezone).format("yyyy-MM-dd : HH:mm:ss")
                .subAggregation(AggregationBuilders.avg("avgScore").field("feedbackConvScore"))
            )
        }
        if (reqAggs.contains(QAAggregationsTypes.avgAlgorithmNotTransferredConvScoreOverTime)) {
          aggregationBuilderList +=
            AggregationBuilders.filter("avgAlgorithmNotTransferredConvScoreOverTime",
              QueryBuilders.boolQuery()
                .must(QueryBuilders.rangeQuery("starchatAnnotations.convIdxCounter").gt(1))
                .mustNot(QueryBuilders.termQuery("escalated", Escalated.TRANSFERRED.toString))
            ).subAggregation(
              AggregationBuilders
                .dateHistogram("avgAlgorithmNotTransferredConvScoreOverTime").field("timestamp")
                .calendarInterval(dateHistInterval).minDocCount(minDocInBuckets)
                .timeZone(dateHistTimezone).format("yyyy-MM-dd : HH:mm:ss")
                .subAggregation(AggregationBuilders.avg("avgScore").field("algorithmConvScore"))
            )
        }
        if (reqAggs.contains(QAAggregationsTypes.avgAlgorithmTransferredConvScoreOverTime)) {
          aggregationBuilderList +=
            AggregationBuilders.filter("avgAlgorithmTransferredConvScoreOverTime",
              QueryBuilders.boolQuery()
                .must(QueryBuilders.rangeQuery("starchatAnnotations.convIdxCounter").gt(1))
                .must(QueryBuilders.termQuery("escalated", Escalated.TRANSFERRED.toString))
            ).subAggregation(
              AggregationBuilders
                .dateHistogram("avgAlgorithmTransferredConvScoreOverTime").field("timestamp")
                .calendarInterval(dateHistInterval).minDocCount(minDocInBuckets)
                .timeZone(dateHistTimezone).format("yyyy-MM-dd : HH:mm:ss")
                .subAggregation(AggregationBuilders.avg("avgScore").field("algorithmConvScore"))
            )
        }
        if (reqAggs.contains(QAAggregationsTypes.avgFeedbackConvScoreOverTime)) {
          val filterQ = QueryBuilders.rangeQuery("feedbackConvScore").gte(0)
          aggregationBuilderList +=
            AggregationBuilders.filter("filtered", filterQ).subAggregation(
              AggregationBuilders
                .dateHistogram("avgFeedbackConvScoreOverTime").field("timestamp")
                .calendarInterval(dateHistInterval).minDocCount(minDocInBuckets)
                .timeZone(dateHistTimezone).format("yyyy-MM-dd : HH:mm:ss")
                .subAggregation(AggregationBuilders.avg("avgScore").field("feedbackConvScore")))
        }
        if (reqAggs.contains(QAAggregationsTypes.avgAlgorithmAnswerScoreOverTime)) {
          aggregationBuilderList += AggregationBuilders
            .dateHistogram("avgAlgorithmAnswerScoreOverTime").field("timestamp")
            .calendarInterval(dateHistInterval).minDocCount(minDocInBuckets)
            .timeZone(dateHistTimezone).format("yyyy-MM-dd : HH:mm:ss")
            .subAggregation(AggregationBuilders.avg("avgScore").field("algorithmAnswerScore"))
        }
        if (reqAggs.contains(QAAggregationsTypes.avgFeedbackAnswerScoreOverTime)) {
          aggregationBuilderList += AggregationBuilders
            .dateHistogram("avgFeedbackAnswerScoreOverTime").field("timestamp")
            .calendarInterval(dateHistInterval).minDocCount(minDocInBuckets)
            .timeZone(dateHistTimezone).format("yyyy-MM-dd : HH:mm:ss")
            .subAggregation(AggregationBuilders.avg("avgScore").field("feedbackAnswerScore"))
        }
        if (reqAggs.contains(QAAggregationsTypes.avgAlgorithmConvScoreOverTime)) {
          aggregationBuilderList += AggregationBuilders
            .dateHistogram("avgAlgorithmConvScoreOverTime").field("timestamp")
            .calendarInterval(dateHistInterval).minDocCount(minDocInBuckets)
            .timeZone(dateHistTimezone).format("yyyy-MM-dd : HH:mm:ss")
            .subAggregation(AggregationBuilders.avg("avgScore").field("algorithmConvScore"))
        }

      case _ => List.empty[QAAggregationsTypes.Value]
    }
    aggregationBuilderList.toList
  }

  def create(indexName: String, document: QADocument,
             updateAnnotation: Boolean = true,
             refreshPolicy: RefreshPolicy.Value,
             anonymize: Boolean): Option[IndexDocumentResult] = {
    val indexLanguageCrud = IndexLanguageCrud(elasticClient, indexName)

    val newDoc = if(document.indexInConversation === 1) {
      document.copy(aggAnnotations = Some(AggAnnotations(convIdxCounter = Some(1))))
    } else {
      document.copy(aggAnnotations = Some(AggAnnotations(convIdxCounter = Some(0))))
    }
    val response = indexLanguageCrud.create(newDoc, new QaDocumentEntityManager(indexName), refreshPolicy)

    if(anonymize) {

    }

    if(document.indexInConversation =/= 1) {
      val followup = document.annotations.getOrElse(QADocumentAnnotations()).followup.getOrElse(Followup.UNSPECIFIED)
      if (followup == Followup.UNSPECIFIED) { // increment only if not FOLLOWUP
        /* increment conversation counter */
        if (updateAnnotation && response.created) {
          updateConvAnnotations(indexName = indexName,
            conversation = newDoc.conversation, refreshPolicy = refreshPolicy)
        }
      }
    }

    Option {
      response
    }
  }

  def update(indexName: String, document: QADocumentUpdate,
             refreshPolicy: RefreshPolicy.Value): UpdateDocumentsResult = {
    val indexLanguageCrud = IndexLanguageCrud(elasticClient, indexName)
    val qaDocumentList = QADocumentUpdateEntity.fromQADocumentUpdateList(document)
    val bulkResponse = indexLanguageCrud.bulkUpdate(qaDocumentList.map(x => x.id -> x),
      entityManager = new QaDocumentEntityManager(indexName),
      refreshPolicy = refreshPolicy)

    UpdateDocumentsResult(data = bulkResponse)
  }

  def updateByQueryFullResults(indexName: String,
                               updateReq: UpdateQAByQueryReq,
                               refreshPolicy: RefreshPolicy.Value): UpdateDocumentsResult = {
    val searchRes: Option[SearchQADocumentsResults] = search(indexName, updateReq.documentSearch)
    searchRes match {
      case Some(r) =>
        val id = r.hits.map(_.document.id)
        if (id.nonEmpty) {
          val updateDoc = updateReq.document.copy(id = id)
          update(indexName = indexName, document = updateDoc, refreshPolicy = refreshPolicy)
        } else {
          UpdateDocumentsResult(data = List.empty[UpdateDocumentResult])
        }
      case _ => UpdateDocumentsResult(data = List.empty[UpdateDocumentResult])
    }
  }

  //TODO: add the parameter "documentUpdate: QADocumentUpdateByQuery" once supported by APIs
  private[this] def updateByQuery(indexName: String, searchReq: QADocumentSearch,
                                  script: Option[Script],
                                  refreshPolicy: RefreshPolicy.Value): UpdateByQueryResult = {
    val indexLanguageCrud = IndexLanguageCrud(elasticClient, indexName)
    val query = queryBuilder(searchReq)
    indexLanguageCrud.updateByQuery(
      queryBuilder = query,
      script = script,
      batchSize = None,
      refreshPolicy = refreshPolicy)
  }

  def clearQuestionField(indexName: String,
                         documentSearch: QADocumentSearch,
                         refreshPolicy: RefreshPolicy.Value): UpdateByQueryResult = {
    updateByQuery(indexName, documentSearch, Some(resetQuestionFieldScript), refreshPolicy)
  }

  def updateConvAnnotationsByQuery(indexName: String,
                                   documentSearch: QADocumentSearch,
                                   refreshPolicy: RefreshPolicy.Value): UpdateByQueryResult = {
    val searchRes: Option[SearchQADocumentsResults] = search(indexName, documentSearch)
    searchRes match {
      case Some(r) =>
        val count = r.hits.length
        updateByQuery(indexName, documentSearch, Some(setIdxCounterScript(count)), refreshPolicy)
      case _ =>
        UpdateByQueryResult(
          timedOut = false,
          totalDocs = 0,
          updatedDocs = 0,
          versionConflicts = 0
        )
    }
  }

  def updateConvAnnotations(indexName: String,
                            conversation: String, refreshPolicy: RefreshPolicy.Value): UpdateByQueryResult = {
    val documentSearch = QADocumentSearch(size = Some(10000),
      annotations = Some(QADocumentAnnotationsSearch(followup = Some(List(Followup.UNSPECIFIED)))),
      conversation = Some(List(conversation)))
    updateConvAnnotationsByQuery(indexName, documentSearch, refreshPolicy)
  }

  def read(indexName: String, ids: List[String]): Option[SearchQADocumentsResults] = {
    val indexLanguageCrud = IndexLanguageCrud(elasticClient, indexName)
    indexLanguageCrud.readAll(ids, new SearchQADocumentEntityManager(indexName)).headOption
  }

  def allDocuments(indexName: String, keepAlive: Long = 60000, size: Int = 100): Iterator[QADocument] = {
    val indexLanguageCrud = IndexLanguageCrud(elasticClient, indexName)
    val query = QueryBuilders.matchAllQuery
    indexLanguageCrud.scroll(query, maxItems = Option(size), scrollTime = keepAlive,
      entityManager = new QaDocumentEntityManager(indexName))
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

  def updateTextTerms(indexName: String, extractionRequest: UpdateQATermsRequest): List[UpdateDocumentResult] = {
    val ids: List[String] = List(extractionRequest.id)
    val q = this.read(indexName, ids)
    val documentResults = q.getOrElse(SearchQADocumentsResults())
    documentResults.hits.filter(_.document.coreData.nonEmpty).map { hit =>
      hit.document.coreData match {
        case Some(coreData) =>
          val extractionReqQ = extractionReq(text = coreData.question.getOrElse(""), er = extractionRequest)
          val (_, termsQ) = manausTermsExtractionService
            .textTerms(indexName = indexName, extractionRequest = extractionReqQ)
          val extractionReqA = extractionReq(text = coreData.answer.getOrElse(""), er = extractionRequest)
          val (_, termsA) = manausTermsExtractionService
            .textTerms(indexName = indexName, extractionRequest = extractionReqA)
          val scoredTermsUpdateReq = QADocumentUpdate(
            id = ids,
            coreData = Some(
              QADocumentCore(
                questionScoredTerms = Some(termsQ.toList),
                answerScoredTerms = Some(termsA.toList)
              )
            )
          )
          val res = update(indexName = indexName, document = scoredTermsUpdateReq,
            refreshPolicy = RefreshPolicy.`false`)
          res.data.headOption.getOrElse(
            UpdateDocumentResult(index = indexName, id = hit.document.id, version = -1, created = false)
          )
        case _ =>
          UpdateDocumentResult(index = indexName, id = hit.document.id, version = -1, created = false)
      }
    }
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
            .textTerms(indexName = indexName, extractionRequest = extractionReqQ)
          val (_, termsA) = manausTermsExtractionService
            .textTerms(indexName = indexName, extractionRequest = extractionReqA)

          val scoredTermsUpdateReq = QADocumentUpdate(
            id = List[String](item.id),
            coreData = Some(
              QADocumentCore(
                questionScoredTerms = Some(termsQ.toList),
                answerScoredTerms = Some(termsA.toList)
              )
            )
          )

          val res = update(indexName = indexName, document = scoredTermsUpdateReq,
            refreshPolicy = RefreshPolicy.`false`)
          res.data.headOption.getOrElse(
            UpdateDocumentResult(index = indexName, id = item.id, version = -1, created = false)
          )
        case _ =>
          UpdateDocumentResult(index = indexName, id = item.id, version = -1, created = false)
      }
    }
  }

  def deleteByQuery(indexName: String, search: QADocumentSearch, user: User): DeleteDocumentsSummaryResult = {
    val start = System.currentTimeMillis()
    val query = queryBuilder(search)
    val crud = IndexLanguageCrud(elasticClient, indexName)
    val response = crud.delete(query, RefreshPolicy.`true`)

    log.info("DeleteByQuery requested by {} - deleted documents {} - took {} ms", user.id,
      search, System.currentTimeMillis() - start)

    DeleteDocumentsSummaryResult("deleteByQuery", response.getDeleted)
  }

  override def delete(indexName: String, ids: List[String], refreshPolicy: RefreshPolicy.Value): DeleteDocumentsResult = {
    val indexLanguageCrud = IndexLanguageCrud(elasticClient, indexName)
    val response = indexLanguageCrud.delete(ids, refreshPolicy, new QaDocumentEntityManager(indexName))

    DeleteDocumentsResult(data = response)
  }

  override def deleteAll(indexName: String, refreshPolicy: RefreshPolicy.Value): DeleteDocumentsSummaryResult = {
    val indexLanguageCrud = IndexLanguageCrud(elasticClient, indexName)
    val response = indexLanguageCrud.delete(QueryBuilders.matchAllQuery, refreshPolicy)

    DeleteDocumentsSummaryResult(message = "delete", deleted = response.getTotal)
  }
}
