package com.getjenny.starchat.services.esclient.crud

import akka.event.{Logging, LoggingAdapter}
import com.getjenny.starchat.SCActorSystem
import com.getjenny.starchat.entities.io.{RefreshIndexResult, RefreshPolicy}
import com.getjenny.starchat.services.esclient.ElasticClient
import org.elasticsearch.action.bulk.{BulkRequest, BulkResponse}
import org.elasticsearch.action.delete.{DeleteRequest, DeleteResponse}
import org.elasticsearch.action.get.{GetRequest, GetResponse, MultiGetRequest, MultiGetResponse}
import org.elasticsearch.action.index.{IndexRequest, IndexResponse}
import org.elasticsearch.action.search.{SearchRequest, SearchResponse, SearchScrollRequest, SearchType}
import org.elasticsearch.action.update.{UpdateRequest, UpdateResponse}
import org.elasticsearch.client.RequestOptions
import org.elasticsearch.common.unit.TimeValue
import org.elasticsearch.common.xcontent.XContentBuilder
import org.elasticsearch.index.query.QueryBuilder
import org.elasticsearch.index.reindex.{BulkByScrollResponse, DeleteByQueryRequest, UpdateByQueryRequest}
import org.elasticsearch.script.Script
import org.elasticsearch.search.aggregations.AggregationBuilder
import org.elasticsearch.search.builder.SearchSourceBuilder
import org.elasticsearch.search.sort.SortBuilder
import org.elasticsearch.search.suggest.SuggestBuilder
import scalaz.Scalaz._

class EsCrudBase(val client: ElasticClient, val index: String) {

  private[this] val log: LoggingAdapter = Logging(SCActorSystem.system, this.getClass.getCanonicalName)

  def read(id: String): GetResponse = {
    val request = new GetRequest()
      .index(index)
      .id(id)

    client.httpClient.get(request, RequestOptions.DEFAULT)
  }

  def read(queryBuilder: QueryBuilder,
           from: Option[Int] = None,
           sort: List[SortBuilder[_]] = List.empty,
           maxItems: Option[Int] = None,
           searchType: SearchType = SearchType.DEFAULT,
           aggregation: List[AggregationBuilder] = List.empty,
           requestCache: Option[Boolean] = None,
           minScore: Option[Float] = None,
           scroll: Boolean = false,
           scrollTime: Long = 60000,
           version: Option[Boolean] = None,
           fetchSource: Option[Array[String]] = None): SearchResponse = {

    val search: SearchSourceBuilder = new SearchSourceBuilder()
      .from(from.getOrElse(0))
      .query(queryBuilder)
      .size(maxItems.getOrElse(100))

    sort.foreach(b => search.sort(b))
    aggregation.foreach(b => search.aggregation(b))
    minScore.foreach(search.minScore)
    version.foreach(search.version(_))
    fetchSource.foreach(x => search.fetchSource(x, Array.empty[String]))

    val request = new SearchRequest(index)
      .source(search)
      .searchType(searchType)

    if (scroll) request.scroll(new TimeValue(scrollTime))
    requestCache.map(request.requestCache(_))

    log.debug("Search request: {}", request)

    client.httpClient.search(request, RequestOptions.DEFAULT)
  }

  def suggest(suggestBuilder: SuggestBuilder,
           maxItems: Option[Int] = None,
           searchType: SearchType = SearchType.DEFAULT,
           requestCache: Option[Boolean] = None,
           minScore: Option[Float] = None,
           scroll: Boolean = false,
           scrollTime: Long = 60000,
           version: Option[Boolean] = None,
           fetchSource: Option[Array[String]] = None): SearchResponse = {

    val search: SearchSourceBuilder = new SearchSourceBuilder()
      .suggest(suggestBuilder)
      .size(maxItems.getOrElse(100))

    minScore.foreach(search.minScore)
    version.foreach(search.version(_))
    fetchSource.foreach(x => search.fetchSource(x, Array.empty[String]))

    val request = new SearchRequest(index)
      .source(search)
      .searchType(searchType)

    if (scroll) request.scroll(new TimeValue(scrollTime))
    requestCache.map(request.requestCache(_))

    log.debug("suggest request: {}", request)

    client.httpClient.search(request, RequestOptions.DEFAULT)
  }

  def scroll(queryBuilder: QueryBuilder,
             from: Option[Int] = None,
             sort: List[SortBuilder[_]] = List.empty,
             maxItems: Option[Int] = None,
             searchType: SearchType = SearchType.DEFAULT,
             aggregation: List[AggregationBuilder] = List.empty,
             requestCache: Option[Boolean] = None,
             minScore: Option[Float] = None,
             scrollTime: Long = 60000,
             version: Option[Boolean] = None,
             fetchSource: Option[Array[String]] = None): Iterator[SearchResponse] = {

    var response = read(queryBuilder, from, sort, maxItems, searchType, aggregation, requestCache,
      minScore, scroll = true, scrollTime, version, fetchSource)

    val scrollId = response.getScrollId

    val initialResponse = response
    Iterator(initialResponse) ++ Iterator.continually {
      val request = new SearchScrollRequest(scrollId)
      request.scroll(new TimeValue(scrollTime))
      response = client.httpClient.scroll(request, RequestOptions.DEFAULT)
      val r = response //store response in an immutable variable
      r
    }.takeWhile(x => x.getHits.getHits.length =/= 0)
  }

  def readAll(ids: List[String]): MultiGetResponse = {
    val request = new MultiGetRequest()
    ids.foreach { id =>
      request.add(new MultiGetRequest.Item(index, id))
    }
    client.httpClient.mget(request, RequestOptions.DEFAULT)
  }

  def create(id: String, builder: XContentBuilder, refreshPolicy: RefreshPolicy.Value): IndexResponse = {
    log.debug("Indexing id: {}", id)
    val request: IndexRequest = createIndexRequest(id, builder)

    /* FIXME: use the following code when is reliable on ES
    refreshPolicy match {
      case RefreshPolicy.`0` => request.setRefreshPolicy("false")
      case RefreshPolicy.`1` => request.setRefreshPolicy("true")
      case _ => request.setRefreshPolicy(refreshPolicy.toString)
    }
     */

    val result = client.httpClient.index(request, RequestOptions.DEFAULT)
    this.refresh(refreshPolicy) //FIXME: remove it when setRefreshPolicy is reliable on ES
    result
  }

  def bulkCreate(elems: List[(String, XContentBuilder)], refreshPolicy: RefreshPolicy.Value): BulkResponse = {
    val request = new BulkRequest
    elems.foreach { case (id, builder) =>
      request.add(createIndexRequest(id, builder))
    }

    val result = bulkUpdate(request, refreshPolicy)
    this.refresh
    result
  }

  def update(id: String, builder: XContentBuilder, upsert: Boolean = false,
             refreshPolicy: RefreshPolicy.Value): UpdateResponse = {
    log.debug("Update id: {}", id)
    val request: UpdateRequest = createUpdateRequest(id, builder, upsert)

    /* FIXME: use the following code when is reliable on ES
     refreshPolicy match {
       case RefreshPolicy.`0` => request.setRefreshPolicy("false")
       case RefreshPolicy.`1` => request.setRefreshPolicy("true")
       case _ => request.setRefreshPolicy(refreshPolicy.toString)
     }
     */

    val result = client.httpClient.update(request, RequestOptions.DEFAULT)
    this.refresh(refreshPolicy) //FIXME: remove it when setRefreshPolicy is reliable on ES
    result
  }

  def bulkUpdate(elems: List[(String, XContentBuilder)], upsert: Boolean = false,
                 refreshPolicy: RefreshPolicy.Value): BulkResponse = {
    val request = new BulkRequest
    elems.foreach { case (id, builder) =>
      request.add(createUpdateRequest(id, builder, upsert))
    }

    val result = bulkUpdate(request, refreshPolicy)
    this.refresh
    result
  }

  private[this] def createIndexRequest(id: String, builder: XContentBuilder): IndexRequest = {
    val indexReq = new IndexRequest()
      .index(index)
      .source(builder)
      .create(true)
      .id(id)

    indexReq
  }

  private[this] def createUpdateRequest(id: String, builder: XContentBuilder, upsert: Boolean): UpdateRequest = {
    val updateReq = new UpdateRequest()
      .index(index)
      .doc(builder)
      .id(id)
      .docAsUpsert(upsert)

    updateReq
  }

  def delete(queryBuilder: QueryBuilder, refreshPolicy: RefreshPolicy.Value): BulkByScrollResponse = {
    val request = new DeleteByQueryRequest(index)
    request.setConflicts("proceed")
    request.setQuery(queryBuilder)

    log.debug("Delete request: {}", request)

    val result = client.httpClient.deleteByQuery(request, RequestOptions.DEFAULT)
    this.refresh(refreshPolicy) //FIXME: to be replaced with wait_for when available
    result
  }

  def updateByQuery(queryBuilder: QueryBuilder,
                    script: Option[Script], batchSize: Option[Int],
                    refreshPolicy: RefreshPolicy.Value): BulkByScrollResponse = {
    val request = new UpdateByQueryRequest(index)
    request.setConflicts("proceed")
    request.setQuery(queryBuilder)
    script match {
      case Some(s) => request.setScript(s)
      case _ =>
    }
    batchSize match {
      case Some(bs) => request.setBatchSize(bs)
      case _ =>
    }

    log.debug("UpdateByQuery request: {}", request)

    val result = client.httpClient.updateByQuery(request, RequestOptions.DEFAULT)
    this.refresh(refreshPolicy) //FIXME: to be replaced with wait_for when available
    result
  }

  def delete(id: String, refreshPolicy: RefreshPolicy.Value): DeleteResponse = {
    val deleteReq = new DeleteRequest()
      .index(index)
      .id(id)

    refreshPolicy match {
      case RefreshPolicy.`0` => deleteReq.setRefreshPolicy("false")
      case RefreshPolicy.`1` => deleteReq.setRefreshPolicy("true")
      case _ => deleteReq.setRefreshPolicy(refreshPolicy.toString)
    }

    val result = client.httpClient.delete(deleteReq, RequestOptions.DEFAULT)
    this.refresh
    result
  }

  def delete(ids: List[String], refreshPolicy: RefreshPolicy.Value): BulkResponse = {
    val bulkRequest: BulkRequest = new BulkRequest()
    ids.foreach( id => {
      val request = new DeleteRequest()
        .index(index)
        .id(id)
      bulkRequest.add(request)
    })

    bulkUpdate(bulkRequest, refreshPolicy)
  }

  def refresh: RefreshIndexResult = {
    client.refresh(index)
  }

  def refresh(refreshPolicy: RefreshPolicy.Value): RefreshIndexResult = {
    refreshPolicy match {
      case RefreshPolicy.`1` | RefreshPolicy.`wait_for` | RefreshPolicy.`true` =>
        refresh
      case _ =>
    }
    client.refresh(index)
  }

  private[this] def bulkUpdate(request: BulkRequest, refreshPolicy: RefreshPolicy.Value): BulkResponse = {
    /* FIXME: use the following code when is reliable on ES
    refreshPolicy match {
      case RefreshPolicy.`0` => request.setRefreshPolicy("false")
      case RefreshPolicy.`1` => request.setRefreshPolicy("true")
      case _ => request.setRefreshPolicy(refreshPolicy.toString)
    }
    */
    val result = client.httpClient.bulk(request, RequestOptions.DEFAULT)
    this.refresh(refreshPolicy)
    result
  }

}
