package com.getjenny.starchat.services

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 23/08/17.
  */

import akka.event.{Logging, LoggingAdapter}
import com.getjenny.starchat.SCActorSystem
import com.getjenny.starchat.entities.{DeleteDocumentsResult, DtReloadTimestamp}
import com.getjenny.starchat.services.esclient.SystemIndexManagementElasticClient
import com.getjenny.starchat.utils.Index
import org.elasticsearch.action.get.{GetRequest, GetResponse}
import org.elasticsearch.action.search.{SearchRequest, SearchResponse}
import org.elasticsearch.action.update.{UpdateRequest, UpdateResponse}
import org.elasticsearch.client.{RequestOptions, RestHighLevelClient}
import org.elasticsearch.common.unit.TimeValue
import org.elasticsearch.common.xcontent.XContentBuilder
import org.elasticsearch.common.xcontent.XContentFactory._
import org.elasticsearch.index.query.{BoolQueryBuilder, QueryBuilders}
import org.elasticsearch.search.SearchHit
import org.elasticsearch.search.builder.SearchSourceBuilder
import org.elasticsearch.search.sort.SortOrder
import scalaz.Scalaz._

import scala.collection.JavaConverters._
import scala.collection.immutable.Map
import scala.concurrent.Future

object DtReloadService extends AbstractDataService {
  val DT_RELOAD_TIMESTAMP_DEFAULT : Long = 0
  override val elasticClient: SystemIndexManagementElasticClient.type = SystemIndexManagementElasticClient
  private[this] val log: LoggingAdapter = Logging(SCActorSystem.system, this.getClass.getCanonicalName)
  private[this] val indexName: String =
    Index.indexName(elasticClient.indexName, elasticClient.systemRefreshDtIndexSuffix)

  def updateDTReloadTimestamp(dtIndexName: String, timestamp:  Long = DT_RELOAD_TIMESTAMP_DEFAULT, refresh: Int = 0):
  Future[Option[DtReloadTimestamp]] = Future {
    val client: RestHighLevelClient = elasticClient.httpClient
    val ts: Long = if (timestamp === DT_RELOAD_TIMESTAMP_DEFAULT) System.currentTimeMillis else timestamp

    val builder : XContentBuilder = jsonBuilder().startObject()
    builder.field(elasticClient.dtReloadTimestampFieldName, ts)
    builder.endObject()

    val updateReq = new UpdateRequest()
      .index(indexName)
      .doc(builder)
      .id(dtIndexName)
      .docAsUpsert(true)

    val response: UpdateResponse = client.update(updateReq, RequestOptions.DEFAULT)

    log.debug("dt reload timestamp response status: " + response.status())

    if (refresh =/= 0) {
      val refreshIndex = elasticClient
        .refresh(indexName)
      if(refreshIndex.failedShardsN > 0) {
        throw new Exception("System: index refresh failed: (" + indexName + ", " + dtIndexName + ")")
      }
    }

    Option {DtReloadTimestamp(indexName = indexName, timestamp = ts)}
  }

  def dTReloadTimestamp(dtIndexName: String) : DtReloadTimestamp = {
    val client: RestHighLevelClient = elasticClient.httpClient
    val dtReloadDocId: String = dtIndexName

    val getReq = new GetRequest()
      .index(indexName)
      .id(dtReloadDocId)

    val response: GetResponse = client.get(getReq, RequestOptions.DEFAULT)

    val timestamp = if(! response.isExists || response.isSourceEmpty) {
      log.debug("dt reload timestamp field is empty or does not exists")
      DT_RELOAD_TIMESTAMP_DEFAULT
    } else {
      val source : Map[String, Any] = response.getSource.asScala.toMap
      val loadedTs : Long = source.get(elasticClient.dtReloadTimestampFieldName) match {
        case Some(t) => t.asInstanceOf[Long]
        case None => DT_RELOAD_TIMESTAMP_DEFAULT
      }
      log.debug("dt reload timestamp is: " + loadedTs)
      loadedTs
    }

    DtReloadTimestamp(indexName = dtIndexName, timestamp = timestamp)
  }

  def deleteEntry(ids: List[String]): DeleteDocumentsResult = {
    delete(indexName = indexName, ids = ids, refresh = 0)
  }

  def allDTReloadTimestamp(minTimestamp: Option[Long] = None,
                           maxItems: Option[Long] = None): List[DtReloadTimestamp] = {
    val client: RestHighLevelClient = elasticClient.httpClient
    val boolQueryBuilder : BoolQueryBuilder = QueryBuilders.boolQuery()
    minTimestamp match {
      case Some(minTs) => boolQueryBuilder.filter(
        QueryBuilders.rangeQuery(elasticClient.dtReloadTimestampFieldName).gt(minTs))
      case _ => ;
    }

    val sourceReq: SearchSourceBuilder = new SearchSourceBuilder()
      .query(boolQueryBuilder)
      .size(maxItems.getOrElse(100L).toInt)
      .version(true)
      .sort(elasticClient.dtReloadTimestampFieldName, SortOrder.ASC)

    val searchReq = new SearchRequest(indexName)
      .source(sourceReq)
      .scroll(new TimeValue(60000))

    val scrollResp : SearchResponse = client.search(searchReq, RequestOptions.DEFAULT)

    val dtReloadTimestamps : List[DtReloadTimestamp] = scrollResp.getHits.getHits.toList.map({ timestampEntry =>
      val item: SearchHit = timestampEntry
      val docId : String = item.getId // the id is the index name
    val source : Map[String, Any] = item.getSourceAsMap.asScala.toMap

      val timestamp : Long = source.get(elasticClient.dtReloadTimestampFieldName) match {
        case Some(t) => t.asInstanceOf[Long]
        case None => DT_RELOAD_TIMESTAMP_DEFAULT
      }

      DtReloadTimestamp(docId, timestamp)
    })
    dtReloadTimestamps
  }

}
