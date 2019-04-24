package com.getjenny.starchat.services

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 23/08/17.
  */

import java.util.UUID.randomUUID

import akka.event.{Logging, LoggingAdapter}
import com.getjenny.starchat.SCActorSystem
import com.getjenny.starchat.entities.{ClusterNode, ClusterNodes, DeleteDocumentsSummaryResult}
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
import org.elasticsearch.index.reindex.DeleteByQueryRequest
import org.elasticsearch.search.SearchHit
import org.elasticsearch.search.builder.SearchSourceBuilder
import scalaz.Scalaz._

import scala.collection.JavaConverters._
import scala.collection.immutable.Map

case class ClusterNodesServiceException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)

object ClusterNodesService extends AbstractDataService {
  val CLUSTER_NODE_ALIVE_TIMESTAMP_DEFAULT : Long = -1
  override val elasticClient: SystemIndexManagementElasticClient.type = SystemIndexManagementElasticClient
  val uuid: String = randomUUID.toString
  private[this] val log: LoggingAdapter = Logging(SCActorSystem.system, this.getClass.getCanonicalName)
  val indexName: String = Index.indexName(elasticClient.indexName, elasticClient.systemClusterNodesIndexSuffix)

  def alive(refresh: Int = 0): ClusterNode = {
    val client: RestHighLevelClient = elasticClient.httpClient
    val timestamp: Long = System.currentTimeMillis

    val builder : XContentBuilder = jsonBuilder().startObject()
    builder.field("timestamp", timestamp)
    builder.endObject()

    val updateReq = new UpdateRequest()
      .index(indexName)
      .doc(builder)
      .id(uuid)
      .docAsUpsert(true)

    val response: UpdateResponse = client.update(updateReq, RequestOptions.DEFAULT)

    log.debug("set alive node status: " + response.status())

    if (refresh =/= 0) {
      val refreshIndex = elasticClient.refresh(indexName)
      if(refreshIndex.failedShardsN > 0) {
        throw ClusterNodesServiceException("System: index refresh failed: (" + indexName + ")")
      }
    }

    ClusterNode(uuid = uuid, alive = true, timestamp = timestamp)
  }

  def isAlive(uuid: String) : ClusterNode = {
    val client: RestHighLevelClient = elasticClient.httpClient
    val currTimestamp: Long = System.currentTimeMillis

    val getReq = new GetRequest()
      .index(indexName)
      .id(uuid)

    val response: GetResponse = client.get(getReq, RequestOptions.DEFAULT)

    val timestamp = if(! response.isExists || response.isSourceEmpty) {
      log.debug("cluster node is field is empty or does not exists")
      CLUSTER_NODE_ALIVE_TIMESTAMP_DEFAULT
    } else {
      val source : Map[String, Any] = response.getSource.asScala.toMap
      val aliveTs : Long = source.get("timestamp") match {
        case Some(t) => t.asInstanceOf[Long]
        case None =>
          CLUSTER_NODE_ALIVE_TIMESTAMP_DEFAULT
      }
      log.debug("UUID(" + uuid + ") last time was seen alive: " + aliveTs)
      aliveTs
    }

    val aliveTimeCheck = timestamp > CLUSTER_NODE_ALIVE_TIMESTAMP_DEFAULT &&
      currTimestamp - timestamp <= elasticClient.clusterNodeAliveMaxInterval * 1000
    ClusterNode(uuid = uuid, alive = aliveTimeCheck, timestamp = timestamp)
  }

  def cleanDeadNodes: DeleteDocumentsSummaryResult = {
    val client: RestHighLevelClient = elasticClient.httpClient
    val boolQueryBuilder : BoolQueryBuilder = QueryBuilders.boolQuery()

    val currTimestamp: Long = System.currentTimeMillis
    val minTs = currTimestamp - (elasticClient.clusterNodeAliveMaxInterval * 1000)

    boolQueryBuilder.filter(QueryBuilders.rangeQuery("timestamp").lt(minTs))

    val request: DeleteByQueryRequest =
      new DeleteByQueryRequest(indexName)
    request.setConflicts("proceed")
    request.setQuery(boolQueryBuilder)

    val bulkResponse = client.deleteByQuery(request, RequestOptions.DEFAULT)

    DeleteDocumentsSummaryResult(message = "delete", deleted = bulkResponse.getTotal)
  }

  def aliveNodes: ClusterNodes = {
    val client: RestHighLevelClient = elasticClient.httpClient
    val boolQueryBuilder : BoolQueryBuilder = QueryBuilders.boolQuery()
    val currTimestamp: Long = System.currentTimeMillis
    val minTs = currTimestamp - (elasticClient.clusterNodeAliveMaxInterval * 1000)
    boolQueryBuilder.filter(QueryBuilders.rangeQuery("timestamp").gte(minTs))

    val sourceReq: SearchSourceBuilder = new SearchSourceBuilder()
      .query(boolQueryBuilder)

    val searchReq = new SearchRequest(indexName)
      .source(sourceReq)
      .scroll(new TimeValue(60000))

    val scrollResp : SearchResponse = client.search(searchReq, RequestOptions.DEFAULT)

    val dtReloadTimestamps : List[ClusterNode] = scrollResp.getHits.getHits.toList.map({ timestampEntry =>
      val item: SearchHit = timestampEntry
      val docId : String = item.getId // the id is the index name
    val source : Map[String, Any] = item.getSourceAsMap.asScala.toMap

      val timestamp : Long = source.get("timestamp") match {
        case Some(t) => t.asInstanceOf[Long]
        case None => CLUSTER_NODE_ALIVE_TIMESTAMP_DEFAULT
      }

      ClusterNode(docId, timestamp > CLUSTER_NODE_ALIVE_TIMESTAMP_DEFAULT, timestamp)
    })
    ClusterNodes(uuid = uuid, nodes = dtReloadTimestamps)
  }

}
