package com.getjenny.starchat.services.esclient

/**
 * Created by Angelo Leto <angelo@getjenny.com> on 01/07/16.
 */

import com.getjenny.starchat.entities.io.{FailedShard, RefreshIndexResult}
import com.typesafe.config.{Config, ConfigFactory}
import org.elasticsearch.action.admin.indices.refresh.{RefreshRequest, RefreshResponse}
import org.elasticsearch.client.indices._
import org.elasticsearch.client.{RequestOptions, RestHighLevelClient}

import scala.collection.immutable.List

trait ElasticClient {
  val config: Config = ConfigFactory.load()
  val clusterName: String = config.getString("es.cluster_name")
  val sniff: Boolean = config.getBoolean("es.enable_sniff")
  val ignoreClusterName: Boolean = config.getBoolean("es.ignore_cluster_name")
  val instanceFieldName = "instance"

  def refresh(index: String): RefreshIndexResult = {
    val refreshReq = new RefreshRequest(index)
    val refreshRes: RefreshResponse =
      httpClient.indices().refresh(refreshReq, RequestOptions.DEFAULT)

    val failedShards: List[FailedShard] = refreshRes.getShardFailures.map(item => {
      val failedShardItem = FailedShard(indexName = item.index,
        shardId = item.shardId,
        reason = item.reason,
        status = item.status.getStatus
      )
      failedShardItem
    }).toList

    val refreshIndexResult =
      RefreshIndexResult(indexName = index,
        failedShardsN = refreshRes.getFailedShards,
        successfulShardsN = refreshRes.getSuccessfulShards,
        totalShardsN = refreshRes.getTotalShards,
        failedShards = failedShards
      )
    refreshIndexResult
  }

  def httpClient: RestHighLevelClient = ElasticsearchClientSingleton.esHttpClient

  def existsIndices(indices: List[String]): Boolean = {
    val request = new GetIndexRequest(indices:_*)
    request.local(false)
    request.humanReadable(true)
    request.includeDefaults(false)
    httpClient.indices().exists(request, RequestOptions.DEFAULT)
  }

  def closeHttp(): Unit = {
    httpClient.close()
  }

  val indexName: String
  val indexSuffix: String

  val commonIndexArbitraryPattern: String = config.getString("es.common_index_arbitrary_pattern")
  val commonIndexDefaultOrgPattern: String = config.getString("es.common_index_default_org_pattern")

  val enableDeleteIndex: Boolean = config.getBoolean("es.enable_delete_application_index")

  val mappingPath: String
  val updateMappingPath: String
  val numberOfShards: Int
  val numberOfReplicas: Int
}
