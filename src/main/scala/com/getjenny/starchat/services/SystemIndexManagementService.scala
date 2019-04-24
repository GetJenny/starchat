package com.getjenny.starchat.services

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 22/11/17.
  */

import java.io._

import com.getjenny.starchat.entities.{IndexManagementResponse, _}
import com.getjenny.starchat.services.esclient.SystemIndexManagementElasticClient
import org.elasticsearch.action.admin.cluster.health.ClusterHealthRequest
import org.elasticsearch.action.admin.indices.delete.DeleteIndexRequest
import org.elasticsearch.action.support.master.AcknowledgedResponse
import org.elasticsearch.client.indices._
import org.elasticsearch.client.{RequestOptions, RestHighLevelClient}
import org.elasticsearch.common.settings._
import org.elasticsearch.common.xcontent.XContentType
import scalaz.Scalaz._

import scala.collection.JavaConverters._
import scala.concurrent.Future
import scala.io.Source

case class SystemIndexManagementServiceException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)

/**
  * Implements functions, eventually used by IndexManagementResource, for ES index management
  */
object SystemIndexManagementService extends AbstractDataService {
  val elasticClient: SystemIndexManagementElasticClient.type = SystemIndexManagementElasticClient

  private[this] val analyzerJsonPath: String = "/index_management/json_index_spec/system/analyzer.json"
  private[this] val analyzerJsonIs: Option[InputStream] = Option {
    getClass.getResourceAsStream(analyzerJsonPath)
  }
  private[this] val analyzerJson: String = analyzerJsonIs match {
    case Some(stream) => Source.fromInputStream(stream, "utf-8").mkString
    case _ =>
      val message = "Check the file: (" + analyzerJsonPath + ")"
      throw new FileNotFoundException(message)
  }

  private[this] val schemaFiles: List[JsonMappingAnalyzersIndexFiles] = List[JsonMappingAnalyzersIndexFiles](
    JsonMappingAnalyzersIndexFiles(path = "/index_management/json_index_spec/system/user.json",
      updatePath = "/index_management/json_index_spec/system/update/user.json",
      indexSuffix = elasticClient.userIndexSuffix),
    JsonMappingAnalyzersIndexFiles(path = "/index_management/json_index_spec/system/refresh_decisiontable.json",
      updatePath = "/index_management/json_index_spec/system/update/refresh_decisiontable.json",
      indexSuffix = elasticClient.systemRefreshDtIndexSuffix),
    JsonMappingAnalyzersIndexFiles(path = "/index_management/json_index_spec/system/cluster_nodes.json",
      updatePath = "/index_management/json_index_spec/system/update/cluster_nodes.json",
      indexSuffix = elasticClient.systemClusterNodesIndexSuffix),
    JsonMappingAnalyzersIndexFiles(path = "/index_management/json_index_spec/system/decision_table_node_status.json",
      updatePath = "/index_management/json_index_spec/system/update/decision_table_node_status.json",
      indexSuffix = elasticClient.systemDtNodesStatusIndexSuffix)
  )

  def create(indexSuffix: Option[String] = None): Future[IndexManagementResponse] = Future {
    val client: RestHighLevelClient = elasticClient.httpClient

    val operationsMessage: List[(String, Boolean)] = schemaFiles.filter(item => {
      indexSuffix match {
        case Some(t) => t === item.indexSuffix
        case _ => true
      }
    }).map(item => {
      val jsonInStream: Option[InputStream] = Option {
        getClass.getResourceAsStream(item.path)
      }

      val schemaJson = jsonInStream match {
        case Some(stream) => Source.fromInputStream(stream, "utf-8").mkString
        case _ =>
          val message = "Check the file: (" + item.path + ")"
          throw new FileNotFoundException(message)
      }

      val fullIndexName = elasticClient.indexName + "." + item.indexSuffix

      val createIndexReq = new CreateIndexRequest(fullIndexName).settings(
        Settings.builder().loadFromSource(analyzerJson, XContentType.JSON)
          .put("index.number_of_shards", elasticClient.numberOfShards)
          .put("index.number_of_replicas", elasticClient.numberOfReplicas)
        ).source(schemaJson, XContentType.JSON)

      val createIndexRes: CreateIndexResponse = client.indices.create(createIndexReq, RequestOptions.DEFAULT)

      (item.indexSuffix + "(" + fullIndexName + ", " + createIndexRes.isAcknowledged + ")",
        createIndexRes.isAcknowledged)
    })

    val message = "IndexCreation: " + operationsMessage.map{ case (msg, _) => msg}.mkString(" ")
    IndexManagementResponse(message = message, check = operationsMessage.forall{case(_, ck) => ck})
  }

  def remove(indexSuffix: Option[String] = None): Future[IndexManagementResponse] = Future {
    val client: RestHighLevelClient = elasticClient.httpClient

    if (!elasticClient.enableDeleteSystemIndex) {
      val message: String = "operation is not allowed, contact system administrator"
      throw SystemIndexManagementServiceException(message)
    }

    val operationsMessage: List[(String, Boolean)] = schemaFiles.filter(item => {
      indexSuffix match {
        case Some(t) => t === item.indexSuffix
        case _ => true
      }
    }).map(item => {
      val fullIndexName = elasticClient.indexName + "." + item.indexSuffix

      val deleteIndexReq = new DeleteIndexRequest(fullIndexName)

      val deleteIndexRes: AcknowledgedResponse = client.indices.delete(deleteIndexReq, RequestOptions.DEFAULT)

      (item.indexSuffix + "(" + fullIndexName + ", " + deleteIndexRes.isAcknowledged + ")",
        deleteIndexRes.isAcknowledged)
    })

    val message = "IndexDeletion: " + operationsMessage.map{ case (msg, _) => msg}.mkString(" ")
    IndexManagementResponse(message = message, check = operationsMessage.forall{case(_, ck) => ck})
  }

  def check(indexSuffix: Option[String] = None): Future[IndexManagementResponse] = Future {
    val client: RestHighLevelClient = elasticClient.httpClient

    val operationsMessage: List[(String, Boolean)] = schemaFiles.filter(item => {
      indexSuffix match {
        case Some(t) => t === item.indexSuffix
        case _ => true
      }
    }).map(item => {
      val fullIndexName = elasticClient.indexName + "." + item.indexSuffix
      val getMappingsReq: GetMappingsRequest = new GetMappingsRequest()
        .indices(fullIndexName)

      val getMappingsRes: GetMappingsResponse = client.indices.getMapping(getMappingsReq, RequestOptions.DEFAULT)

      val check = getMappingsRes.mappings.containsKey(fullIndexName)
      (item.indexSuffix + "(" + fullIndexName + ", " + check + ")", check)
    })

    val message = "IndexCheck: " + operationsMessage.map{ case (msg, _) => msg}.mkString(" ")
    IndexManagementResponse(message = message, check = operationsMessage.forall{case(_, ck) => ck})
  }

  def update(indexSuffix: Option[String] = None): Future[IndexManagementResponse] = Future {
    val client: RestHighLevelClient = elasticClient.httpClient

    val operationsMessage: List[(String, Boolean)] = schemaFiles.filter(item => {
      indexSuffix match {
        case Some(t) => t === item.indexSuffix
        case _ => true
      }
    }).map(item => {
      val jsonInStream: Option[InputStream] = Option {
        getClass.getResourceAsStream(item.updatePath)
      }
      val schemaJson: String = jsonInStream match {
        case Some(stream) => Source.fromInputStream(stream, "utf-8").mkString
        case _ =>
          val message = "Check the file: (" + item.path + ")"
          throw new FileNotFoundException(message)
      }

      val fullIndexName = elasticClient.indexName + "." + item.indexSuffix

      val putMappingReq = new PutMappingRequest(fullIndexName)
        .source(schemaJson, XContentType.JSON)

      val putMappingRes: AcknowledgedResponse = client.indices
        .putMapping(putMappingReq, RequestOptions.DEFAULT)

      val check = putMappingRes.isAcknowledged
      (item.indexSuffix + "(" + fullIndexName + ", " + check + ")", check)
    })

    val message = "IndexUpdate: " + operationsMessage.map{ case (msg, _) => msg}.mkString(" ")
    IndexManagementResponse(message = message, check = operationsMessage.forall{case(_, ck) => ck})
  }

  def refresh(indexSuffix: Option[String] = None): Future[Option[RefreshIndexResults]] = Future {
    val operationsResults: List[RefreshIndexResult] = schemaFiles.filter(item => {
      indexSuffix match {
        case Some(t) => t === item.indexSuffix
        case _ => true
      }
    }).map(item => {
      val fullIndexName = elasticClient.indexName + "." + item.indexSuffix
      val refreshIndexRes: RefreshIndexResult = elasticClient.refresh(fullIndexName)
      if (refreshIndexRes.failedShardsN > 0) {
        val indexRefreshMessage = item.indexSuffix + "(" + fullIndexName + ", " + refreshIndexRes.failedShardsN + ")"
        throw SystemIndexManagementServiceException(indexRefreshMessage)
      }

      refreshIndexRes
    })

    Option {
      RefreshIndexResults(results = operationsResults)
    }
  }

  def indices: Future[List[String]] = Future {
    val clusterHealthReq = new ClusterHealthRequest()
    clusterHealthReq.level(ClusterHealthRequest.Level.INDICES)
    val clusterHealthRes = elasticClient.httpClient.cluster().health(clusterHealthReq, RequestOptions.DEFAULT)
    clusterHealthRes.getIndices.asScala.map { case (k, _) => k }.toList
  }

}
