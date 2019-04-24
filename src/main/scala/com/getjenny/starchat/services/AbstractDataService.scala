package com.getjenny.starchat.services

import com.getjenny.starchat.SCActorSystem
import com.getjenny.starchat.entities.{DeleteDocumentResult, DeleteDocumentsResult, DeleteDocumentsSummaryResult}
import com.getjenny.starchat.services.esclient.ElasticClient
import com.getjenny.starchat.utils.Index
import org.elasticsearch.action.bulk.{BulkRequest, BulkResponse}
import org.elasticsearch.action.delete.DeleteRequest
import org.elasticsearch.client.{RequestOptions, RestHighLevelClient}
import org.elasticsearch.index.query.QueryBuilders
import org.elasticsearch.index.reindex.DeleteByQueryRequest
import org.elasticsearch.rest.RestStatus
import scalaz.Scalaz._

import scala.collection.immutable.List
import scala.concurrent.{ExecutionContext, Future}

case class DeleteDataServiceException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)

trait AbstractDataService {
  implicit def executionContext: ExecutionContext = SCActorSystem.system.dispatchers.lookup("starchat.dispatcher")

  protected[this] val elasticClient: ElasticClient

  /** delete all the terms in a table
    *
    * @param indexName index name
    * @return a DeleteDocumentsResult with the status of the delete operation
    */
  def deleteAll(indexName: String): DeleteDocumentsSummaryResult = {
    val client: RestHighLevelClient = elasticClient.httpClient

    val request: DeleteByQueryRequest =
      new DeleteByQueryRequest(Index.indexName(indexName, elasticClient.indexSuffix))
    request.setConflicts("proceed")
    request.setQuery(QueryBuilders.matchAllQuery)

    val bulkResponse = client.deleteByQuery(request, RequestOptions.DEFAULT)

    DeleteDocumentsSummaryResult(message = "delete", deleted = bulkResponse.getTotal)
  }

  /** delete one or more terms
    *
    * @param indexName index name
    * @param ids the list of term ids to delete
    * @param refresh whether to call an index update on ElasticSearch or not
    * @return DeleteDocumentListResult with the result of term delete operations
    */
  def delete(indexName: String, ids: List[String], refresh: Int): DeleteDocumentsResult = {
    val client: RestHighLevelClient = elasticClient.httpClient

    val bulkReq : BulkRequest = new BulkRequest()

    ids.foreach( id => {
      val deleteReq = new DeleteRequest()
        .index(Index.indexName(indexName, elasticClient.indexSuffix))
        .id(id)
      bulkReq.add(deleteReq)
    })

    val bulkRes: BulkResponse = client.bulk(bulkReq, RequestOptions.DEFAULT)

    if (refresh =/= 0) {
      val refreshIndex = elasticClient
        .refresh(Index.indexName(indexName, elasticClient.indexSuffix))
      if(refreshIndex.failedShardsN > 0) {
        throw DeleteDataServiceException("index refresh failed: (" + indexName + ")")
      }
    }

    val listOfDocRes: List[DeleteDocumentResult] = bulkRes.getItems.map(x => {
      DeleteDocumentResult(x.getIndex, x.getId, x.getVersion, x.status =/= RestStatus.NOT_FOUND)
    }).toList

    DeleteDocumentsResult(data=listOfDocRes)
  }
}
