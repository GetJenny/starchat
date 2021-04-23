package com.getjenny.starchat.services

/**
 * Created by Angelo Leto <angelo@getjenny.com> on 23/08/17.
 */

import akka.event.{Logging, LoggingAdapter}
import com.getjenny.starchat.SCActorSystem
import com.getjenny.starchat.entities.io.{DtReloadTimestamp, IndexManagementResponse, IndexManagementStatusResponse, RefreshPolicy}
import com.getjenny.starchat.entities.io.{InstanceRegistryDocument => InstanceRegistryDocumentCC}
import com.getjenny.starchat.services.esclient.crud.EsCrudBase
import com.getjenny.starchat.services.esclient.{InstanceRegistryElasticClient, NodeDtLoadingStatusElasticClient}
import com.getjenny.starchat.utils.Index
import org.elasticsearch.action.update.UpdateResponse
import org.elasticsearch.index.query.{BoolQueryBuilder, QueryBuilder, QueryBuilders}
import org.elasticsearch.search.SearchHit
import org.elasticsearch.search.sort.{FieldSortBuilder, SortOrder}
import scalaz.Scalaz._

import scala.collection.JavaConverters._
import scala.collection.immutable.Map
import scala.util.{Failure, Success, Try}

case class InstanceRegistryException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)

object InstanceRegistryDocument {
  val InstanceRegistryTimestampDefault: Long = 0L

  def apply(source: Map[String, Any]): InstanceRegistryDocumentCC = {
    val timestamp = source.get("timestamp") match {
      case Some(value: Long) => value
      case Some(value: Int) => value.toLong
      case _ => InstanceRegistryTimestampDefault
    }
    val enabled = source.get("enabled").map(_.asInstanceOf[Boolean])
    val delete = source.get("delete").map(_.asInstanceOf[Boolean])
    val isDeleted = source.get("deleted").map(_.asInstanceOf[Boolean])
    val incremental = source.get("incremental").map(_.asInstanceOf[Boolean])
    val staleAge = source.get("staleAge") match {
      case Some(value: Long) => value
      case Some(value: Int) => value.toLong
      case _ => 0L
    }
    InstanceRegistryDocumentCC(timestamp.some, enabled, delete, isDeleted, incremental, staleAge.some)
  }

  def empty: InstanceRegistryDocumentCC = {
    InstanceRegistryDocumentCC()
  }
}

object InstanceRegistryStatus extends Enumeration {
  type InstanceRegistryStatus = Value
  val Missing: InstanceRegistryStatus = Value("Missing")
  val MarkedForDeletion: InstanceRegistryStatus = Value("MarkedForDeletion")
  val Enabled: InstanceRegistryStatus = Value("Enabled")
  val Disabled: InstanceRegistryStatus = Value("Disabled")
}

object InstanceRegistryService extends AbstractDataService {
  override val elasticClient: InstanceRegistryElasticClient.type = InstanceRegistryElasticClient
  private[this] val nodeDtLoadingStatusElasticClient: NodeDtLoadingStatusElasticClient.type = NodeDtLoadingStatusElasticClient
  private[this] val log: LoggingAdapter = Logging(SCActorSystem.system, this.getClass.getCanonicalName)
  private[this] val instanceRegistryIndex: String = Index.indexName(elasticClient.indexName, elasticClient.indexSuffix)
  private[this] val languageIndexManagementService: LanguageIndexManagementService.type = LanguageIndexManagementService
  private[this] val esCrudBase = new EsCrudBase(elasticClient, instanceRegistryIndex)
  private[this] val userEsService: AbstractUserService = UserService.service

  def updateTimestamp(dtIndexName: String,
                      timestamp: Long =
                      InstanceRegistryDocument.InstanceRegistryTimestampDefault,
                      incremental: Boolean = true
                     ): Option[DtReloadTimestamp] = {
    val ts: Long = if (timestamp === InstanceRegistryDocument.InstanceRegistryTimestampDefault)
      System.currentTimeMillis
    else
      timestamp

    val response = updateInstance(dtIndexName, timestamp = ts.some,
      enabled = None, delete = None, deleted = None, incremental = Some(incremental),
      refreshPolicy = RefreshPolicy.`wait_for`, staleAge = None)

    log.debug("dt reload timestamp response status: {}", response.status())

    DtReloadTimestamp(indexName = instanceRegistryIndex, timestamp = ts, incremental = incremental).some
  }

  def addInstance(indexName: String): IndexManagementResponse = {
    if (!isValidIndexName(indexName) || !elasticSearchIndexExists(indexName))
      throw new IllegalArgumentException(s"Index name $indexName is not a valid index to be used with instanceRegistry")

    val document = com.getjenny.starchat.entities.io.InstanceRegistryDocument(
      timestamp = InstanceRegistryDocument.InstanceRegistryTimestampDefault.some,
      enabled = true.some,
      delete = false.some,
      deleted = false.some,
      incremental = true.some,
      staleAge = 0L.some
    )

    val instanceRegistryDocument = findInstance(indexName)

    if (!instanceRegistryDocument.isEmpty)
      throw new InstanceRegistryException(s"Instance ($indexName) already exists, delete it to recreate")

    val response = esCrudBase.update(indexName, document.builder, upsert = true,
      refreshPolicy = RefreshPolicy.`wait_for`)

    IndexManagementResponse(s"Created instance $indexName, operation status: ${response.status}", check = true)
  }

  def getInstance(indexName: String): InstanceRegistryDocumentCC = {
    if (!isValidIndexName(indexName))
      throw new IllegalArgumentException(s"Index name $indexName is not a valid index to be used with instanceRegistry")
    findInstance(indexName)
  }

  def checkInstance(indexName: String): IndexManagementStatusResponse = {
    val document = findInstance(indexName)

    IndexManagementStatusResponse(message = Some(s"Index check for instance: $indexName"), document.status().toString)
  }

  def enableInstance(indexName: String): IndexManagementResponse = {
    require(!findInstance(indexName).isEmpty, s"Instance $indexName does not exists")

    val response = updateInstance(indexName, timestamp = None,
      enabled = true.some, delete = None, deleted = None, incremental = None,
      refreshPolicy = RefreshPolicy.`wait_for`, staleAge = None)
    userEsService.enablePermissionForIndex(indexName)
    IndexManagementResponse(s"Enabled instance $indexName, operation status: ${response.status}", check = true)
  }

  def disableInstance(indexName: String): IndexManagementResponse = {
    require(!findInstance(indexName).isEmpty, s"Instance $indexName does not exists")

    val response = updateInstance(indexName, timestamp = None,
      enabled = false.some, delete = None, deleted = None, incremental = None,
      refreshPolicy = RefreshPolicy.`wait_for`, staleAge = None)
    userEsService.disablePermissionForIndex(indexName)
    IndexManagementResponse(s"Disabled instance $indexName, operation status: ${response.status}", check = true)
  }

  def markDeleteInstance(indexName: String): IndexManagementResponse = {
    require(!findInstance(indexName).isEmpty, s"Instance $indexName does not exists")

    val response = updateInstance(indexName, timestamp = None, enabled = false.some,
      delete = true.some, deleted = None, incremental = None,
      refreshPolicy= RefreshPolicy.`wait_for`, staleAge = None)
    userEsService.disablePermissionForIndex(indexName)
    IndexManagementResponse(s"Mark Delete instance $indexName, operation status: ${response.status}", check = true)
  }

  def updateInstance(indexName: String,
                                   timestamp: Option[Long],
                                   enabled: Option[Boolean],
                                   delete: Option[Boolean],
                                   deleted: Option[Boolean],
                                   incremental: Option[Boolean],
                                   staleAge: Option[Long],
                                   refreshPolicy: RefreshPolicy.Value): UpdateResponse = {
    if (!isValidIndexName(indexName))
      throw new IllegalArgumentException(s"Index name $indexName is not a valid index to be used with instanceRegistry")

    val toBeUpdated = com.getjenny.starchat.entities.io.InstanceRegistryDocument(timestamp = timestamp,
      enabled = enabled,
      delete = delete,
      deleted = deleted,
      incremental = incremental,
      staleAge = staleAge)

    val response = esCrudBase.update(id = indexName,
      builder = toBeUpdated.builder,
      refreshPolicy = refreshPolicy)

    val updatedDocument = findInstance(indexName)
    log.debug("Updated instance {} with document: {}", indexName, updatedDocument)
    response
  }

  private[this] def findInstance(dtIndexName: String): InstanceRegistryDocumentCC = Try {
    esCrudBase.read(dtIndexName)
  } match {
    case Success(response) => if (!response.isExists || response.isSourceEmpty) {
      InstanceRegistryDocument.empty
    } else {
      val document = InstanceRegistryDocument(response.getSource.asScala.toMap)
      document.deleted match {
        case Some(true) => InstanceRegistryDocument.empty
        case _ => document
      }
    }
    case Failure(e) =>
      log.error(e, "Error while reading from instance-registry - id: {}", dtIndexName)
      InstanceRegistryDocument.empty
  }

  def instanceTimestamp(dtIndexName: String): DtReloadTimestamp = {
    val document = findInstance(dtIndexName)

    DtReloadTimestamp(indexName = dtIndexName, timestamp = document.timestamp
      .getOrElse(InstanceRegistryDocument.InstanceRegistryTimestampDefault))
  }

  def markAsDeleted(ids: List[String]): Unit = {
    ids.foreach { entry =>
      updateInstance(entry, None, enabled = false.some, delete = false.some, deleted = true.some,
        incremental = false.some,
        refreshPolicy = RefreshPolicy.`wait_for`, staleAge = None)
      userEsService.disablePermissionForIndex(entry)
    }
  }

  def getAll(): Map[String, InstanceRegistryDocumentCC] = {
    queryRegistry(QueryBuilders.matchAllQuery())
  }

  def getAllMarkedToDelete: Map[String, InstanceRegistryDocumentCC] = {
    val queryBuilder = QueryBuilders.boolQuery().filter(QueryBuilders.termQuery("delete", true))
    queryRegistry(queryBuilder)
  }

  private[this] def queryRegistry(queryBuilder: QueryBuilder): Map[String, InstanceRegistryDocumentCC] = {
    esCrudBase.scroll(queryBuilder).flatMap { e =>
      e.getHits.getHits.map { x =>
        (x.getId, InstanceRegistryDocument(x.getSourceAsMap.asScala.toMap))
      }
    }.toMap
  }

  def allEnabledInstanceTimestamp(minTimestamp: Option[Long] = None,
                                  maxItems: Option[Long] = None): List[DtReloadTimestamp] = {
    val boolQueryBuilder: BoolQueryBuilder = QueryBuilders.boolQuery()
    minTimestamp match {
      case Some(minTs) => boolQueryBuilder.filter(
        QueryBuilders.rangeQuery(nodeDtLoadingStatusElasticClient.dtReloadTimestampFieldName).gt(minTs))
      case _ => ;
    }
    boolQueryBuilder.filter(QueryBuilders.termQuery("enabled", true))
    val response = esCrudBase.read(boolQueryBuilder,
      maxItems = maxItems.orElse(10000L.some).map(_.toInt),
      version = true.some,
      sort =
        List(new FieldSortBuilder(nodeDtLoadingStatusElasticClient.dtReloadTimestampFieldName).order(SortOrder.DESC))
    )

    val dtReloadTimestamps: List[DtReloadTimestamp] = response.getHits.getHits.toList
      .map { timestampEntry =>
        val item: SearchHit = timestampEntry
        val docId: String = item.getId // the id is the index name
        val document = InstanceRegistryDocument(item.getSourceAsMap.asScala.toMap)
        DtReloadTimestamp(docId, document.timestamp
          .getOrElse(InstanceRegistryDocument.InstanceRegistryTimestampDefault),
          incremental = document.incremental.getOrElse(false)
        )
      }
    dtReloadTimestamps
  }

  def isValidIndexName(indexName: String): Boolean = indexName match {
    case Index.fullInstanceIndex(_) => true
    case _ => false
  }

  private[this] def elasticSearchIndexExists(indexName: String): Boolean = {
    val languageSpecificIndex = Index.esLanguageFromIndexName(indexName, "")
    languageIndexManagementService.indices.exists(_.startsWith(languageSpecificIndex))
  }

}
