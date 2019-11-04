package com.getjenny.starchat.services

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 10/03/17.
  */

import java.io._

import com.getjenny.starchat.entities._
import com.getjenny.starchat.services.SystemIndexManagementService.getClass
import com.getjenny.starchat.services.esclient.{IndexLanguageCrud, IndexManagementElasticClient}
import com.getjenny.starchat.utils.Index
import org.elasticsearch.action.admin.indices.delete.DeleteIndexRequest
import org.elasticsearch.action.admin.indices.open.{OpenIndexRequest, OpenIndexResponse}
import org.elasticsearch.action.admin.indices.settings.put.UpdateSettingsRequest
import org.elasticsearch.action.support.master.AcknowledgedResponse
import org.elasticsearch.client.indices._
import org.elasticsearch.client.{RequestOptions, RestHighLevelClient}
import org.elasticsearch.common.settings._
import org.elasticsearch.common.xcontent.XContentType
import org.elasticsearch.index.query.QueryBuilders
import scalaz.Scalaz._

import scala.collection.immutable.List
import scala.concurrent.Future
import scala.io.Source
import scala.util.{Failure, Success, Try}

case class IndexManagementServiceException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)

case class LangResourceException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)

/**
  * Implements functions, eventually used by IndexManagementResource, for ES index management
  */
object LangaugeIndexManagementService extends AbstractDataService {
  override val elasticClient: IndexManagementElasticClient.type = IndexManagementElasticClient
  private[this] val dtReloadService = InstanceRegistryService

  private[this] def analyzerFiles(language: String): JsonMappingAnalyzersIndexFiles =
    JsonMappingAnalyzersIndexFiles(path = "/index_management/json_index_spec/" + language + "/analyzer.json",
      updatePath = "/index_management/json_index_spec/" + language + "/update/analyzer.json",
      indexSuffix = "",
      numberOfShards = 0,
      numberOfReplicas = 0
    )

  private[this] val schemaFiles: List[JsonMappingAnalyzersIndexFiles] = List[JsonMappingAnalyzersIndexFiles](
    JsonMappingAnalyzersIndexFiles(path = "/index_management/json_index_spec/general/state.json",
      updatePath = "/index_management/json_index_spec/general/update/state.json",
      indexSuffix = elasticClient.dtIndexSuffix,
      numberOfShards = elasticClient.stateNumberOfShards,
      numberOfReplicas = elasticClient.stateNumberOfReplicas
    ),
    JsonMappingAnalyzersIndexFiles(path = "/index_management/json_index_spec/general/question_answer.json",
      updatePath = "/index_management/json_index_spec/general/update/question_answer.json",
      indexSuffix = elasticClient.convLogsIndexSuffix,
      numberOfShards = elasticClient.logsDataNumberOfShards,
      numberOfReplicas = elasticClient.logsDataNumberOfReplicas
    ),
    JsonMappingAnalyzersIndexFiles(path = "/index_management/json_index_spec/general/question_answer.json",
      updatePath = "/index_management/json_index_spec/general/update/question_answer.json",
      indexSuffix = elasticClient.priorDataIndexSuffix,
      numberOfShards = elasticClient.priorDataNumberOfShards,
      numberOfReplicas = elasticClient.priorDataNumberOfReplicas
    ),
    JsonMappingAnalyzersIndexFiles(path = "/index_management/json_index_spec/general/question_answer.json",
      updatePath = "/index_management/json_index_spec/general/update/question_answer.json",
      indexSuffix = elasticClient.kbIndexSuffix,
      numberOfShards = elasticClient.kbNumberOfShards,
      numberOfReplicas = elasticClient.kbNumberOfReplicas
    ),
    JsonMappingAnalyzersIndexFiles(path = "/index_management/json_index_spec/general/term.json",
      updatePath = "/index_management/json_index_spec/general/update/term.json",
      indexSuffix = elasticClient.termIndexSuffix,
      numberOfShards = elasticClient.termNumberOfShards,
      numberOfReplicas = elasticClient.termNumberOfReplicas
    )
  )

  private[this] object LangResourceType extends Enumeration {
    val STOPWORD,
    STEMMER_OVERRIDE,
    UNSPECIFIED = LangResourceType.Value

    def value(v: String): LangResourceType.Value = values.find(_.toString === v).getOrElse(UNSPECIFIED)
  }

  private[this] val accessoryFilePathTpl = "/index_management/json_index_spec/%1$s/%2$s"
  private[this] val langSpecificDataFiles: List[(String, LangResourceType.Value)] =
    List[(String, LangResourceType.Value)](
      ("stopwords.json", LangResourceType.STOPWORD),
      ("stemmer_override.json", LangResourceType.STEMMER_OVERRIDE)
    )

  private[this] def loadLangSpecificResources(indexName: String, indexSuffix: String,
                                              language: String, openCloseIndices: Boolean = false): Unit = {
    val client: RestHighLevelClient = elasticClient.httpClient
    val resourcesJson = langSpecificDataFiles.map { case (file, resType) =>
      val resPath: String = accessoryFilePathTpl.format(language, file)
      val resFileJsonIs: Option[InputStream] = Option {
        getClass.getResourceAsStream(resPath)
      }
      resFileJsonIs match {
        case Some(stream) => (Source.fromInputStream(stream, "utf-8").mkString, resType)
        case _ => ("", resType)
      }
    }.filter { case (json, _) => json != "" }

    resourcesJson.foreach { case (resJson, resType) =>
      resType match {
        case LangResourceType.STOPWORD | LangResourceType.STEMMER_OVERRIDE =>
          if (openCloseIndices) openClose(indexName, Some(indexSuffix), "close")
          val updateIndexSettingsReq = new UpdateSettingsRequest().indices(indexName)
            .settings(Settings.builder().loadFromSource(resJson, XContentType.JSON))
          val updateIndexSettingsRes: AcknowledgedResponse = client.indices
            .putSettings(updateIndexSettingsReq, RequestOptions.DEFAULT)
          if (!updateIndexSettingsRes.isAcknowledged) {
            val message = "Failed to apply index settings (" + resType + ") for index: " + indexName
            throw LangResourceException(message)
          }
          if (openCloseIndices) openClose(indexName, Some(indexSuffix), "open")
        case _ =>
          val message = "Bad ResourceType(" + resType + ") for index: " + indexName
          throw LangResourceException(message)
      }
    }
  }

  private[this] def findAllLanguages: List[String] = {
    new File(getClass.getResource("/index_management/json_index_spec/").getFile)
      .listFiles
      .filter(_.isDirectory)
      .map(_.getName)
      .filterNot(x => x.equals("general") || x.equals("system"))
      .toList
  }

  def create(languages: List[String]): List[IndexManagementResponse] = {
    val client: RestHighLevelClient = elasticClient.httpClient

    val languagesToCreate = if(languages.isEmpty) findAllLanguages else languages

    languagesToCreate.map { language =>
      val indexName = s"index_$language"
      val analyzerJsonPath: String = analyzerFiles(language).path
      val analyzerJsonIs: Option[InputStream] = Option {
        getClass.getResourceAsStream(analyzerJsonPath)
      }
      val analyzerJson: String = analyzerJsonIs match {
        case Some(stream) => Source.fromInputStream(stream, "utf-8").mkString
        case _ =>
          val message = "Check the file: (" + analyzerJsonPath + ")"
          throw new FileNotFoundException(message)
      }

      val operationsMessage: List[(String, Boolean)] = schemaFiles.map(item => {
        val jsonInStream: Option[InputStream] = Option {
          getClass.getResourceAsStream(item.path)
        }

        val schemaJson: String = jsonInStream match {
          case Some(stream) => Source.fromInputStream(stream, "utf-8").mkString
          case _ =>
            val message = "Check the file: (" + item.path + ")"
            throw new FileNotFoundException(message)
        }

        val fullIndexName = s"index_$language.${item.indexSuffix}"

        val createIndexReq = new CreateIndexRequest(fullIndexName).settings(
          Settings.builder().loadFromSource(analyzerJson, XContentType.JSON)
            .put("index.number_of_shards", item.numberOfShards)
            .put("index.number_of_replicas", item.numberOfReplicas)
        ).source(schemaJson, XContentType.JSON)

        val createIndexRes: CreateIndexResponse = client.indices.create(createIndexReq, RequestOptions.DEFAULT)

        loadLangSpecificResources(indexName = indexName, indexSuffix = item.indexSuffix,
          language = language, openCloseIndices = true)

        (item.indexSuffix + "(" + fullIndexName + ", " + createIndexRes.isAcknowledged + ")",
          createIndexRes.isAcknowledged)
      })

      val message = "IndexCreation: " + operationsMessage.map { case (msg, _) => msg }.mkString(" ")

      IndexManagementResponse(message = message, check = operationsMessage.forall { case (_, ck) => ck })
    }
  }

  def remove(indexName: String,
             indexSuffix: Option[String] = None): IndexManagementResponse = {
    val client: RestHighLevelClient = elasticClient.httpClient

    if (!elasticClient.enableDeleteIndex) {
      val message: String = "operation is not allowed, contact system administrator"
      throw IndexManagementServiceException(message)
    }

    val operationsMessage: List[(String, Boolean)] = schemaFiles.filter(item => {
      indexSuffix match {
        case Some(t) => t === item.indexSuffix
        case _ => true
      }
    }).map(item => {

      val fullIndexName = Index.indexName(indexName, item.indexSuffix)

      val deleteIndexReq = new DeleteIndexRequest(fullIndexName)

      val deleteIndexRes: AcknowledgedResponse = client.indices.delete(deleteIndexReq, RequestOptions.DEFAULT)

      (item.indexSuffix + "(" + fullIndexName + ", " + deleteIndexRes.isAcknowledged + ")",
        deleteIndexRes.isAcknowledged)
    })

    val message = "IndexDeletion: " + operationsMessage.map { case (msg, _) => msg }.mkString(" ")
    IndexManagementResponse(message = message, check = operationsMessage.forall { case (_, ck) => ck })
  }

  def check(indexName: String,
            indexSuffix: Option[String] = None): IndexManagementResponse = {
    val client: RestHighLevelClient = elasticClient.httpClient

    val operations: List[(String, Boolean)] = schemaFiles.filter { item =>
      indexSuffix match {
        case Some(t) => t === item.indexSuffix
        case _ => true
      }
    }.map { item =>
      val fullIndexName = Index.indexName(indexName, item.indexSuffix)

      val getMappingsReq: GetMappingsRequest = new GetMappingsRequest()
        .indices(fullIndexName)

      Try(client.indices.getMapping(getMappingsReq, RequestOptions.DEFAULT)) match {
        case Success(mappingsRes) =>
          val check = mappingsRes.mappings.containsKey(fullIndexName)
          (item.indexSuffix + "(" + fullIndexName + ", " + check + ")", check)
        case Failure(exception) =>
          (item.indexSuffix + "(" + fullIndexName + ", false)", false)
      }
    }

    val (messages, checks) = operations.unzip
    IndexManagementResponse(message = "IndexCheck: " + messages.mkString(" "),
      check = operations.forall { case (_, ck) => ck })
  }

  def openClose(indexName: String, indexSuffix: Option[String] = None,
                operation: String): List[OpenCloseIndex] = {
    val client: RestHighLevelClient = elasticClient.httpClient
    schemaFiles.filter(item => {
      indexSuffix match {
        case Some(t) => t === item.indexSuffix
        case _ => true
      }
    }).map(item => {
      val fullIndexName = Index.indexName(indexName, item.indexSuffix)
      operation match {
        case "close" =>
          val closeIndexReq = new CloseIndexRequest(fullIndexName)
          val closeIndexRes: AcknowledgedResponse = client.indices.close(closeIndexReq, RequestOptions.DEFAULT)
          OpenCloseIndex(indexName = indexName, indexSuffix = item.indexSuffix, fullIndexName = fullIndexName,
            operation = operation, acknowledgement = closeIndexRes.isAcknowledged)
        case "open" =>
          val openIndexReq = new OpenIndexRequest().indices(fullIndexName)
          val openIndexRes: OpenIndexResponse = client.indices.open(openIndexReq, RequestOptions.DEFAULT)
          OpenCloseIndex(indexName = indexName, indexSuffix = item.indexSuffix, fullIndexName = fullIndexName,
            operation = operation, acknowledgement = openIndexRes.isAcknowledged)
        case _ => throw IndexManagementServiceException("operation not supported on index: " + operation)
      }
    })
  }

  def updateSettings(indexName: String,
                     indexSuffix: Option[String] = None): IndexManagementResponse = {
    val client: RestHighLevelClient = elasticClient.httpClient

    val (_, language, _) = Index.patternsFromIndexName(indexName: String)

    val analyzerJsonPath: String = analyzerFiles(language).updatePath
    val analyzerJsonIs: Option[InputStream] = Option {
      getClass.getResourceAsStream(analyzerJsonPath)
    }
    val analyzerJson: String = analyzerJsonIs match {
      case Some(stream) => Source.fromInputStream(stream, "utf-8").mkString
      case _ =>
        val message = "file not found: (" + analyzerJsonPath + ")"
        throw new FileNotFoundException(message)
    }

    val operationsMessage: List[(String, Boolean)] = schemaFiles.filter(item => {
      indexSuffix match {
        case Some(t) => t === item.indexSuffix
        case _ => true
      }
    }).map(item => {
      val fullIndexName = Index.indexName(indexName, item.indexSuffix)

      val updateIndexSettingsReq = new UpdateSettingsRequest().indices(fullIndexName)
        .settings(Settings.builder().loadFromSource(analyzerJson, XContentType.JSON))

      val updateIndexSettingsRes: AcknowledgedResponse = client.indices
        .putSettings(updateIndexSettingsReq, RequestOptions.DEFAULT)

      loadLangSpecificResources(indexName, item.indexSuffix, language)

      (item.indexSuffix + "(" + fullIndexName + ", " + updateIndexSettingsRes.isAcknowledged + ")",
        updateIndexSettingsRes.isAcknowledged)
    })

    val message = "IndexSettingsUpdate: " + operationsMessage.map { case (msg, _) => msg }.mkString(" ")
    IndexManagementResponse(message = message, check = operationsMessage.forall { case (_, ck) => ck })
  }

  def updateMappings(indexName: String,
                     indexSuffix: Option[String] = None): IndexManagementResponse = {
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
          val message = "Check the file: (" + item.updatePath + ")"
          throw new FileNotFoundException(message)
      }

      val putMappingReq = new PutMappingRequest(indexName)
        .source(schemaJson, XContentType.JSON)

      val putMappingRes: AcknowledgedResponse = client.indices
        .putMapping(putMappingReq, RequestOptions.DEFAULT)

      (item.indexSuffix + "(" + indexName + ", " + putMappingRes.isAcknowledged + ")",
        putMappingRes.isAcknowledged)
    })

    val message = "IndexUpdateMappings: " + operationsMessage.mkString(" ")
    IndexManagementResponse(message = message, check = operationsMessage.forall { case (_, ck) => ck })
  }

  def refresh(indexName: String,
              indexSuffix: Option[String] = None): RefreshIndexResults = {
    val operationsResults: List[RefreshIndexResult] = schemaFiles.filter(item => {
      indexSuffix match {
        case Some(t) => t === item.indexSuffix
        case _ => true
      }
    }).map(item => {
      val fullIndexName = Index.indexName(indexName, item.indexSuffix)
      val refreshIndexRes: RefreshIndexResult = elasticClient.refresh(fullIndexName)
      if (refreshIndexRes.failedShardsN > 0) {
        val indexRefreshMessage = item.indexSuffix + "(" + fullIndexName + ", " + refreshIndexRes.failedShardsN + ")"
        throw new Exception(indexRefreshMessage)
      }

      refreshIndexRes
    })

    RefreshIndexResults(results = operationsResults)
  }
}


