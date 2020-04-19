package com.getjenny.starchat

/**
 * Created by Angelo Leto <angelo@getjenny.com> on 27/06/16.
 */

import akka.http.scaladsl.server.Route
import com.getjenny.starchat.resources._
import com.getjenny.starchat.services._

import scala.concurrent.ExecutionContext

trait RestInterface extends RootAPIResource
  with SystemIndexManagementResource with IndexManagementResource
  with LanguageGuesserResource
  with TermResource with TokenizersResource
  with DecisionTableResource with AnalyzersPlaygroundResource with TermsExtractionResource
  with SpellcheckResource
  with KnowledgeBaseResource with ConversationLogsResource with PriorDataResource
  with UserResource with NodeDtLoadingStatusResource with ClusterNodesResource with LanguageIndexManagementResource {

  implicit def executionContext: ExecutionContext

  lazy val decisionTableService: DecisionTableService.type = DecisionTableService
  lazy val indexManagementService: LangaugeIndexManagementService.type = LangaugeIndexManagementService
  lazy val systemIndexManagementService: SystemIndexManagementService.type = SystemIndexManagementService
  lazy val languageGuesserService: LanguageGuesserService.type = LanguageGuesserService
  lazy val termService: TermService.type = TermService
  lazy val responseService: ResponseService.type = ResponseService
  lazy val analyzerService: AnalyzerService.type = AnalyzerService
  lazy val userService: UserService.type = UserService
  lazy val spellcheckService: SpellcheckService.type = SpellcheckService
  lazy val clusterNodesServices: ClusterNodesService.type = ClusterNodesService
  lazy val nodeDtLoadingStatusService: NodeDtLoadingStatusService.type = NodeDtLoadingStatusService
  lazy val cronReloadDTService: CronReloadDTService.type = CronReloadDTService
  lazy val cronCleanDTService: CronCleanInMemoryDTService.type = CronCleanInMemoryDTService
  lazy val cronCleanDeadNodesService: CronCleanDeadNodesService.type = CronCleanDeadNodesService
  lazy val cronNodeAliveSignalService: CronNodeAliveSignalService.type = CronNodeAliveSignalService
  lazy val cronCleanDtLoadingRecordsService: CronCleanDtLoadingRecordsService.type = CronCleanDtLoadingRecordsService
  lazy val cronDeleteInstanceService: CronDeleteInstanceService.type = CronDeleteInstanceService
  lazy val cronInitializeSystemIndicesService: CronInitializeSystemIndicesService.type = CronInitializeSystemIndicesService
  lazy val systemService: InstanceRegistryService.type = InstanceRegistryService
  lazy val knowledgeBaseService: KnowledgeBaseService.type = KnowledgeBaseService
  lazy val conversationLogsService: ConversationLogsService.type = ConversationLogsService
  lazy val priorDataService: PriorDataService.type = PriorDataService

  val routes: Route = rootAPIsRoutes ~
    LoggingEntities.logRequestAndResult(analyzersPlaygroundRoutes) ~
    LoggingEntities.logRequestAndResult(clCountersCacheSizeRoutes) ~
    LoggingEntities.logRequestAndResult(clDictSizeRoutes) ~
    LoggingEntities.logRequestAndResult(clQuestionAnswerSearchRoutes) ~
    LoggingEntities.logRequestAndResult(clTermsCountRoutes) ~
    LoggingEntities.logRequestAndResult(clTotalTermsRoutes) ~
    LoggingEntities.logRequestAndResult(clUpdateTermsRoutes)~
    LoggingEntities.logRequestAndResult(decisionTableAnalyzerRoutes) ~
    LoggingEntities.logRequestAndResult(decisionTableAsyncReloadRoutes) ~
    LoggingEntities.logRequestAndResult(decisionTableCacheLoad) ~
    LoggingEntities.logRequestAndResult(decisionTableCloneIndexRoutes) ~
    LoggingEntities.logRequestAndResult(decisionTableResponseRequestRoutes) ~
    LoggingEntities.logRequestAndResult(decisionTableRoutes) ~
    LoggingEntities.logRequestAndResult(decisionTableRoutesAllRoutes) ~
    LoggingEntities.logRequestAndResult(decisionTableRoutesBulkDeleteRoutes) ~
    LoggingEntities.logRequestAndResult(decisionTableSearchRoutes) ~
    LoggingEntities.logRequestAndResult(decisionTableUploadFilesRoutes) ~
    LoggingEntities.logRequestAndResult(decisionTableAutoCompleteRequestRoutes) ~
    LoggingEntities.logRequestAndResult(delUserRoutes) ~
    LoggingEntities.logRequestAndResult(esTokenizersRoutes) ~
    LoggingEntities.logRequestAndResult(freqExtractionRoutes) ~
    LoggingEntities.logRequestAndResult(genUserRoutes) ~
    LoggingEntities.logRequestAndResult(getUserRoutes) ~
    LoggingEntities.logRequestAndResult(kbCountersCacheSizeRoutes) ~
    LoggingEntities.logRequestAndResult(kbDictSizeRoutes) ~
    LoggingEntities.logRequestAndResult(kbQuestionAnswerSearchRoutes) ~
    LoggingEntities.logRequestAndResult(kbTermsCountRoutes) ~
    LoggingEntities.logRequestAndResult(kbTotalTermsRoutes) ~
    LoggingEntities.logRequestAndResult(kbUpdateTermsRoutes) ~
    LoggingEntities.logRequestAndResult(languageGuesserRoutes) ~
    LoggingEntities.logRequestAndResult(pdCountersCacheSizeRoutes) ~
    LoggingEntities.logRequestAndResult(pdDictSizeRoutes) ~
    LoggingEntities.logRequestAndResult(pdQuestionAnswerSearchRoutes) ~
    LoggingEntities.logRequestAndResult(pdTermsCountRoutes) ~
    LoggingEntities.logRequestAndResult(pdTotalTermsRoutes) ~
    LoggingEntities.logRequestAndResult(pdUpdateTermsRoutes) ~
    LoggingEntities.logRequestAndResult(postIndexManagementCreateRoutes) ~
    LoggingEntities.logRequestAndResult(postUserRoutes) ~
    LoggingEntities.logRequestAndResult(putUserRoutes) ~
    LoggingEntities.logRequestAndResult(spellcheckRoutes) ~
    LoggingEntities.logRequestAndResult(synExtractionRoutes) ~
    LoggingEntities.logRequestAndResult(systemGetIndexesRoutes) ~
    LoggingEntities.logRequestAndResult(systemIndexManagementRoutes) ~
    LoggingEntities.logRequestAndResult(termsExtractionRoutes) ~
    LoggingEntities.logRequestAndResultReduced(clQuestionAnswerAnalyticsRoutes) ~
    LoggingEntities.logRequestAndResultReduced(clQuestionAnswerConversationsRoutes) ~
    LoggingEntities.logRequestAndResultReduced(clQuestionAnswerRoutes) ~
    LoggingEntities.logRequestAndResultReduced(clQuestionAnswerStreamRoutes) ~
    LoggingEntities.logRequestAndResultReduced(clAnnotationsRoutes) ~
    LoggingEntities.logRequestAndResultReduced(clusterNodesRoutes) ~
    LoggingEntities.logRequestAndResultReduced(decisionTableBulkCreateRoutes) ~
    LoggingEntities.logRequestAndResultReduced(kbQuestionAnswerAnalyticsRoutes) ~
    LoggingEntities.logRequestAndResultReduced(kbQuestionAnswerConversationsRoutes) ~
    LoggingEntities.logRequestAndResultReduced(kbQuestionAnswerRoutes) ~
    LoggingEntities.logRequestAndResultReduced(kbQuestionAnswerStreamRoutes) ~
    LoggingEntities.logRequestAndResultReduced(kbAnnotationsRoutes) ~
    LoggingEntities.logRequestAndResultReduced(nodeDtLoadingStatusRoutes) ~
    LoggingEntities.logRequestAndResultReduced(pdQuestionAnswerAnalyticsRoutes) ~
    LoggingEntities.logRequestAndResultReduced(pdQuestionAnswerConversationsRoutes) ~
    LoggingEntities.logRequestAndResultReduced(pdQuestionAnswerRoutes) ~
    LoggingEntities.logRequestAndResultReduced(pdQuestionAnswerStreamRoutes) ~
    LoggingEntities.logRequestAndResultReduced(pdAnnotationsRoutes) ~
    LoggingEntities.logRequestAndResultReduced(termRoutes) ~
    LoggingEntities.logRequestAndResultReduced(termStreamRoutes) ~
    LoggingEntities.logRequestAndResultReduced(languageIndexManagementRoutes)

}
