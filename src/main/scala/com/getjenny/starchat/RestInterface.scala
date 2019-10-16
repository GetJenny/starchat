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
  with UserResource with NodeDtLoadingStatusResource with ClusterNodesResource {

  implicit def executionContext: ExecutionContext

  lazy val decisionTableService: DecisionTableService.type = DecisionTableService
  lazy val indexManagementService: IndexManagementService.type = IndexManagementService
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
  lazy val cronCleanDTService: CronCleanDTService.type = CronCleanDTService
  lazy val cronCleanDeadNodesService: CronCleanDeadNodesService.type = CronCleanDeadNodesService
  lazy val cronNodeAliveSignalService: CronNodeAliveSignalService.type = CronNodeAliveSignalService
  lazy val cronCleanDtLoadingRecordsService: CronCleanDtLoadingRecordsService.type = CronCleanDtLoadingRecordsService
  lazy val cronInitializeSystemIndicesService: CronInitializeSystemIndicesService.type = CronInitializeSystemIndicesService
  lazy val systemService: DtReloadService.type = DtReloadService
  lazy val knowledgeBaseService: KnowledgeBaseService.type = KnowledgeBaseService
  lazy val conversationLogsService: ConversationLogsService.type = ConversationLogsService
  lazy val priorDataService: PriorDataService.type = PriorDataService

  val routes: Route = rootAPIsRoutes ~
    LoggingEntities.logRequestAndResultReduced(kbQuestionAnswerRoutes) ~
    LoggingEntities.logRequestAndResultReduced(kbQuestionAnswerStreamRoutes) ~
    LoggingEntities.logRequestAndResult(kbQuestionAnswerSearchRoutes) ~
    LoggingEntities.logRequestAndResult(kbTotalTermsRoutes) ~
    LoggingEntities.logRequestAndResult(kbDictSizeRoutes) ~
    LoggingEntities.logRequestAndResult(kbTermsCountRoutes) ~
    LoggingEntities.logRequestAndResult(kbUpdateTermsRoutes) ~
    LoggingEntities.logRequestAndResult(kbCountersCacheSizeRoutes) ~
    LoggingEntities.logRequestAndResultReduced(kbQuestionAnswerConversationsRoutes) ~
    LoggingEntities.logRequestAndResultReduced(kbQuestionAnswerAnalyticsRoutes) ~
    LoggingEntities.logRequestAndResultReduced(pdQuestionAnswerRoutes) ~
    LoggingEntities.logRequestAndResultReduced(pdQuestionAnswerStreamRoutes) ~
    LoggingEntities.logRequestAndResult(pdQuestionAnswerSearchRoutes) ~
    LoggingEntities.logRequestAndResult(pdTotalTermsRoutes) ~
    LoggingEntities.logRequestAndResult(pdDictSizeRoutes) ~
    LoggingEntities.logRequestAndResult(pdTermsCountRoutes) ~
    LoggingEntities.logRequestAndResult(pdUpdateTermsRoutes) ~
    LoggingEntities.logRequestAndResult(pdCountersCacheSizeRoutes) ~
    LoggingEntities.logRequestAndResultReduced(pdQuestionAnswerConversationsRoutes) ~
    LoggingEntities.logRequestAndResultReduced(pdQuestionAnswerAnalyticsRoutes) ~
    LoggingEntities.logRequestAndResultReduced(clQuestionAnswerRoutes) ~
    LoggingEntities.logRequestAndResultReduced(clQuestionAnswerStreamRoutes) ~
    LoggingEntities.logRequestAndResult(clQuestionAnswerSearchRoutes) ~
    LoggingEntities.logRequestAndResult(clTotalTermsRoutes) ~
    LoggingEntities.logRequestAndResult(clDictSizeRoutes) ~
    LoggingEntities.logRequestAndResult(clTermsCountRoutes) ~
    LoggingEntities.logRequestAndResult(clUpdateTermsRoutes)~
    LoggingEntities.logRequestAndResult(clCountersCacheSizeRoutes) ~
    LoggingEntities.logRequestAndResultReduced(clQuestionAnswerConversationsRoutes) ~
    LoggingEntities.logRequestAndResultReduced(clQuestionAnswerAnalyticsRoutes) ~
    LoggingEntities.logRequestAndResult(termsExtractionRoutes) ~
    LoggingEntities.logRequestAndResult(synExtractionRoutes) ~
    LoggingEntities.logRequestAndResult(freqExtractionRoutes) ~
    LoggingEntities.logRequestAndResult(decisionTableRoutes) ~
    LoggingEntities.logRequestAndResult(decisionTableRoutesAllRoutes) ~
    LoggingEntities.logRequestAndResult(decisionTableUploadFilesRoutes) ~
    LoggingEntities.logRequestAndResult(decisionTableSearchRoutes) ~
    LoggingEntities.logRequestAndResult(decisionTableAsyncReloadRoutes) ~
    LoggingEntities.logRequestAndResult(decisionTableResponseRequestRoutes) ~
    LoggingEntities.logRequestAndResult(decisionTableAnalyzerRoutes) ~
    LoggingEntities.logRequestAndResult(postIndexManagementCreateRoutes) ~
    LoggingEntities.logRequestAndResult(postIndexManagementRefreshRoutes) ~
    LoggingEntities.logRequestAndResult(putIndexManagementRoutes) ~
    LoggingEntities.logRequestAndResult(indexManagementRoutes) ~
    LoggingEntities.logRequestAndResult(postIndexManagementOpenCloseRoutes) ~
    LoggingEntities.logRequestAndResult(systemIndexManagementRoutes) ~
    LoggingEntities.logRequestAndResult(systemGetIndexesRoutes) ~
    LoggingEntities.logRequestAndResult(languageGuesserRoutes) ~
    LoggingEntities.logRequestAndResultReduced(termRoutes) ~
    LoggingEntities.logRequestAndResultReduced(termStreamRoutes) ~
    LoggingEntities.logRequestAndResult(esTokenizersRoutes) ~
    LoggingEntities.logRequestAndResult(analyzersPlaygroundRoutes) ~
    LoggingEntities.logRequestAndResult(spellcheckRoutes) ~
    LoggingEntities.logRequestAndResult(postUserRoutes) ~
    LoggingEntities.logRequestAndResult(putUserRoutes) ~
    LoggingEntities.logRequestAndResult(getUserRoutes) ~
    LoggingEntities.logRequestAndResult(delUserRoutes) ~
    LoggingEntities.logRequestAndResult(genUserRoutes) ~
    LoggingEntities.logRequestAndResultReduced(clusterNodesRoutes) ~
    LoggingEntities.logRequestAndResultReduced(nodeDtLoadingStatusRoutes)
}
