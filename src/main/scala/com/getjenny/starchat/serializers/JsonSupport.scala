package com.getjenny.starchat.serializers

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 27/06/16.
  */

import akka.http.scaladsl.common.{EntityStreamingSupport, JsonEntityStreamingSupport}
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.unmarshalling.Unmarshaller
import akka.stream.scaladsl.Flow
import akka.util.ByteString
import com.getjenny.analyzer.expressions.AnalyzersData
import com.getjenny.starchat.entities.{QAAggregatedAnalyticsRequest, _}
import scalaz.Scalaz._
import spray.json._

trait JsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  private[this] val start: ByteString = ByteString.empty
  private[this] val sep: ByteString = ByteString("\n")
  private[this] val end: ByteString = ByteString.empty

  implicit val jsonStreamingSupport: JsonEntityStreamingSupport =
    EntityStreamingSupport.json(1024 * 1024) // max object size 1MB
      .withFramingRendererFlow(Flow[ByteString].intersperse(start, sep, end).asJava)
      .withParallelMarshalling(parallelism = 8, unordered = true)

  implicit val searchAlgorithmUnmarshalling:
    Unmarshaller[String, SearchAlgorithm.Value] =
    Unmarshaller.strict[String, SearchAlgorithm.Value] { enumValue =>
      SearchAlgorithm.value(enumValue)
    }

  implicit object SearchAlgorithmFormat extends JsonFormat[SearchAlgorithm.Value] {
    def write(obj: SearchAlgorithm.Value): JsValue = JsString(obj.toString)
    def read(json: JsValue): SearchAlgorithm.Value = json match {
      case JsString(str) =>
        SearchAlgorithm.values.find(_.toString === str) match {
          case Some(t) => t
          case _ => throw DeserializationException("SearchAlgorithm string is invalid")
        }
      case _ => throw DeserializationException("SearchAlgorithm string expected")
    }
  }

  implicit val qAAggregationsTypesUnmarshalling:
    Unmarshaller[String, QAAggregationsTypes.Value] =
    Unmarshaller.strict[String, QAAggregationsTypes.Value] { enumValue =>
      QAAggregationsTypes.value(enumValue)
    }

  implicit object QAAggregationsTypesFormat extends JsonFormat[QAAggregationsTypes.Value] {
    def write(obj: QAAggregationsTypes.Value): JsValue = JsString(obj.toString)
    def read(json: JsValue): QAAggregationsTypes.Value = json match {
      case JsString(str) =>
        QAAggregationsTypes.values.find(_.toString === str) match {
          case Some(t) => t
          case _ => throw DeserializationException("QAAggregationsTypesFormat string is invalid")
        }
      case _ => throw DeserializationException("QAAggregationsTypesFormat string expected")
    }
  }

  implicit val qaAggregatedAnalyticsRequestFormat = jsonFormat6(QAAggregatedAnalyticsRequest)
  implicit val scoreHistogramFormat = jsonFormat2(ScoreHistogramItem)
  implicit val conversationsHistogramFormat = jsonFormat3(CountOverTimeHistogramItem)
  implicit val labelCountHistogramItem = jsonFormat2(LabelCountHistogramItem)
  implicit val scoresOverTimeFormat = jsonFormat4(AvgScoresHistogramItem)
  implicit val qaAggregatedAnalyticsFormat = jsonFormat10(QAAggregatedAnalytics)

  ////////////////////////////
  implicit val doctypesUnmarshalling:
    Unmarshaller[String, Doctypes.Value] =
    Unmarshaller.strict[String, Doctypes.Value] { enumValue =>
      Doctypes.value(enumValue)
    }

  implicit object DoctypesFormat extends JsonFormat[Doctypes.Value] {
    def write(obj: Doctypes.Value): JsValue = JsString(obj.toString)
    def read(json: JsValue): Doctypes.Value = json match {
      case JsString(str) =>
        Doctypes.values.find(_.toString === str) match {
          case Some(t) => t
          case _ => throw DeserializationException("Doctypes string is invalid")
        }
      case _ => throw DeserializationException("Doctypes string expected")
    }
  }

  implicit val agentUnmarshalling:
    Unmarshaller[String, Agent.Value] =
    Unmarshaller.strict[String, Agent.Value] { enumValue =>
      Agent.value(enumValue)
    }

  implicit object AgentFormat extends JsonFormat[Agent.Value] {
    def write(obj: Agent.Value): JsValue = JsString(obj.toString)
    def read(json: JsValue): Agent.Value = json match {
      case JsString(str) =>
        Agent.values.find(_.toString === str) match {
          case Some(t) => t
          case _ => throw DeserializationException("Agent string is invalid")
        }
      case _ => throw DeserializationException("Agent string expected")
    }
  }


  implicit val escalatedUnmarshalling:
    Unmarshaller[String, Escalated.Value] =
    Unmarshaller.strict[String, Escalated.Value] { enumValue =>
      Escalated.value(enumValue)
    }

  implicit object EscalatedFormat extends JsonFormat[Escalated.Value] {
    def write(obj: Escalated.Value): JsValue = JsString(obj.toString)
    def read(json: JsValue): Escalated.Value = json match {
      case JsString(str) =>
        Escalated.values.find(_.toString === str) match {
          case Some(t) => t
          case _ => throw DeserializationException("Escalated string is invalid")
        }
      case _ => throw DeserializationException("Escalated string expected")
    }
  }

  implicit val answeredUnmarshalling:
    Unmarshaller[String, Answered.Value] =
    Unmarshaller.strict[String, Answered.Value] { enumValue =>
      Answered.value(enumValue)
    }

  implicit object AnsweredFormat extends JsonFormat[Answered.Value] {
    def write(obj: Answered.Value): JsValue = JsString(obj.toString)
    def read(json: JsValue): Answered.Value = json match {
      case JsString(str) =>
        Answered.values.find(_.toString === str) match {
          case Some(t) => t
          case _ => throw DeserializationException("Answered string is invalid")
        }
      case _ => throw DeserializationException("Answered string expected")
    }
  }

  implicit val triggeredUnmarshalling:
    Unmarshaller[String, Triggered.Value] =
    Unmarshaller.strict[String, Triggered.Value] { enumValue =>
      Triggered.value(enumValue)
    }

  implicit object TriggeredFormat extends JsonFormat[Triggered.Value] {
    def write(obj: Triggered.Value): JsValue = JsString(obj.toString)
    def read(json: JsValue): Triggered.Value = json match {
      case JsString(str) =>
        Triggered.values.find(_.toString === str) match {
          case Some(t) => t
          case _ => throw DeserializationException("Triggered string is invalid")
        }
      case _ => throw DeserializationException("Triggered string expected")
    }
  }

  implicit val followupUnmarshalling:
    Unmarshaller[String, Followup.Value] =
    Unmarshaller.strict[String, Followup.Value] { enumValue =>
      Followup.value(enumValue)
    }

  implicit object FollowupFormat extends JsonFormat[Followup.Value] {
    def write(obj: Followup.Value): JsValue = JsString(obj.toString)
    def read(json: JsValue): Followup.Value = json match {
      case JsString(str) =>
        Followup.values.find(_.toString === str) match {
          case Some(t) => t
          case _ => throw DeserializationException("Followup string is invalid")
        }
      case _ => throw DeserializationException("Followup string expected")
    }
  }

  implicit val responseMessageDataFormat = jsonFormat2(ReturnMessageData)
  implicit val responseRequestUserInputFormat = jsonFormat2(ResponseRequestInUserInput)
  implicit val responseRequestInputFormat = jsonFormat9(ResponseRequestIn)
  implicit val responseRequestOutputFormat = jsonFormat13(ResponseRequestOut)
  implicit val dtDocumentFormat = jsonFormat13(DTDocument)
  implicit val dtDocumentUpdateFormat = jsonFormat12(DTDocumentUpdate)
  implicit val qaDocumentAnnotationsSearchFormat = jsonFormat20(QADocumentAnnotationsSearch)
  implicit val qaDocumentCoreFormat = jsonFormat8(QADocumentCore)
  implicit val qaDocumentAnnotationsFormat = jsonFormat15(QADocumentAnnotations)
  implicit val qaDocumentFormat = jsonFormat7(QADocument)
  implicit val qaDocumentUpdateFormat = jsonFormat7(QADocumentUpdate)
  implicit val searchQADocumentFormat = jsonFormat2(SearchQADocument)
  implicit val searchDTDocumentFormat = jsonFormat2(SearchDTDocument)
  implicit val searchQAResultsFormat = jsonFormat4(SearchQADocumentsResults)
  implicit val searchDTResultsFormat = jsonFormat3(SearchDTDocumentsResults)
  implicit val qaDocumentSearchFormat = jsonFormat12(QADocumentSearch)
  implicit val dtDocumentSearchFormat = jsonFormat9(DTDocumentSearch)
  implicit val indexDocumentResultFormat = jsonFormat4(IndexDocumentResult)
  implicit val updateDocumentResultFormat = jsonFormat4(UpdateDocumentResult)
  implicit val deleteDocumentResultFormat = jsonFormat4(DeleteDocumentResult)
  implicit val indexDocumentResultListFormat = jsonFormat1(IndexDocumentListResult)
  implicit val updateDocumentResultListFormat = jsonFormat1(UpdateDocumentsResult)
  implicit val deleteDocumentResultListFormat = jsonFormat1(DeleteDocumentsResult)
  implicit val deleteDocumentsResultFormat = jsonFormat2(DeleteDocumentsSummaryResult)
  implicit val listOfDocumentIdFormat = jsonFormat1(ListOfDocumentId)
  implicit val dtAnalyzerItemFormat = jsonFormat4(DTAnalyzerItem)
  implicit val dtAnalyzerMapFormat = jsonFormat1(DTAnalyzerMap)
  implicit val dtAnalyzerLoadFormat = jsonFormat1(DTAnalyzerLoad)
  implicit val indexManagementResponseFormat = jsonFormat2(IndexManagementResponse)
  implicit val languageGuesserRequestInFormat = jsonFormat1(LanguageGuesserRequestIn)
  implicit val languageGuesserRequestOutFormat = jsonFormat4(LanguageGuesserRequestOut)
  implicit val languageGuesserInformationsFormat = jsonFormat1(LanguageGuesserInformations)
  implicit val searchTermFormat = jsonFormat9(SearchTerm)
  implicit val termFormat = jsonFormat9(Term)
  implicit val termIdsRequestFormat = jsonFormat1(DocsIds)
  implicit val termsFormat = jsonFormat1(Terms)
  implicit val termsResultsFormat = jsonFormat3(TermsResults)
  implicit val textTermsFormat = jsonFormat4(TextTerms)
  implicit val failedShardsFormat = jsonFormat4(FailedShard)
  implicit val refreshIndexResultFormat = jsonFormat5(RefreshIndexResult)
  implicit val refreshIndexResultsFormat = jsonFormat1(RefreshIndexResults)
  implicit val analyzerQueryRequestFormat = jsonFormat2(TokenizerQueryRequest)
  implicit val analyzerResponseItemFormat = jsonFormat5(TokenizerResponseItem)
  implicit val analyzerResponseFormat = jsonFormat1(TokenizerResponse)
  implicit val analyzerDataFormat = jsonFormat2(AnalyzersData)
  implicit val analyzerEvaluateRequestFormat = jsonFormat5(AnalyzerEvaluateRequest)
  implicit val analyzerEvaluateResponseFormat = jsonFormat4(AnalyzerEvaluateResponse)
  implicit val spellcheckTokenSuggestionsFormat = jsonFormat3(SpellcheckTokenSuggestions)
  implicit val spellcheckTokenFormat = jsonFormat4(SpellcheckToken)
  implicit val spellcheckTermsResponseFormat = jsonFormat1(SpellcheckTermsResponse)
  implicit val spellcheckTermsRequestFormat = jsonFormat4(SpellcheckTermsRequest)
  implicit val responseRequestOutOperationResultFormat = jsonFormat2(ResponseRequestOutOperationResult)
  implicit val clusterNodeFormat = jsonFormat3(ClusterNode)
  implicit val clusterNodesFormat = jsonFormat2(ClusterNodes)
  implicit val clusterLoadingDtStatusFormat = jsonFormat5(ClusterLoadingDtStatusIndex)
  implicit val nodeDtLoadingStatusFormat = jsonFormat3(NodeDtLoadingStatus)
  implicit val nodeLoadingAllDtStatusFormat = jsonFormat3(NodeLoadingAllDtStatus)

  implicit object PermissionsJsonFormat extends JsonFormat[Permissions.Value] {
    def write(obj: Permissions.Value): JsValue = JsString(obj.toString)
    def read(json: JsValue): Permissions.Value = json match {
      case JsString(str) =>
        Permissions.values.find(_.toString === str) match {
          case Some(t) => t
          case _ => throw DeserializationException("Permission string is invalid")
        }
      case _ => throw DeserializationException("Permission string expected")
    }
  }

  implicit val userFormat = jsonFormat4(User)
  implicit val userUpdateFormat = jsonFormat4(UserUpdate)
  implicit val userDelete = jsonFormat1(UserId)
  implicit val dtReloadTimestamp = jsonFormat2(DtReloadTimestamp)
  implicit val openCloseIndexFormat = jsonFormat5(OpenCloseIndex)

  implicit val commonOrSpecificSearchUnmarshalling:
    Unmarshaller[String, CommonOrSpecificSearch.Value] =
    Unmarshaller.strict[String, CommonOrSpecificSearch.Value] { enumValue =>
      CommonOrSpecificSearch.value(enumValue)
    }

  implicit object CommonOrSpecificSearchFormat extends JsonFormat[CommonOrSpecificSearch.Value] {
    def write(obj: CommonOrSpecificSearch.Value): JsValue = JsString(obj.toString)
    def read(json: JsValue): CommonOrSpecificSearch.Value = json match {
      case JsString(str) =>
        CommonOrSpecificSearch.values.find(_.toString === str) match {
          case Some(t) => t
          case _ => throw DeserializationException("CommonOrSpecificSearch string is invalid")
        }
      case _ => throw DeserializationException("CommonOrSpecificSearch string expected")
    }
  }

  implicit val observedDataSourcesUnmarshalling:
    Unmarshaller[String, ObservedDataSources.Value] = Unmarshaller.strict[String, ObservedDataSources.Value] { enumValue =>
    ObservedDataSources.value(enumValue)
  }

  implicit object ObservedSearchDestFormat extends JsonFormat[ObservedDataSources.Value] {
    def write(obj: ObservedDataSources.Value): JsValue = JsString(obj.toString)
    def read(json: JsValue): ObservedDataSources.Value = json match {
      case JsString(str) =>
        ObservedDataSources.values.find(_.toString === str) match {
          case Some(t) => t
          case _ => throw DeserializationException("ObservedSearchDests string is invalid")
        }
      case _ => throw DeserializationException("ObservedSearchDests string expected")
    }
  }

  implicit val termCountFieldsUnmarshalling:
    Unmarshaller[String, TermCountFields.Value] =
    Unmarshaller.strict[String, TermCountFields.Value] { enumValue =>
      TermCountFields.value(enumValue)
    }

  implicit object TermCountFieldsFormat extends JsonFormat[TermCountFields.Value] {
    def write(obj: TermCountFields.Value): JsValue = JsString(obj.toString)
    def read(json: JsValue): TermCountFields.Value = json match {
      case JsString(str) =>
        TermCountFields.values.find(_.toString === str) match {
          case Some(t) => t
          case _ => throw DeserializationException("TermCountFields string is invalid")
        }
      case _ => throw DeserializationException("TermCountFields string expected")
    }
  }

  implicit val synonymExtractionDistanceFunctionUnmarshalling:
    Unmarshaller[String, SynonymExtractionDistanceFunction.Value] =
    Unmarshaller.strict[String, SynonymExtractionDistanceFunction.Value] { enumValue =>
      SynonymExtractionDistanceFunction.value(enumValue)
    }

  implicit object SynonymExtractionDistanceFunctionFormat extends JsonFormat[SynonymExtractionDistanceFunction.Value] {
    def write(obj: SynonymExtractionDistanceFunction.Value): JsValue = JsString(obj.toString)
    def read(json: JsValue): SynonymExtractionDistanceFunction.Value = json match {
      case JsString(str) =>
        SynonymExtractionDistanceFunction.values.find(_.toString === str) match {
          case Some(t) => t
          case _ => throw DeserializationException("SynonymExtractionDistanceFunction string is invalid")
        }
      case _ => throw DeserializationException("SynonymExtractionDistanceFunction string expected")
    }
  }

  implicit val termCountFormat = jsonFormat2(TermCount)
  implicit val totalTermsFormat = jsonFormat3(TotalTerms)
  implicit val dictSizeFormat = jsonFormat4(DictSize)
  implicit val termsExtractionRequestFormat = jsonFormat15(TermsExtractionRequest)
  implicit val synExtractionRequestFormat = jsonFormat19(SynExtractionRequest)
  implicit val synonymItemFormat = jsonFormat4(SynonymItem)
  implicit val synonymExtractionItemFormat = jsonFormat4(SynonymExtractionItem)
  implicit val tokenFrequencyItemFormat = jsonFormat3(TokenFrequencyItem)
  implicit val tokenFrequencyFormat = jsonFormat3(TokenFrequency)
  implicit val termsDistanceResFormat = jsonFormat6(TermsDistanceRes)
  implicit val updateQATermsRequestFormat = jsonFormat13(UpdateQATermsRequest)
  implicit val countersCacheParametersFormat = jsonFormat4(CountersCacheParameters)
  implicit val countersCacheSizeFormat = jsonFormat3(CountersCacheSize)
  implicit val conversationFormat = jsonFormat2(Conversation)
  implicit val conversationsFormat = jsonFormat2(Conversations)
  implicit val updateQAByQueryReq = jsonFormat2(UpdateQAByQueryReq)
}
