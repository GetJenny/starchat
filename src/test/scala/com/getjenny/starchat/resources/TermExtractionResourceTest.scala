package com.getjenny.starchat.resources

import akka.http.scaladsl.model.StatusCodes
import com.getjenny.starchat.entities._
import com.getjenny.starchat.utils.Index

class TermExtractionResourceTest extends TestEnglishBase {

  "StarChat" should {
    "return an HTTP code 201 when creating a new user" in {
      val user = User(
        id = "test_user",
        password = "3c98bf19cb962ac4cd0227142b3495ab1be46534061919f792254b80c0f3e566f7819cae73bdc616af0ff555f7460ac96d88d56338d659ebd93e2be858ce1cf9",
        salt = "salt",
        permissions = Map[String, Set[Permissions.Value]](
          "index_getjenny_english_0" -> Set(Permissions.read, Permissions.write),
          "index_getjenny_english_common_0" -> Set(Permissions.read, Permissions.write))
      )
      Post(s"/user", user) ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.Created
      }
    }
  }

  it should {
    s"return an HTTP code 201 when populating knowledge base" in {
      val documents = List(QADocument(
        id = "id1",
        conversation = "conv_id_1",
        indexInConversation = 1,
        coreData = Some(QADocumentCore(
          question = Some("term1 term2"),
          answer = Some("term1 term3")
        )),
        annotations = Some(QADocumentAnnotations(
          doctype = Some(Doctypes.NORMAL),
          agent = Some(Agent.STARCHAT)
        ))
      ), QADocument(
        id = "id2",
        conversation = "conv_id_1",
        indexInConversation = 2,
        coreData = Some(QADocumentCore(
          question = Some("term3 term4"),
          answer = Some("term1")
        )),
        annotations = Some(QADocumentAnnotations(
          doctype = Some(Doctypes.NORMAL),
          agent = Some(Agent.HUMAN_REPLY)
        ))
      ))

      for(document <- documents) {
        Post(s"/index_getjenny_english_0/knowledgebase?refresh=1", document) ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.Created
          val response = responseAs[IndexDocumentResult]
        }
      }
    }
  }

  it should {
    "return an HTTP code 201 when populating prior_data" in {
      val document = QADocument(
        id = "id3",
        conversation = "conv_id_1",
        indexInConversation = 4,
        coreData = Some(QADocumentCore(
          question = Some("term6 term5 term1"),
          answer = Some("term1")
        )),
        annotations = Some(QADocumentAnnotations(
          doctype = Some(Doctypes.NORMAL),
          agent = Some(Agent.HUMAN_REPLY)
        ))
      )
      Post(s"/index_getjenny_english_0/prior_data?refresh=1", document) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.Created
        val response = responseAs[IndexDocumentResult]
      }
      Post(s"/index_getjenny_english_common_0/prior_data?refresh=1", document) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.Created
        val response = responseAs[IndexDocumentResult]
      }
    }
  }

  it should {
    "return an HTTP code 200 when extracting term frequencies" in {
      val request = TermsExtractionRequest(
        text = "term1 term2 term3 term4 term5 term1",
        fieldsObserved = Some(TermCountFields.all),
        commonOrSpecificSearchPrior = Some(CommonOrSpecificSearch.IDXSPECIFIC),
        observedDataSource = Some(ObservedDataSources.KNOWLEDGEBASE)
      )
      Post("/index_getjenny_english_0/extraction/frequencies", request) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[TokenFrequency]
        response.tokensFreq shouldEqual List(
          TokenFrequencyItem(
            token = "term1", priorFrequency = 2, observedFrequency = 3
          ), TokenFrequencyItem(
            token = "term2", priorFrequency = 0, observedFrequency = 1
          ), TokenFrequencyItem(
            token = "term3", priorFrequency = 0, observedFrequency = 2
          ), TokenFrequencyItem(
            token = "term4", priorFrequency = 0, observedFrequency = 1
          ), TokenFrequencyItem(
            token = "term5", priorFrequency = 1, observedFrequency = 0
          )
        )
      }
    }
  }

  it should {
    "return an HTTP code 200 when extracting term keywords" in {
      val request = TermsExtractionRequest(text = "term1 term2 term3 term4",
        fieldsObserved = Some(TermCountFields.all),
        commonOrSpecificSearchPrior = Some(CommonOrSpecificSearch.IDXSPECIFIC),
        observedDataSource = Some(ObservedDataSources.KNOWLEDGEBASE),
        minWordsPerSentence = Some(0),
        minSentenceInfoBit = Some(0),
        minKeywordInfo = Some(0)
      )
      Post("/index_getjenny_english_0/extraction/keywords", request) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[Map[String, Double]]
        response shouldNot be (Map.empty)
      }
    }
  }

  it should {
    "return an HTTP code 200 when extracting term synonyms" in {
      val request = TermsExtractionRequest(text = "term1 term2")
      Post("/index_getjenny_english_0/extraction/synonyms", request) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[List[SynonymExtractionItem]]
        response.map(_.token.token) shouldBe List("term1", "term2")
      }
    }
  }
}
