package com.getjenny.starchat.resources

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.headers.BasicHttpCredentials
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import akka.testkit._
import com.getjenny.starchat.entities._
import com.getjenny.starchat.serializers.JsonSupport
import com.getjenny.starchat.utils.Index
import org.scalatest.{Matchers, WordSpec}

import scala.concurrent.duration._

class QAResourceTest extends WordSpec with Matchers with ScalatestRouteTest with JsonSupport {
  implicit def default(implicit system: ActorSystem) = RouteTestTimeout(10.seconds.dilated(system))

  val service = TestFixtures.service
  val routes = service.routes

  val testAdminCredentials = BasicHttpCredentials("admin", "adminp4ssw0rd")
  val testUserCredentials = BasicHttpCredentials("test_user", "p4ssw0rd")

  val qaRoutes = List("conversation_logs", "knowledgebase", "prior_data")
  val documents = List(
        QADocument(
          id = "id1",
          conversation = "conv_id_1",
          indexInConversation = 1,
          annotations = Some(QADocumentAnnotations(
            doctype = Some(Doctypes.NORMAL),
            agent = Some(Agent.HUMAN_REPLY),
          ))
        ),
        QADocument(
          id = "id2",
          conversation = "conv_id_2",
          indexInConversation = 1,
          annotations = Some(QADocumentAnnotations(
            doctype = Some(Doctypes.DECISIONTABLE),
            agent = Some(Agent.HUMAN_REPLY)
          ))
        ),
        QADocument(
          id = "id3",
          conversation = "conv_id_3",
          indexInConversation = 1,
          annotations = Some(QADocumentAnnotations(
            doctype = Some(Doctypes.CANNED),
            agent = Some(Agent.HUMAN_REPLY)
          ))
        ),
        QADocument(
          id = "id4",
          conversation = "conv_id_4",
          indexInConversation = 1,
          annotations = Some(QADocumentAnnotations(
            doctype = Some(Doctypes.HIDDEN),
            agent = Some(Agent.HUMAN_REPLY)
          ))
        ),
        QADocument(
          id = "id5",
          conversation = "conv_id_1",
          indexInConversation = 2,
          annotations = Some(QADocumentAnnotations(
            doctype = Some(Doctypes.HIDDEN),
            agent = Some(Agent.HUMAN_REPLY)
          )),
          coreData = Some(QADocumentCore(
            question = Some("what is the capital of finland"),
            questionScoredTerms = Some(List(
              ("jokeri", 1.2345)
            )),
            answer = Some("Helsinki")
          ))
        ))

  "StarChat" should {
    "return an HTTP code 201 when creating a new system index" in {
      Post(s"/system_index_management/create") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.Created
        val response = responseAs[IndexManagementResponse]
        response.message should fullyMatch regex "IndexCreation: " +
          "(?:[A-Za-z0-9_]+)\\(" + Index.systemIndexMatchRegex + "\\.(?:[A-Za-z0-9_]+), true\\) " +
          "(?:[A-Za-z0-9_]+)\\(" + Index.systemIndexMatchRegex + "\\.(?:[A-Za-z0-9_]+), true\\) " +
          "(?:[A-Za-z0-9_]+)\\(" + Index.systemIndexMatchRegex + "\\.(?:[A-Za-z0-9_]+), true\\) " +
          "(?:[A-Za-z0-9_]+)\\(" + Index.systemIndexMatchRegex + "\\.(?:[A-Za-z0-9_]+), true\\)".r
      }
    }
  }

  it should {
    "return an HTTP code 201 when creating a new user" in {
      val user = User(
        id = "test_user",
        password = "3c98bf19cb962ac4cd0227142b3495ab1be46534061919f792254b80c0f3e566f7819cae73bdc616af0ff555f7460ac96d88d56338d659ebd93e2be858ce1cf9",
        salt = "salt",
        permissions = Map[String, Set[Permissions.Value]]("index_getjenny_english_0" -> Set(Permissions.read, Permissions.write))
      )
      Post(s"/user", user) ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.Created
      }
    }
  }

  it should {
    "return an HTTP code 201 when creating a new index" in {
      Post(s"/index_getjenny_english_0/index_management/create") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.Created
        val response = responseAs[IndexManagementResponse]
        response.message should fullyMatch regex "IndexCreation: " +
          "(?:[A-Za-z0-9_]+)\\(" + Index.indexMatchRegex + "\\.(?:[A-Za-z0-9_]+), true\\) " +
          "(?:[A-Za-z0-9_]+)\\(" + Index.indexMatchRegex + "\\.(?:[A-Za-z0-9_]+), true\\) " +
          "(?:[A-Za-z0-9_]+)\\(" + Index.indexMatchRegex + "\\.(?:[A-Za-z0-9_]+), true\\) " +
          "(?:[A-Za-z0-9_]+)\\(" + Index.indexMatchRegex + "\\.(?:[A-Za-z0-9_]+), true\\) " +
          "(?:[A-Za-z0-9_]+)\\(" + Index.indexMatchRegex + "\\.(?:[A-Za-z0-9_]+), true\\)".r
      }
    }
  }

  it should {
    "return an HTTP code 201 when creating a new common index" in {
      Post(s"/index_getjenny_english_common_0/index_management/create") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.Created
        val response = responseAs[IndexManagementResponse]
        response.message should fullyMatch regex "IndexCreation: " +
          "(?:[A-Za-z0-9_]+)\\(" + Index.indexMatchRegex + "\\.(?:[A-Za-z0-9_]+), true\\) " +
          "(?:[A-Za-z0-9_]+)\\(" + Index.indexMatchRegex + "\\.(?:[A-Za-z0-9_]+), true\\) " +
          "(?:[A-Za-z0-9_]+)\\(" + Index.indexMatchRegex + "\\.(?:[A-Za-z0-9_]+), true\\) " +
          "(?:[A-Za-z0-9_]+)\\(" + Index.indexMatchRegex + "\\.(?:[A-Za-z0-9_]+), true\\) " +
          "(?:[A-Za-z0-9_]+)\\(" + Index.indexMatchRegex + "\\.(?:[A-Za-z0-9_]+), true\\)".r
      }
    }
  }

  for(qaRoute <- qaRoutes) {
    it should {
      s"return an HTTP code 201 when inserting a QA document to the $qaRoute" in {
        for(document <- documents) {
          Post(s"/index_getjenny_english_0/$qaRoute?refresh=1", document) ~> addCredentials(testUserCredentials) ~> routes ~> check {
            status shouldEqual StatusCodes.Created
            val response = responseAs[IndexDocumentResult]
            response.version should be (1)
            response.created should be (true)
          }
        }
      }
    }
  }

  for(qaRoute <- qaRoutes) {
    it should {
      s"return an HTTP code 201 when updating documents in bulk in the $qaRoute" in {
        val request = QADocumentUpdate(
          id = List("id1", "id2", "id3", "id123"),
          coreData = Some(QADocumentCore(
            question = Some("I forgot my password"),
            answer = Some("did you try hunter2?"),
            verified = Some(true)
          )),
          annotations = Some(QADocumentAnnotations(
            feedbackConvScore = Some(1.0),
            algorithmConvScore = Some(1.0),
            feedbackAnswerScore = Some(1.0),
            algorithmAnswerScore = Some(1.0)
          ))
        )
        Put(s"/index_getjenny_english_0/$qaRoute?refresh=1", request) ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.Created
          val response = responseAs[IndexDocumentListResult]
          response.data.map(_.version) should be (List(2,2,2,-1))
          response.data.map(_.id) should be (List("id1","id2","id3","id123"))
          response.data.map(_.created) should contain only false
        }
      }
    }
  }

  for (qaRoute <- qaRoutes) {
    it should {
      s"return an HTTP code 200 when searching documents from the $qaRoute" in {
        val request = QADocumentSearch(
          coreData = Some(QADocumentCore(question = Some("I forgot my password"))),
          size = Some(2)
        )
        val request2 = QADocumentSearch(
          size = Some(0)
        )
        val request3 = QADocumentSearch(
          conversation = Some(List("conv_id_1", "conv_id_2", "conv_id_3", "conv_id_123"))
        )
        Post(s"/index_getjenny_english_0/$qaRoute/search", request) ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[SearchQADocumentsResults]
          response.totalHits should be (3)
          response.hitsCount should be (2)
          response.hits.size should be (2)
          response.hits.map(_.document
            .coreData.getOrElse(fail)
            .question.getOrElse(fail)) should contain only "I forgot my password"
        }
        Post(s"/index_getjenny_english_0/$qaRoute/search", request2) ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[SearchQADocumentsResults]
          response.totalHits should be (5)
          response.hits should be (List.empty)
        }
        Post(s"/index_getjenny_english_0/$qaRoute/search", request3) ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[SearchQADocumentsResults]
          response.totalHits should be (4)
          response.hits.map(_.document.conversation) should contain only ("conv_id_1", "conv_id_2", "conv_id_3")
        }
      }
    }
  }

  for (qaRoute <- qaRoutes) {
    it should {
      s"return an HTTP code 200 when retrieving documents from the $qaRoute" in {
        Get(s"/index_getjenny_english_0/$qaRoute?id=id1&id=id2&id=id3&id=id4&id=id5&id=id6") ~> addCredentials(testUserCredentials) ~> routes ~> check {
          val response = responseAs[SearchQADocumentsResults]
          response.totalHits should be (5)
          response.hits.size should be (5)
          response.hits.map(_.document.id) should contain only ("id1", "id2", "id3", "id4", "id5")
        }
      }
    }
  }

  for (qaRoute <- qaRoutes) {
    it should {
      s"return an HTTP code 200 when retrieving conversations from the $qaRoute" in {
        val request = ListOfDocumentId(ids = List("conv_id_1", "conv_id_2", "conv_id_3", "conv_id_123"))
        Post(s"/index_getjenny_english_0/$qaRoute/conversations", request) ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[Conversations]
          response.total should be (3)
          response.conversations.size should be (3)
          response.conversations.map(_.count) should contain only (1, 2)
          response.conversations.map(_.docs.map(_.conversation)) should contain only
            (List("conv_id_1", "conv_id_1"), List("conv_id_2"), List("conv_id_3"))
        }
      }
    }
  }

  for (qaRoute <- qaRoutes) {
    it should {
      s"return an HTTP code 200 when counting terms from the $qaRoute" in {
        Get(s"/index_getjenny_english_0/term_count/$qaRoute?term=password") ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[TermCount]
          response.count should be (3)
          response.numDocs should be (3)
        }
        Get(s"/index_getjenny_english_0/term_count/$qaRoute?term=capital&") ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[TermCount]
          response.numDocs should be (1)
        }
      }
    }
  }

  for (qaRoute <- qaRoutes) {
    it should {
      s"return an HTTP code 200 when getting dictionary size from the $qaRoute" in {
        Get(s"/index_getjenny_english_0/dict_size/$qaRoute") ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[DictSize]
          response.numDocs should be (5)
          response.answer should be (5)
          response.question should be (10)
          response.total should be (15)
        }
      }
    }
  }

  for (qaRoute <- qaRoutes) {
    it should {
      s"return an HTTP code 200 when getting total term count from the $qaRoute" in {
        Get(s"/index_getjenny_english_0/total_terms/$qaRoute") ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[TotalTerms]
          response.numDocs should be (5)
          response.answer should be (13)
          response.question should be (18)
        }
      }
    }
  }

  for (qaRoute <- qaRoutes) {
    it should {
      s"return an HTTP code 200 when updating the terms in a document in the $qaRoute" in {
        val request = UpdateQATermsRequest(
          id = "id1"
        )

        Put(s"/index_getjenny_english_0/updateTerms/$qaRoute", request) ~> addCredentials(testAdminCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[List[UpdateDocumentResult]]
        }
      }
    }
  }

  for (qaRoute <- qaRoutes) {
    it should {
      s"return an HTTP code 200 when getting analytics from the $qaRoute" in {
        val request = QAAggregatedAnalyticsRequest(
          aggregations = Some(QAAggregationsTypes.values.toList)
        )
        Post(s"/index_getjenny_english_0/analytics/$qaRoute", request) ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[QAAggregatedAnalytics]
          response.avgAlgorithmAnswerScore should not be None
          response.avgAlgorithmConvScore should not be None
          response.avgFeedbackAnswerScore should not be None
          response.avgFeedbackConvScore should not be None
          response.labelCountHistograms should not be None
          response.countOverTimeHistograms should not be None
          response.scoreHistograms should not be None
          response.scoresOverTime should not be None
          response.totalConversations should be (4)
          response.totalDocuments should be (5)
        }
      }
    }
  }

  for (qaRoute <- qaRoutes) {
    it should {
      s"return an HTTP code 200 when deleting documents in bulk from the $qaRoute" in {
        val request = DocsIds(ids = List("id1","id2"))
        Delete(s"/index_getjenny_english_0/$qaRoute?refresh=1", request) ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[DeleteDocumentsResult]
          response.data.size should be (2)
        }
      }
    }
  }

  for (qaRoute <- qaRoutes) {
    it should {
      s"should have lower term count after deleting documents from the $qaRoute" in {
        Get(s"/index_getjenny_english_0/term_count/$qaRoute?term=password") ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[TermCount]
          response.count should be (1)
          response.numDocs should be (1)
        }
      }
    }
  }

  for (qaRoute <- qaRoutes) {
    it should {
      s"should not find any deleted documents by get from the $qaRoute" in {
        Get(s"/index_getjenny_english_0/$qaRoute?id=id1&id=id2&id=id3&id=id4&id=id5") ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[SearchQADocumentsResults]
          response.totalHits should be (3)
          response.hitsCount should be (3)
          response.hits.map(_.document.id) should contain only ("id3", "id4", "id5")
        }
      }
    }
  }

  for (qaRoute <- qaRoutes) {
    it should {
      s"should not find any deleted documents by search from the $qaRoute" in {
        val request = QADocumentSearch()
        Post(s"/index_getjenny_english_0/$qaRoute/search", request) ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[SearchQADocumentsResults]
          response.totalHits should be (3)
          response.hitsCount should be (3)
          response.hits.map(_.document.id) should contain only ("id3", "id4", "id5")
        }
      }
    }
  }

  for (qaRoute <- qaRoutes) {
    it should {
      s"return an HTTP code 200 when deleting all documents from the $qaRoute" in {
        val request = DocsIds(ids = Nil)
        Delete(s"/index_getjenny_english_0/$qaRoute?refresh=1", request) ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[DeleteDocumentsSummaryResult]
          response.deleted should be (3)
        }
      }
    }
  }

  for (qaRoute <- qaRoutes) {
    it should {
      s"should not find any documents by get in the $qaRoute" in {
        Get(s"/index_getjenny_english_0/$qaRoute?id=id1&id=id2&id=id3&id=id4&id=id5") ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[SearchQADocumentsResults]
          response.totalHits should be (0)
        }
      }
    }
  }

  for (qaRoute <- qaRoutes) {
    val document = QADocument(
      id = "id94",
      conversation = "conv_id_1",
      indexInConversation = 1,
      coreData = Some(QADocumentCore(
        question = Some(""),
        answer = Some("")
      )),
      annotations = Some(QADocumentAnnotations(
        doctype = Some(Doctypes.NORMAL),
        agent = Some(Agent.HUMAN_REPLY),
      ))
    )
    it should {
      s"should not store empty strings in question or answer fields in the $qaRoute" in {
        Post(s"/index_getjenny_english_0/$qaRoute?refresh=1", document) ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.Created
          val response = responseAs[IndexDocumentResult]
          response.id
        }.andThen(id => {
          Get(s"/index_getjenny_english_0/$qaRoute?id=$id") ~> addCredentials(testUserCredentials) ~> routes ~> check {
            status shouldEqual StatusCodes.OK
            val response = responseAs[SearchQADocumentsResults]
            val document = response.hits.headOption.getOrElse(fail("document not found")).document
            val coreData = document.coreData.getOrElse(fail("no coreData"))
            coreData.answer shouldEqual None
            coreData.question shouldEqual None
          }
        })
      }
    }
  }

  it should {
    "return an HTTP code 200 when deleting an index" in {
      Delete(s"/index_getjenny_english_0/index_management") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[IndexManagementResponse]
      }
    }
  }

  it should {
    "return an HTTP code 200 when deleting an common index" in {
      Delete(s"/index_getjenny_english_common_0/index_management") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[IndexManagementResponse]
      }
    }
  }

  it should {
    "return an HTTP code 200 when deleting an existing system index" in {
      Delete(s"/system_index_management") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[IndexManagementResponse]
      }
    }
  }

}
