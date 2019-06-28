package com.getjenny.starchat.resources

import akka.actor.ActorSystem
import akka.http.scaladsl.model.headers.BasicHttpCredentials
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, Multipart, StatusCodes}
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import akka.testkit._
import com.getjenny.starchat.entities._
import com.getjenny.starchat.serializers.JsonSupport
import com.getjenny.starchat.utils.Index
import org.scalatest.{Matchers, WordSpec}

import scala.concurrent.duration._

class DecisionTableResourceTest extends WordSpec with Matchers with ScalatestRouteTest with JsonSupport {
  implicit def default(implicit system: ActorSystem) = RouteTestTimeout(10.seconds.dilated(system))

  val service = TestFixtures.service
  val routes = service.routes

  val testAdminCredentials = BasicHttpCredentials("admin", "adminp4ssw0rd")
  val testUserCredentials = BasicHttpCredentials("test_user", "p4ssw0rd")

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
    "return an HTTP code 201 when indexing a decision table from csv file" in {

      val input_file = getClass.getResourceAsStream("/doc/decision_table_starchat_doc.csv")
      val input_data = scala.io.Source.fromInputStream(input_file).mkString

      val multipartForm =
        Multipart.FormData(
          Multipart.FormData.BodyPart.Strict(
            "csv",
            HttpEntity(ContentTypes.`text/plain(UTF-8)`, input_data),
            Map("filename" -> "data.csv")))

      Post(s"/index_getjenny_english_0/decisiontable/upload/csv", multipartForm) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[IndexDocumentListResult]
      }
    }
  }

  it should {
    "return an HTTP code 201 when creating a new document" in {
      val decisionTableRequest = DTDocument(
        state = "forgot_password",
        executionOrder = 0,
        maxStateCount = 0,
        analyzer = "",
        queries = List(),
        bubble = "",
        action = "",
        actionInput = Map(),
        stateData = Map(),
        successValue = "",
        failureValue = "",
        evaluationClass = Some("default"),
        version = None
      )

      val decisionTableRequest2 = DTDocument(
        state = "dont_tell_password",
        executionOrder = 0,
        maxStateCount = 0,
        analyzer = "bor(keyword(\"password\"))",
        queries = List(),
        bubble = "Never tell your password to anyone!",
        action = "",
        actionInput = Map(),
        stateData = Map(),
        successValue = "",
        failureValue = "",
        evaluationClass = Some("default"),
        version = None
      )

      Post(s"/index_getjenny_english_0/decisiontable?refresh=1", decisionTableRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.Created
        val response = responseAs[IndexDocumentResult]
        response.created should be (true)
        response.id should be ("forgot_password")
        response.index should be ("index_getjenny_english_0.state")
        response.version should be (1)
      }
      Post(s"/index_getjenny_english_0/decisiontable?refresh=1", decisionTableRequest2) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.Created
        val response = responseAs[IndexDocumentResult]
        response.created should be (true)
        response.id should be ("dont_tell_password")
        response.index should be ("index_getjenny_english_0.state")
        response.version should be (1)
      }
    }
  }

  it should {
    "return an HTTP code 201 when creating a new document with state that already exist" in {
      val decisionTableRequest = DTDocument(
        state = "forgot_password",
        executionOrder = 0,
        maxStateCount = 0,
        analyzer = "reinfConjunction(bor(keyword(\"forgot\"), keyword(\"don't remember\")), bor(keyword(\"password\")))",
        queries = List("I forgot my password",
          "my password is wrong",
          "don't remember the password"),
        bubble = "Hello %name%, how can I help you?",
        action = "show_button",
        actionInput = Map("text to be shown on button" -> "password_recovery"),
        stateData = Map("url" -> "www.getjenny.com"),
        successValue = "eval(show_buttons)",
        failureValue = "dont_understand",
        evaluationClass = Some("default"),
        version = None
      )

      Post(s"/index_getjenny_english_0/decisiontable?refresh=1", decisionTableRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.Created
        val response = responseAs[IndexDocumentResult]
        response.created should be (false)
        response.id should be ("forgot_password")
        response.index should be ("index_getjenny_english_0.state")
        response.version should be (2)
      }
    }
  }

  it should {
    "return an HTTP code 200 when updating an existing document" in {
      val decisionTableRequest = DTDocumentUpdate(
        state = "forgot_password",
        executionOrder = None,
        maxStateCount = None,
        analyzer = None,
        queries = Some(List(
          "I forgot my password",
          "my password is wrong",
          "don't remember the password",
          "I don't know my password")),
        bubble = None,
        action = None,
        actionInput = None,
        stateData = None,
        successValue = None,
        failureValue = None,
        evaluationClass = None
      )

      Put(s"/index_getjenny_english_0/decisiontable", decisionTableRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[UpdateDocumentResult]
        response.created should be (false)
        response.id should be ("forgot_password")
        response.index should be ("index_getjenny_english_0.state")
        response.version should be (3)
      }
    }
  }

  it should {
    "return an HTTP code 400 when updating a non-existing document" in {
      val decisionTableRequest = DTDocumentUpdate(
        state = "house_is_on_fire",
        executionOrder = Some(0),
        maxStateCount = Some(0),
        analyzer = Some("keyword(\"fire\")"),
        queries = Some(List(
          "Bots are not working",
          "House is on fire")),
        bubble = Some("House is on fire!"),
        action = Some(""),
        actionInput = Some(Map()),
        stateData = Some(Map()),
        successValue = Some(""),
        failureValue = Some(""),
        evaluationClass = Some("")
      )

      Put(s"/index_getjenny_english_0/decisiontable", decisionTableRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
        val response = responseAs[ReturnMessageData]
      }
    }
  }

  it should {
    "return an HTTP code 200 when getting documents by id" in {
      Get("/index_getjenny_english_0/decisiontable?id=forgot_password&id=call_operator") ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[SearchDTDocumentsResults]
        response.total should be (2)
        response.hits.map(_.document.state) should contain only ("forgot_password", "call_operator")
      }
    }
  }

  it should {
    "return an HTTP code 200 when dumping all documents" in {
      Get("/index_getjenny_english_0/decisiontable?dump=true") ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[SearchDTDocumentsResults]
        response.total should be (21)
      }
    }
  }

  it should {
    "return an HTTP code 400 when no queries are given" in {
      Get("/index_getjenny_english_0/decisiontable") ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
        val resopnse =responseAs[ReturnMessageData]
      }
    }
  }

  it should {
    "return an HTTP code 200 when searching documents" in {
      val searchRequest = DTDocumentSearch(
        from = Some(0),
        size = Some(10),
        executionOrder = None,
        minScore = Some(0.0F),
        boostExactMatchFactor = Some(100),
        state = None,
        evaluationClass = None,
        queries = Some("I forgot my password"),
        searchAlgorithm = Some(SearchAlgorithm.NGRAM3)
      )

      Post("/index_getjenny_english_0/decisiontable/search", searchRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[SearchDTDocumentsResults]
        response.hits.headOption.getOrElse(fail).document.state should be ("forgot_password")
      }
    }
  }

  it should {
    "return an HTTP code 200 when searching documents and no documents are found" in {
      val searchRequest = DTDocumentSearch(
        from = Some(0),
        size = Some(10),
        executionOrder = Some(0),
        minScore = Some(0.6F),
        boostExactMatchFactor = Some(100),
        state = None,
        evaluationClass = None,
        queries = Some("I need coffee!!!"),
        searchAlgorithm = Some(SearchAlgorithm.NGRAM3)
      )

      Post("/index_getjenny_english_0/decisiontable/search", searchRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[SearchDTDocumentsResults]
        response.total should be (0)
      }
    }
  }

  it should {
    "return an HTTP code 205 getting next response and decisiontable is not updated" in {
      val request = ResponseRequestIn(conversationId = "conv_12131",
          traversedStates = None,
          userInput = Some(ResponseRequestInUserInput(text = Some("It doesn't matter what I say here when state is defined"), img = None
          )),
          state = None,
          data = Some(Map("name" -> "Donald Duck", "job" -> "idle")),
          threshold = Some(0),
          evaluationClass = None,
          maxResults = Some(1),
          searchAlgorithm = Some(SearchAlgorithm.NGRAM3)
        )

      Post("/index_getjenny_english_0/get_next_response", request) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.ResetContent
        val response = responseAs[ResponseRequestOutOperationResult]
      }
    }
  }

  it should {
    "return an HTTP code 200 when triggering an update of the DecisionTable" in {
      Post("/index_getjenny_english_0/decisiontable/analyzer") ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[DTAnalyzerLoad]
      }
    }
  }

  it should {
    "return an HTTP code 200 when getting runtime list of analyzers" in {
      Get("/index_getjenny_english_0/decisiontable/analyzer") ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[DTAnalyzerMap]
      }
    }
  }

  it should {
    "return an HTTP code 200 when triggering an asynchronous update of the DecisionTable" in {
      Post("/index_getjenny_english_0/decisiontable/analyzer/async") ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.Accepted
        val response = responseAs[DtReloadTimestamp]
      }
    }
  }

  it should {
    "return an HTTP code 200 when getting next response by state" in {
      val request = ResponseRequestIn(conversationId = "conv_12131",
        traversedStates = None,
        userInput = None,
        state = Some(List("forgot_password")),
        data = Some(Map("name" -> "Donald Duck", "job" -> "idle")),
        threshold = None,
        evaluationClass = None,
        maxResults = None,
        searchAlgorithm = Some(SearchAlgorithm.NGRAM3)
      )

      Post("/index_getjenny_english_0/get_next_response", request) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[List[ResponseRequestOut]]
        val headResponseRequestOut = response.headOption.getOrElse(fail)
        headResponseRequestOut.bubble should be ("Hello Donald Duck, how can I help you?")
        headResponseRequestOut.traversedStates should be (Vector("forgot_password"))
      }
    }
  }

  it should {
    "return an HTTP code 200 when restricting the search to a subset of states" in {
      val request = ResponseRequestIn(
        conversationId = "conv_12345",
        traversedStates = None,
        userInput = Some(ResponseRequestInUserInput(
          text = Some("I forgot my password"),
          img = None
        )),
        state = Some(List(
          "help",
          "contribute",
          "forgot_password",
          "dont_tell_password"
        )),
        data = None,
        threshold = Some(1),
        evaluationClass = None,
        maxResults = Some(10),
        searchAlgorithm = Some(SearchAlgorithm.NGRAM2)
      )

      val request2 = ResponseRequestIn(
        conversationId = "conv_12345",
        traversedStates = None,
        userInput = Some(ResponseRequestInUserInput(
          text = Some("I forgot my password"),
          img = None
        )),
        state = Some(List(
          "help",
          "contribute",
          "forgot_password"
        )),
        data = None,
        threshold = Some(1),
        evaluationClass = None,
        maxResults = Some(10),
        searchAlgorithm = Some(SearchAlgorithm.NGRAM2)
      )

      Post("/index_getjenny_english_0/get_next_response", request) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[List[ResponseRequestOut]]
        response.map(_.state) should contain only ("forgot_password", "dont_tell_password")
      }

      Post("/index_getjenny_english_0/get_next_response", request2) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[List[ResponseRequestOut]]
        response.map(_.state) should contain only "forgot_password"
      }
    }
  }

  it should {
    "return an HTTP code 400 when get_next_response request contains a non-existing state" in {
      val request = ResponseRequestIn(conversationId = "conv_12131",
        traversedStates = None,
        userInput = None,
        state = Some(List("forgot_password", "this_state_does_not_exist")),
        data = None,
        threshold = Some(0),
        evaluationClass = None,
        maxResults = Some(1),
        searchAlgorithm = Some(SearchAlgorithm.NGRAM3)
      )

      Post("/index_getjenny_english_0/get_next_response", request) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
        val response = responseAs[ResponseRequestOutOperationResult]
      }
    }
  }

  it should {
    "return an HTTP code 204 when getting next response by search and no states found" in {
      val request = ResponseRequestIn(conversationId = "conv_12131",
        traversedStates = None,
        userInput = Some(ResponseRequestInUserInput(text = Some("I need coffee!!!"), img = None
        )),
        state = None,
        data = None,
        threshold = Some(0.6),
        evaluationClass = None,
        maxResults = Some(1),
        searchAlgorithm = Some(SearchAlgorithm.NGRAM3)
      )

      Post("/index_getjenny_english_0/get_next_response", request) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.NoContent
      }
    }
  }

  it should {
    "return an HTTP code 200 when getting next response by search" in {
      val request = ResponseRequestIn(conversationId = "conv_12131",
        traversedStates = Some(Vector("state_0", "state_1", "state_2", "state_3")),
        userInput = Some(ResponseRequestInUserInput(text = Some("I forgot my password"), img = None
        )),
        state = None,
        data = Some(Map("name" -> "Donald Duck", "job" -> "idle")),
        threshold = Some(0),
        evaluationClass = None,
        maxResults = Some(1),
        searchAlgorithm = Some(SearchAlgorithm.NGRAM3)
      )

      Post("/index_getjenny_english_0/get_next_response", request) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[List[ResponseRequestOut]]
        val headResponseRequestOut = response.headOption.getOrElse(fail)
        headResponseRequestOut.bubble should be ("Hello Donald Duck, how can I help you?")
        headResponseRequestOut.traversedStates should be (Vector("state_0", "state_1", "state_2", "state_3", "forgot_password"))
      }
    }
  }

  it should {
    "return an HTTP code 200 when deleting a document" in {
      Delete("/index_getjenny_english_0/decisiontable?id=forgot_password&refresh=1") ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[DeleteDocumentsResult]
        val headDeleteDocumentResult = response.data.headOption.getOrElse(fail)
        headDeleteDocumentResult.index should be ("index_getjenny_english_0.state")
        headDeleteDocumentResult.id should be ("forgot_password")
        headDeleteDocumentResult.found should be (true)
        headDeleteDocumentResult.version should be (4)
      }
    }
  }

  it should {
    "return an HTTP code 200 when getting a deleted document" in {
      Get("/index_getjenny_english_0/decisiontable?id=forgot_password") ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[SearchDTDocumentsResults]
        response.total should be (0)
        response.hits.isEmpty should be (true)
      }
    }
  }

  it should {
    "return an HTTP code 200 when deleting all documents" in {
      Delete("/index_getjenny_english_0/decisiontable/all") ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[DeleteDocumentsSummaryResult]
        response.deleted should be (20)
      }
    }
  }

  it should {
    "return an HTTP code 200 when deleting an index" in {
      Delete(s"/index_getjenny_english_0/index_management") ~>  addCredentials(testAdminCredentials) ~> routes ~> check {
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


