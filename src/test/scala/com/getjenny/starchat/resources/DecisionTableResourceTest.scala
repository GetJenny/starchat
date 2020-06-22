package com.getjenny.starchat.resources

import akka.http.scaladsl.model.{ContentTypes, HttpEntity, Multipart, StatusCodes}
import com.getjenny.starchat.TestEnglishBase
import com.getjenny.starchat.entities.io._
import com.getjenny.starchat.entities.persistents._
import spray.json._

class DecisionTableResourceTest extends TestEnglishBase {

  "StarChat" should {
    "return an HTTP code 201 when indexing a decision table from csv file" in {

      val inputFile = getClass.getResourceAsStream("/doc/decision_table_starchat_doc.csv")
      val inputData = scala.io.Source.fromInputStream(inputFile).mkString

      val multipartForm =
        Multipart.FormData(
          Multipart.FormData.BodyPart.Strict(
            "csv",
            HttpEntity(ContentTypes.`text/plain(UTF-8)`, inputData),
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
        actionInput = Seq.empty[JsObject],
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
        analyzer = "bor(vOneKeyword(\"password\"))",
        queries = List(),
        bubble = "Never tell your password to anyone!",
        action = "",
        actionInput = Seq.empty[JsObject],
        stateData = Map(),
        successValue = "",
        failureValue = "",
        evaluationClass = Some("default"),
        version = None
      )

      Post(s"/index_getjenny_english_0/decisiontable?refresh=wait_for", decisionTableRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.Created
        val response = responseAs[IndexDocumentResult]
        response.created should be (true)
        response.id should be ("forgot_password")
        response.index should be ("index_english.state")
        response.version should be (1)
      }
      Post(s"/index_getjenny_english_0/decisiontable?refresh=wait_for", decisionTableRequest2) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.Created
        val response = responseAs[IndexDocumentResult]
        response.created should be (true)
        response.id should be ("dont_tell_password")
        response.index should be ("index_english.state")
        response.version should be (1)
      }
    }
  }

  it should {
    "return an HTTP code 201 when creating a new document with state that already exist" in {
      val actionInput = Seq(
        Map("text to be shown on button" -> "password_recovery").toJson.asJsObject
      )
      val decisionTableRequest = DTDocument(
        state = "forgot_password",
        executionOrder = 0,
        maxStateCount = 0,
        analyzer = "reinfConjunction(bor(vOneKeyword(\"forgot\"), vOneKeyword(\"don't remember\")), bor(vOneKeyword(\"password\")))",
        queries = List("I forgot my password",
          "my password is wrong",
          "don't remember the password"),
        bubble = "Hello %name%, how can I help you?",
        action = "show_button",
        actionInput = actionInput,
        stateData = Map("url" -> "www.getjenny.com"),
        successValue = "eval(show_buttons)",
        failureValue = "dont_understand",
        evaluationClass = Some("default"),
        version = None
      )

      Post(s"/index_getjenny_english_0/decisiontable?refresh=wait_for", decisionTableRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.Created
        val response = responseAs[IndexDocumentResult]
        response.created should be (false)
        response.id should be ("forgot_password")
        response.index should be ("index_english.state")
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
        response.index should be ("index_english.state")
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
        analyzer = Some("vOneKeyword(\"fire\")"),
        queries = Some(List(
          "Bots are not working",
          "House is on fire")),
        bubble = Some("House is on fire!"),
        action = Some(""),
        actionInput = Some(Seq.empty[JsObject]),
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
        response.total should be (24)
      }
    }
  }

  it should {
    "return an HTTP code 400 when no queries are given" in {
      Get("/index_getjenny_english_0/decisiontable") ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
        val resopnse = responseAs[ReturnMessageData]
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
        println(responseAs[String])
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
    "return an HTTP code 200  and returning a random bubble response" in {
      val actionInput = Seq(
        Map("text to be shown on button" -> "password_recovery").toJson.asJsObject
      )
      val decisionTableRequest = DTDocument(
        state = "forgot_password",
        executionOrder = 0,
        maxStateCount = 0,
        analyzer = "reinfConjunction(bor(vOneKeyword(\"forgot\"), vOneKeyword(\"don't remember\")), bor(vOneKeyword(\"password\")))",
        queries = List("I forgot my password",
          "my password is wrong",
          "don't remember the password"),
        bubble = "Hello %name%, how can I help you?|Hello %name%, ask me anything!",
        action = "show_button",
        actionInput = actionInput,
        stateData = Map("url" -> "www.getjenny.com"),
        successValue = "eval(show_buttons)",
        failureValue = "dont_understand",
        evaluationClass = Some("default"),
        version = None
      )

      Post(s"/index_getjenny_english_0/decisiontable?refresh=wait_for", decisionTableRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.Created
      }

      Post(s"/index_getjenny_english_0/decisiontable/analyzer", decisionTableRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
      }

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
        assert(headResponseRequestOut.bubble === "Hello Donald Duck, how can I help you?" ||
          headResponseRequestOut.bubble === "Hello Donald Duck, ask me anything!")
      }
    }
  }

  it should {
    "preserve actionInput order" in {
      val actionInput = Seq(
        Map("action1" -> "action1").toJson.asJsObject,
        Map("action2" -> "action2").toJson.asJsObject
      )
      val decisionTableRequest = DTDocument(
        state = "forgot_password",
        executionOrder = 0,
        maxStateCount = 0,
        analyzer = "reinfConjunction(bor(vOneKeyword(\"forgot\"), vOneKeyword(\"don't remember\")), bor(vOneKeyword(\"password\")))",
        queries = List("I forgot my password",
          "my password is wrong",
          "don't remember the password"),
        bubble = "Hello how can I help you?",
        action = "show_button",
        actionInput = actionInput,
        stateData = Map("url" -> "www.getjenny.com"),
        successValue = "eval(show_buttons)",
        failureValue = "dont_understand",
        evaluationClass = Some("default"),
        version = None
      )

      Post(s"/index_getjenny_english_0/decisiontable?refresh=wait_for", decisionTableRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.Created
      }

      Post(s"/index_getjenny_english_0/decisiontable/analyzer", decisionTableRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
      }

      val request = ResponseRequestIn(conversationId = "conv_12131",
        traversedStates = Some(Vector("state_0", "state_1", "state_2", "state_3")),
        userInput = Some(ResponseRequestInUserInput(text = Some("I forgot my password"), img = None
        )),
        state = None,
        data = Some(Map("number" -> "5", "action" -> "test")),
        threshold = Some(0),
        evaluationClass = None,
        maxResults = Some(1),
        searchAlgorithm = Some(SearchAlgorithm.NGRAM3)
      )

      Post("/index_getjenny_english_0/get_next_response", request) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[List[ResponseRequestOut]]
        val headResponseRequestOut: ResponseRequestOut = response.headOption.getOrElse(fail)
        headResponseRequestOut.actionInput.head shouldEqual actionInput.head
        headResponseRequestOut.actionInput.tail.head shouldEqual actionInput.tail.head
      }
    }
  }

  it should {
    "return an HTTP code 200  and returning a replaced dictionary" in {
      val actionInput = Seq(
        Map("text to be shown on button number: %number%" -> "test action %action%").toJson.asJsObject
      )
      val decisionTableRequest = DTDocument(
        state = "forgot_password",
        executionOrder = 0,
        maxStateCount = 0,
        analyzer = "reinfConjunction(bor(vOneKeyword(\"forgot\"), vOneKeyword(\"don't remember\")), bor(vOneKeyword(\"password\")))",
        queries = List("I forgot my password",
          "my password is wrong",
          "don't remember the password"),
        bubble = "Hello how can I help you?",
        action = "show_button",
        actionInput = actionInput,
        stateData = Map("url" -> "www.getjenny.com"),
        successValue = "eval(show_buttons)",
        failureValue = "dont_understand",
        evaluationClass = Some("default"),
        version = None
      )

      Post(s"/index_getjenny_english_0/decisiontable?refresh=wait_for", decisionTableRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.Created
      }

      Post(s"/index_getjenny_english_0/decisiontable/analyzer", decisionTableRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
      }

      val request = ResponseRequestIn(conversationId = "conv_12131",
        traversedStates = Some(Vector("state_0", "state_1", "state_2", "state_3")),
        userInput = Some(ResponseRequestInUserInput(text = Some("I forgot my password"))),
        state = None,
        data = Some(Map("number" -> "5", "action" -> "test")),
        threshold = Some(0),
        evaluationClass = None,
        maxResults = Some(1),
        searchAlgorithm = Some(SearchAlgorithm.NGRAM3)
      )

      Post("/index_getjenny_english_0/get_next_response", request) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[List[ResponseRequestOut]]
        val headResponseRequestOut: ResponseRequestOut = response.headOption.getOrElse(fail)
        headResponseRequestOut.actionInput.head shouldEqual Map("text to be shown on button number: 5" -> "test action test").toJson.asJsObject
      }
    }
  }

  it should {
    "return an HTTP code 200 and not returning string templates in action and bubble if there are no substitutions" in {
      val actionInput = Seq(
        Map("test template %not_available%" -> "test action %action%").toJson.asJsObject
      )
      val decisionTableRequest = DTDocument(
        state = "forgot_password",
        executionOrder = 0,
        maxStateCount = 0,
        analyzer = "reinfConjunction(bor(vOneKeyword(\"forgot\"), vOneKeyword(\"don't remember\")), bor(vOneKeyword(\"password\")))",
        queries = List("I forgot my password",
          "my password is wrong",
          "don't remember the password"),
        bubble = "Hello %name% how can I help you?",
        action = "show_button",
        actionInput = actionInput,
        stateData = Map("url" -> "www.getjenny.com"),
        successValue = "eval(show_buttons)",
        failureValue = "dont_understand",
        evaluationClass = Some("default"),
        version = None
      )

      Post(s"/index_getjenny_english_0/decisiontable?refresh=wait_for", decisionTableRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.Created
      }

      Post(s"/index_getjenny_english_0/decisiontable/analyzer", decisionTableRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
      }

      val request = ResponseRequestIn(conversationId = "conv_12131",
        traversedStates = Some(Vector("state_0", "state_1", "state_2", "state_3")),
        userInput = Some(ResponseRequestInUserInput(text = Some("I forgot my password"))),
        state = None,
        data = Some(Map.empty),
        threshold = Some(0),
        evaluationClass = None,
        maxResults = Some(1),
        searchAlgorithm = Some(SearchAlgorithm.NGRAM3)
      )

      Post("/index_getjenny_english_0/get_next_response", request) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[List[ResponseRequestOut]]
        val headResponseRequestOut: ResponseRequestOut = response.headOption.getOrElse(fail)
        headResponseRequestOut.actionInput.head shouldEqual Map("test template " -> "test action ").toJson.asJsObject
      }
    }
  }

  it should {
    "return an HTTP code 200 and call an http atom" in {
      val decisionTableRequest = DTDocument(
        state = "forgot_password",
        executionOrder = 0,
        maxStateCount = 0,
        analyzer = """vOneKeyword("email")""",
        queries = List("my email is"),
        bubble = "Thank you",
        action = """com.getjenny.analyzer.analyzers.DefaultParser matchPatternRegex("[email](?:([a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+))")""",
        actionInput = List.empty,
        stateData = Map("url" -> "www.getjenny.com"),
        successValue = "thanks_email",
        failureValue = "dont_understand",
        evaluationClass = Some("default"),
        version = None
      )

      Post(s"/index_getjenny_english_0/decisiontable?refresh=wait_for", decisionTableRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.Created
      }

      Post(s"/index_getjenny_english_0/decisiontable/analyzer", decisionTableRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
      }

      val request = ResponseRequestIn(conversationId = "conv_12131",
        traversedStates = Some(Vector("state_0", "state_1", "state_2", "state_3")),
        userInput = Some(ResponseRequestInUserInput(text = Some("my email is this.is.test@email.com"))),
        state = Some(List("forgot_password")),
        data = None,
        threshold = Some(0.5),
        evaluationClass = None,
        maxResults = None,
        searchAlgorithm = Some(SearchAlgorithm.NGRAM3)
      )

      Post("/index_getjenny_english_0/get_next_response", request) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[List[ResponseRequestOut]]
        val out: ResponseRequestOut = response.headOption.getOrElse(fail)
        out.state shouldEqual "thanks_email"
        out.score shouldEqual 0.5
      }
    }
  }

  it should {
    "return an HTTP code 200 when deleting a document" in {
      Delete("/index_getjenny_english_0/decisiontable?id=forgot_password&refresh=wait_for") ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[DeleteDocumentsResult]
        val headDeleteDocumentResult = response.data.headOption.getOrElse(fail)
        headDeleteDocumentResult.index should be ("index_english.state")
        headDeleteDocumentResult.id should be ("forgot_password")
        headDeleteDocumentResult.found should be (true)
        headDeleteDocumentResult.version should be (9)
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
    "return an HTTP code 200 when bulk deleting documents" in {
      val request = DocsIds(ids = List("hello2","hello3"))
      Post("/index_getjenny_english_0/decisiontable/delete", request) ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[DeleteDocumentsResult]
          response.data.size should be (2)
      }
    }
  }

  it should {
    "return an HTTP code 200 when deleting all documents" in {
      Delete("/index_getjenny_english_0/decisiontable/all") ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[DeleteDocumentsSummaryResult]
        response.deleted should be (21)
      }
    }
  }

}


