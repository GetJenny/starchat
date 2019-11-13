package com.getjenny.starchat.resources

import akka.http.scaladsl.model.StatusCodes
import com.getjenny.starchat.entities._
import com.getjenny.starchat.entities.es._

class SpellcheckResourceTest extends TestEnglishBase {

  "StarChat" should {
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
    "return an HTTP code 201 when populating knowledge base" in {
      val knowledgeBaseRequest: QADocument = QADocument(
        id = "0",
        conversation = "id:1000",
        indexInConversation = 1,
        coreData = Some(QADocumentCore(
          question = Some("is this text mispelled?"),
          answer = Some("it might be")
        )),
        annotations = Some(QADocumentAnnotations(
          doctype = Some(Doctypes.NORMAL),
          agent = Some(Agent.STARCHAT),
          escalated = Some(Escalated.UNSPECIFIED),
          answered = Some(Answered.UNSPECIFIED),
          triggered = Some(Triggered.UNSPECIFIED),
          followup = Some(Followup.UNSPECIFIED)
        ))
      )
      Post(s"/index_getjenny_english_0/knowledgebase?refresh=1", knowledgeBaseRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.Created
        val response = responseAs[IndexDocumentResult]
        response.created should be (true)
        response.id should be ("0")
        response.index should be ("index_english.question_answer")
      }
    }
  }

  it should {
    val spellcheckRequest: SpellcheckTermsRequest = SpellcheckTermsRequest(
      text = "is this text misplelled",
      minDocFreq = 0
    )

    s"return an HTTP code 200 when spellchecking" in {
      Post(s"/index_getjenny_english_0/spellcheck/terms", spellcheckRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[SpellcheckTermsResponse]
        response.tokens.map(_.text) should contain only ("is", "this", "text", "misplelled")
        response.tokens.find(_.text === "misplelled").getOrElse(fail).options match {
          case SpellcheckTokenSuggestions(_, _, text) :: Nil => text should be ("mispelled")
          case _ => fail("Spellcheck didn't correct misplelled")
        }
      }
    }
  }

  it should {
    val spellcheckRequest: SpellcheckTermsRequest = SpellcheckTermsRequest(
      text = "is this text misplelled",
      minDocFreq = -1
    )

    s"return an HTTP code 400 when minDocFreq is negative" in {
      Post(s"/index_getjenny_english_0/spellcheck/terms", spellcheckRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
        val response = responseAs[ReturnMessageData]
        response.code should be (100)
        response.message should be("minDocFreq must be positive")
      }
    }
  }

  it should {
    val spellcheckRequest: SpellcheckTermsRequest = SpellcheckTermsRequest(
      text = "is this text misplelled",
      prefixLength = -1
    )
    s"return an HTTP code 400 when prefixLength is negative" in {

      Post(s"/index_getjenny_english_0/spellcheck/terms", spellcheckRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
        val response = responseAs[ReturnMessageData]
        response.code should be (100)
        response.message should be("prefixLength must be positive")
      }
    }
  }

  it should {
    val spellcheckRequest: SpellcheckTermsRequest = SpellcheckTermsRequest(
      text = "is this text misplelled",
      maxEdit = 0
    )
    val spellcheckRequest2 = spellcheckRequest.copy(maxEdit = 3)

    s"return an HTTP code 400 when maxEdit is not between 1 and 2" in {
      Post(s"/index_getjenny_english_0/spellcheck/terms", spellcheckRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
        val response = responseAs[ReturnMessageData]
        response.code should be (100)
        response.message should be ("maxEdits must be between 1 and 2")
      }
      Post(s"/index_getjenny_english_0/spellcheck/terms", spellcheckRequest2) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
        val response = responseAs[ReturnMessageData]
        response.code should be (100)
        response.message should be ("maxEdits must be between 1 and 2")
      }
    }
  }

  it should {
    val deleteRequest: ListOfDocumentId = ListOfDocumentId(ids = List("0"))
    "return an HTTP code 200 when deleting an document from knowledgebase" in {
      Delete(s"/index_getjenny_english_0/knowledgebase", deleteRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[DeleteDocumentsResult]
        response.data.size should be (1)
        response.data.headOption match {
          case Some(result) => result.id should be ("0")
          case None => fail
        }
      }
    }
  }

}
