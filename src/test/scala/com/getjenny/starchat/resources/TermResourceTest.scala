package com.getjenny.starchat.resources

import akka.actor.ActorSystem
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, Multipart, StatusCodes}
import akka.http.scaladsl.model.headers.BasicHttpCredentials
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import akka.testkit._
import com.getjenny.starchat.entities._
import com.getjenny.starchat.serializers.JsonSupport
import com.getjenny.starchat.utils.Index
import org.scalatest.{Matchers, WordSpec}

import scala.io.Source.fromResource
import scala.concurrent.duration._

class TermResourceTest extends WordSpec with Matchers with ScalatestRouteTest with JsonSupport {
  implicit def default(implicit system: ActorSystem) = RouteTestTimeout(20.seconds.dilated(system))

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
    "return an HTTP code 200 when creating multiple terms" in {
      val terms = Terms(
        terms = List(Term(term = "term1"), Term(term = "term2"))
      )
      Post("/index_getjenny_english_0/term/index?refresh=1", terms) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[UpdateDocumentsResult]
        response.data.length should be (terms.terms.length)
      }
    }

    "return an HTTP code 400 when creating terms with empty list" in {
      Post("/index_getjenny_english_0/term/index", Terms(Nil)) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
        val response = responseAs[ReturnMessageData]
      }
    }
  }

  it should {
    "return an HTTP code 200 when calculating term distances with a list of term ids" in {
      val docsIds = DocsIds(List("term1", "term2"))
      Post("/index_getjenny_english_0/term/distance", docsIds) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[List[TermsDistanceRes]]
        response.length should be (docsIds.ids.length)
      }
    }

    "return an HTTP code 400 when calculating term distances without payload" in {
      Post("/index_getjenny_english_0/term/distance") ~> addCredentials(testUserCredentials) ~> Route.seal(routes) ~> check {
        status shouldEqual StatusCodes.BadRequest
      }
    }
  }

  it should {
    "return an HTTP code 200 when updating terms" in {
      val terms = Terms(List(
        Term(term="term1", synonyms = Some(Map("term2" -> 1.0))),
        Term(term="term2", synonyms = Some(Map("term1" -> 1.0)))
      ))
      Put("/index_getjenny_english_0/term?refresh=1", terms) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[UpdateDocumentsResult]
        response.data.length should be (terms.terms.length)
      }
    }
  }

  it should {
    "return an HTTP code 200 when streaming terms" in {
      Get("/index_getjenny_english_0/stream/term") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[String]
        response should (equal("""{"synonyms":{"term1":1.0},"term":"term2"}""" + "\n" +
            """{"synonyms":{"term2":1.0},"term":"term1"}""")
          or equal("""{"synonyms":{"term2":1.0},"term":"term1"}""" + "\n" +
            """{"synonyms":{"term1":1.0},"term":"term2"}"""))
      }
    }
  }

  it should {
    "return an HTTP code 200 when indexing default synonyms" in {
      Post("/index_getjenny_english_0/term/index_default_synonyms?refresh=1") ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[UpdateDocumentsResult]
      }
    }
  }

  it should {
    "return an HTTP code 200 when indexing synonyms from a file" in {
      val synonyms = fromResource("index_management/json_index_spec/english/synonyms.csv").mkString
      val multipartForm = Multipart.FormData(
        Multipart.FormData.BodyPart.Strict(
          "csv",
          HttpEntity(ContentTypes.`text/plain(UTF-8)`, synonyms),
          Map("filename" -> "synonyms.csv")
        ))

      Post("/index_getjenny_english_0/term/index_synonyms", multipartForm) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[UpdateDocumentsResult]
      }
    }
  }

  it should {
    "return an HTTP code 200 when getting terms" in {
      val docsIds = DocsIds(List("hello", "typo"))
      Post("/index_getjenny_english_0/term/get", docsIds) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[Terms]
        response.terms.map(_.term) shouldEqual docsIds.ids
      }
    }
  }

  it should {
    "return an HTTP code 200 when searching term" in {
      val searchTerm = SearchTerm(term = Some("hello"))
      Post("/index_getjenny_english_0/term/term", searchTerm) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[TermsResults]
        response.hits.terms.headOption.getOrElse(fail("Hits are empty")).term should be ("hello")
      }
    }
  }

  it should {
    "return an HTTP code 200 when finding terms for given text input" in {
      Post("/index_getjenny_english_0/term/text", "hello, this is my query") ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[TermsResults]
        response.hits.terms.map(_.term) should contain ("hello")
      }
    }
  }

  it should {
    val terms = List("term1", "term2")
    "return an HTTP code 200 when deleting terms" in {
      val docsIds = DocsIds(terms)
      Post("/index_getjenny_english_0/term/delete?refresh=1", docsIds) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[DeleteDocumentsResult]
        response.data.length should be(docsIds.ids.length)
      }
    }

    "return an HTTP code 200 when searching deleted terms" in {
      for(term <- terms){
        val searchTerm = SearchTerm(term = Some(term))
        Post("/index_getjenny_english_0/term/term", searchTerm) ~> addCredentials(testUserCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[TermsResults]
          response.total shouldBe 0
        }
      }
    }
  }

  it should {
    "return an HTTP code 200 when deleting all terms" in {
      Post("/index_getjenny_english_0/term/delete?refresh=1", DocsIds(Nil)) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[DeleteDocumentsSummaryResult]
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
