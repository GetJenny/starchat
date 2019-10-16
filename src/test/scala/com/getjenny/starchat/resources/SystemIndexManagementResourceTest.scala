package com.getjenny.starchat.resources

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.headers.BasicHttpCredentials
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import akka.testkit._
import com.getjenny.starchat.entities._
import com.getjenny.starchat.serializers.JsonSupport
import com.getjenny.starchat.utils.Index
import org.scalatest.{Matchers, WordSpec}

import scala.concurrent.duration._
class SystemIndexManagementResourceTest extends WordSpec with Matchers with ScalatestRouteTest with JsonSupport {
  implicit def default(implicit system: ActorSystem) = RouteTestTimeout(10.seconds.dilated(system))

  val service = TestFixtures.service
  val routes = service.routes
  val sealedRoutes = Route.seal(routes)

  val testAdminCredentials = BasicHttpCredentials("admin", "adminp4ssw0rd")
  val testUserCredentials = BasicHttpCredentials("test_user", "p4ssw0rd")

  val suffixes = Seq("user", "refresh_decisiontable", "cluster_nodes", "decision_table_node_status")

  "StarChat" should {
    "return an HTTP code 400 when getting system indices that don't exist" in {
      Get(s"/system_index_management") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
        val response = responseAs[IndexManagementResponse]
        response.check shouldBe false
      }
    }
  }

  it should {
    for(suffix <- suffixes) {
      s"return an HTTP code 201 when creating system_index.$suffix" in {
        Post(s"/system_index_management/create?indexSuffix=$suffix") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.Created
          val response = responseAs[IndexManagementResponse]
          response.message should fullyMatch regex "IndexCreation: " +
            "(?:[A-Za-z0-9_]+)\\(" + Index.systemIndexMatchRegex + "\\.(?:[A-Za-z0-9_]+), true\\)"
        }
      }

      s"return HTTP code 200 refreshing system_index.$suffix" in {
        Post(s"/system_index_management/refresh?indexSuffix=$suffix") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[RefreshIndexResults]
        }
      }
    }

    "return HTTP code 400 when an operation is not supported" in {
      Post(s"/system_index_management/operation") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.BadRequest
        val response = responseAs[IndexManagementResponse]
      }
    }

  }

  it should {
    "return an HTTP code 200 getting system indices" in {
      Get(s"/system_indices") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[List[String]]
        response.length should be (4)
      }
    }
  }

  it should {
    "return 200 when getting system indices" in {
      Get(s"/system_index_management") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[IndexManagementResponse]
        response.check shouldEqual true
      }
    }
  }

  it should {
    for(suffix <- suffixes) {
      s"return HTTP code 200 when deleting system_index.$suffix" in {
        Delete(s"/system_index_management?indexSuffix=$suffix") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[IndexManagementResponse]
        }
      }
    }
  }

  it should {
    "return HTTP code 400 when indices don't exist" in {
      Delete(s"/system_index_management") ~> addCredentials(testAdminCredentials) ~> sealedRoutes ~> check {
        status shouldEqual StatusCodes.BadRequest
        val response = responseAs[IndexManagementResponse]
      }
    }
  }
}
