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

class ClusterNodesResourceTest extends WordSpec with Matchers with ScalatestRouteTest with JsonSupport {
  implicit def default(implicit system: ActorSystem) = RouteTestTimeout(10.seconds.dilated(system))

  val service = TestFixtures.service
  val routes = service.routes

  val testAdminCredentials = BasicHttpCredentials("admin", "adminp4ssw0rd")

  "StarChat" should {
    "return an HTTP code 201 when creating system_index.cluster_nodes" in {
      Post(s"/system_index_management/create?indexSuffix=cluster_nodes") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.Created
        val response = responseAs[IndexManagementResponse]
          "(?:[A-Za-z0-9_]+)\\(" + Index.systemIndexMatchRegex + "\\.(?:[A-Za-z0-9_]+), true\\)".r
      }
    }
  }

  it should {
    "return an HTTP code 200 when updating alive timestamp for the node" in {
      Post(s"/cluster_node") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[ClusterNode]
        response.alive shouldBe true
      }
    }
  }

  it should {
    "return an HTTP code 200 when retrieving information about a node" in {
      Post(s"/cluster_node") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        val response = responseAs[ClusterNode]
        response.uuid
      }.andThen(uuid => {
        Get(s"/cluster_node/$uuid") ~>  addCredentials(testAdminCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[ClusterNode]
          response.uuid shouldEqual uuid
          response.alive shouldBe true
        }
      })
    }
  }

  it should {
    "return an HTTP code 200 when getting alive nodes" in {
      Get(s"/cluster_node") ~>  addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[ClusterNodes]
      }
    }
  }

  it should {
    "return an HTTP code 200 when removing dead nodes" in {
      Delete(s"/cluster_node") ~>  addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[DeleteDocumentsSummaryResult]
        response.message shouldEqual "delete"
      }
    }
  }

  it should {
    "return an HTTP code 200 when deleting an existing system_index.cluster_nodes" in {
      Delete(s"/system_index_management?indexSuffix=cluster_nodes") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[IndexManagementResponse]
      }
    }
  }
}
