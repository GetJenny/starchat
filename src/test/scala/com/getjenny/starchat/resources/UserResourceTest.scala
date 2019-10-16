package com.getjenny.starchat.resources

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.headers.BasicHttpCredentials
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import akka.testkit._
import com.getjenny.starchat.StarChatService
import com.getjenny.starchat.entities._
import com.getjenny.starchat.serializers.JsonSupport
import com.getjenny.starchat.utils.Index
import org.scalatest.{Matchers, WordSpec}

import scala.concurrent.duration._

class UserResourceTest extends WordSpec with Matchers with ScalatestRouteTest with JsonSupport {
  implicit def default(implicit system: ActorSystem) = RouteTestTimeout(10.seconds.dilated(system))

  val service: StarChatService = TestFixtures.service
  val routes: Route = service.routes

  val testAdminCredentials = BasicHttpCredentials("admin", "adminp4ssw0rd")
  val userCredentials = BasicHttpCredentials("AzureDiamond", "hunter3")

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
    "return an HTTP code 200 when generating a user" in {
      val user = UserUpdate(
        id = "AzureDiamond",
        password = None,
        salt = None,
        permissions = None
      )
      Post("/user/generate", user) ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[User]
        response.id should be ("AzureDiamond")
        response.password.length should be (128)
        response.salt.length should be (16)
        response.permissions should be (Map.empty)
      }
    }
  }

  it should {
    "return an HTTP code 201 when creating a new user" in {
      val user = UserUpdate(
        id = "AzureDiamond",
        password = Some("hunter2"),
        salt = Some("salt"),
        permissions = None
      )
      Post("/user/generate", user) ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[User]
      }.andThen( newUser =>
        Post("/user", newUser) ~> addCredentials(testAdminCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.Created
          newUser
      }).andThen( createdUser =>
        Post("/user/get", UserId(user.id)) ~> addCredentials(testAdminCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[User]
          response shouldEqual createdUser
      })
    }
  }

  it should {
    "return an HTTP code 200 when updating a user" in {
      val userUpdate = UserUpdate(
        id = "AzureDiamond",
        password = Some("hunter3"),
        salt = Some("smoked salt"),
        permissions = Some(Map("index_getjenny_english_0" -> Set(Permissions.read, Permissions.write)))
      )
      Post("/user/generate", userUpdate) ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[UserUpdate]
      }.andThen( updateUser =>
        Put("/user", updateUser) ~> addCredentials(testAdminCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[UpdateDocumentResult]
          response.created should be (false)
          response.id should be (updateUser.id)
          response.index should be ("starchat_system_0.user")
          response.version should be (2)
          updateUser
      }).andThen( updatedUser =>
        Post("/user/get", UserId(userUpdate.id)) ~> addCredentials(testAdminCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.OK
          val response = responseAs[UserUpdate]
          response shouldEqual updatedUser
      })
    }
  }

  it should {
    "return an HTTP code 403 when not having admin permissions" in {
      val userUpdate = UserUpdate(
        id = "AzureDiamond",
        password = Some("hunter3"),
        salt = Some("smoked salt"),
        permissions = Some(Map("index_getjenny_english_0" -> Set(
          Permissions.read, Permissions.write, Permissions.admin, Permissions.stream)))
      )
      Post("/user/generate", userUpdate) ~> addCredentials(userCredentials) ~> Route.seal(routes) ~> check {
        status shouldEqual StatusCodes.Forbidden
      }
      Put("/user", userUpdate) ~> addCredentials(userCredentials) ~> Route.seal(routes) ~> check {
        status shouldEqual StatusCodes.Forbidden
      }
      Post("/user/get", UserId(userUpdate.id)) ~> addCredentials(userCredentials) ~> Route.seal(routes) ~> check {
        status shouldEqual StatusCodes.Forbidden
      }
      Post("/user/delete", UserId(userUpdate.id)) ~> addCredentials(userCredentials) ~> Route.seal(routes) ~> check {
        status shouldEqual StatusCodes.Forbidden
      }
    }
  }

  it should {
    "return an HTTP code 401 when unauthorized" in {
      val fakeCredentials = BasicHttpCredentials("admin", "admin")
      val userUpdate = UserUpdate(
        id = "admin",
        password = Some("admin"),
        salt = Some("smelly salt"),
        permissions = Some(Map("index_getjenny_english_0" -> Set(
          Permissions.read, Permissions.write, Permissions.admin, Permissions.stream)))
      )
      Post("/user/generate", userUpdate) ~> addCredentials(fakeCredentials) ~> Route.seal(routes) ~> check {
        status shouldEqual StatusCodes.Unauthorized
      }
      Put("/user", userUpdate) ~> addCredentials(fakeCredentials) ~> Route.seal(routes) ~> check {
        status shouldEqual StatusCodes.Unauthorized
      }
      Post("/user/get", UserId(userUpdate.id)) ~> addCredentials(fakeCredentials) ~> Route.seal(routes) ~> check {
        status shouldEqual StatusCodes.Unauthorized
      }
      Post("/user/delete", UserId("admin")) ~> addCredentials(fakeCredentials) ~> Route.seal(routes) ~> check {
        status shouldEqual StatusCodes.Unauthorized
      }
    }
  }

  it should {
    "return an HTTP code 200 when deleting an existing user" in {
      val userId = UserId("AzureDiamond")
      Post("/user/delete", userId) ~> addCredentials(testAdminCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[DeleteDocumentResult]
        response.found should be (true)
        response.id should be (userId.id)
        response.version should be (3)
      }.andThen( _ =>
        Post("/user/get", userId) ~> addCredentials(testAdminCredentials) ~> routes ~> check {
          status shouldEqual StatusCodes.Unauthorized
        }
      )
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
