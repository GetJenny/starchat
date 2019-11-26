package com.getjenny.starchat.resources

import akka.http.scaladsl.model.StatusCodes
import com.getjenny.starchat.entities._
import com.getjenny.starchat.entities.io.CreateLanguageIndexRequest


trait TestEnglishBase extends TestBase {
  val createEnglishRequest = CreateLanguageIndexRequest(List("english"))

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    Post("/language_index_management", createEnglishRequest) ~> addCredentials(testAdminCredentials) ~> routes ~> check {
      true
    }
    Post(s"/index_getjenny_english_0/index_management/create") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
      true
    }
    Post(s"/index_getjenny_english_common_0/index_management/create") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
      true
    }
  }

  override protected def afterAll(): Unit = {
    Delete(s"/language_index_management?index_name=index_english") ~> addCredentials(testAdminCredentials) ~> routes ~> check {
      true
    }
    super.afterAll()
  }

}
