package com.getjenny.starchat.resources

import akka.http.scaladsl.model.StatusCodes
import com.getjenny.analyzer.expressions.AnalyzersData
import com.getjenny.starchat.TestEnglishBase
import com.getjenny.starchat.entities.io.{AnalyzerEvaluateRequest, AnalyzerEvaluateResponse, Permissions, User}

class AnalyzersPlaygroundResourceTest extends TestEnglishBase {

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
    "return an HTTP code 200 when evaluating a simple vOneKeyword analyzer with an empty query" in {
      val evaluateRequest: AnalyzerEvaluateRequest =
        AnalyzerEvaluateRequest(
          query = "",
          analyzer = """vOneKeyword("test")""",
          data = Option {
            AnalyzersData()
          }
        )

      Post(s"/index_getjenny_english_0/analyzer/playground", evaluateRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[AnalyzerEvaluateResponse]
        response.build should be(true)
        response.buildMessage should be("success")
        response.value should be(0.0)
      }
    }
  }

  it should {
    "return an HTTP code 200 when evaluating a simple vOneKeyword analyzer" in {
      val evaluateRequest: AnalyzerEvaluateRequest =
        AnalyzerEvaluateRequest(
          query = "this is a test",
          analyzer = """vOneKeyword("test")""",
          data = Option {
            AnalyzersData()
          }
        )

      Post(s"/index_getjenny_english_0/analyzer/playground", evaluateRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[AnalyzerEvaluateResponse]
        response.build should be(true)
        response.buildMessage should be("success")
        response.value should be(0.25)
      }
    }
  }

  it should {
    "return an HTTP code 200 when checking if a value exists in the traversed states list" in {
      val evaluateRequest: AnalyzerEvaluateRequest =
        AnalyzerEvaluateRequest(
          query = "query",
          analyzer = """hasTravState("one")""",
          data = Option {
            AnalyzersData(traversedStates = Vector("one", "two"), extractedVariables = Map.empty[String, String])
          }
        )

      Post(s"/index_getjenny_english_0/analyzer/playground", evaluateRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[AnalyzerEvaluateResponse]
        response.build should be(true)
        response.buildMessage should be("success")
        response.value should be(1)
      }
    }
  }

  it should {
    "return an HTTP code 200 when checking if a value does not exists in the traversed states list" in {
      val evaluateRequest: AnalyzerEvaluateRequest =
        AnalyzerEvaluateRequest(
          query = "query",
          analyzer = """bnot(hasTravState("three"))""",
          data = Option {
            AnalyzersData(traversedStates = Vector("one", "two"), extractedVariables = Map.empty[String, String])
          }
        )

      Post(s"/index_getjenny_english_0/analyzer/playground", evaluateRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[AnalyzerEvaluateResponse]
        response.build should be(true)
        response.buildMessage should be("success")
        response.value should be(1)
      }
    }
  }

  it should {
    "return an HTTP code 200 when checking if the last value of the traversed states is correct" in {
      val evaluateRequest: AnalyzerEvaluateRequest =
        AnalyzerEvaluateRequest(
          query = "query",
          analyzer = """lastTravStateIs("two")""",
          data = Option {
            AnalyzersData(traversedStates = Vector("one", "two"), extractedVariables = Map.empty[String, String])
          }
        )

      Post(s"/index_getjenny_english_0/analyzer/playground", evaluateRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[AnalyzerEvaluateResponse]
        response.build should be(true)
        response.buildMessage should be("success")
        response.value should be(1)
      }
    }
  }

  it should {
    "return an HTTP code 200 when checking if the previous value of the traversed states is correct" in {
      val evaluateRequest: AnalyzerEvaluateRequest =
        AnalyzerEvaluateRequest(
          query = "query",
          analyzer = """prevTravStateIs("one")""",
          data = Option {
            AnalyzersData(traversedStates = Vector("one", "two"), extractedVariables = Map.empty[String, String])
          }
        )

      Post(s"/index_getjenny_english_0/analyzer/playground", evaluateRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[AnalyzerEvaluateResponse]
        response.build should be(true)
        response.buildMessage should be("success")
        response.value should be(1)
      }
    }
  }

  it should {
    "return an HTTP code 200 when checking the variable extraction analyzer" in {
      val evaluateRequest: AnalyzerEvaluateRequest =
        AnalyzerEvaluateRequest(
          query = "on 31-11-1900",
          analyzer =
            """band(prevTravStateIs("one"),binarize(vOneKeyword("on")),matchPatternRegex("[day,month,year](?:(0[1-9]|[12][0-9]|3[01])(?:[- \/\.])(0[1-9]|1[012])(?:[- \/\.])((?:19|20)\d\d))"))""",
          data = Option {
            AnalyzersData(traversedStates = Vector("one", "two"),
              extractedVariables = Map.empty[String, String])
          }
        )

      Post(s"/index_getjenny_english_0/analyzer/playground", evaluateRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[AnalyzerEvaluateResponse]
        response.build should be(true)
        response.buildMessage should be("success")
        response.value should be(1.0)
        response.data.nonEmpty should be(true)
        response.data.getOrElse(AnalyzersData()).extractedVariables.exists(_ == ("month.0", "11")) should be(true)
        response.data.getOrElse(AnalyzersData()).extractedVariables.exists(_ == ("day.0", "31")) should be(true)
        response.data.getOrElse(AnalyzersData()).extractedVariables.exists(_ == ("year.0", "1900")) should be(true)
      }
    }
  }

  it should {
    "return an HTTP code 200 when checking if an extracted variable exists" in {
      val evaluateRequest: AnalyzerEvaluateRequest =
        AnalyzerEvaluateRequest(
          query = "",
          analyzer =
            """existsVariable("month.0")""",
          data = Option {
            AnalyzersData(traversedStates = Vector("one", "two"),
              extractedVariables =
                Map[String, String](
                  "month.0" -> "11",
                  "day.0" -> "31",
                  "year.0" -> "1900"))
          }
        )

      Post(s"/index_getjenny_english_0/analyzer/playground", evaluateRequest) ~>
        addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[AnalyzerEvaluateResponse]
        response.build should be(true)
        response.buildMessage should be("success")
        response.value should be(1)
      }
    }
  }

  it should {
    "return an HTTP code 200 when checking if the traversed states has the state in the position from the first" in {
      val evaluateRequest: AnalyzerEvaluateRequest =
        AnalyzerEvaluateRequest(
          query = "",
          analyzer =
            """hasTravStateInPosition("two","2")""",
          data = Option {
            AnalyzersData(traversedStates = Vector("one", "two", "three", "four", "five"))
          }
        )
      val evaluateRequest2: AnalyzerEvaluateRequest =
        AnalyzerEvaluateRequest(
          query = "",
          analyzer =
            """hasTravStateInPosition("one", "0")""",
          data = Option {
            AnalyzersData(traversedStates = Vector("one", "two", "three", "four", "five"))
          }
        )
      Post(s"/index_getjenny_english_0/analyzer/playground", evaluateRequest) ~>
        addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[AnalyzerEvaluateResponse]
        response.build should be(true)
        response.buildMessage should be("success")
        response.value should be(1)
      }
      Post(s"/index_getjenny_english_0/analyzer/playground", evaluateRequest2) ~>
        addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[AnalyzerEvaluateResponse]
        response.build should be(true)
        response.buildMessage should be("success")
        response.value should be(0)
      }
    }
  }

  it should {
    "return an HTTP code 200 when checking if the traversed states has the state in the position from the last" in {
      val evaluateRequest: AnalyzerEvaluateRequest =
        AnalyzerEvaluateRequest(
          query = "",
          analyzer =
            """hasTravStateInPositionRev("four","2")""",
          data = Option {
            AnalyzersData(traversedStates = Vector("one", "two", "three", "four", "five"))
          }
        )
      val evaluateRequest2: AnalyzerEvaluateRequest =
        AnalyzerEvaluateRequest(
          query = "",
          analyzer =
            """hasTravStateInPositionRev("one", "1")""",
          data = Option {
            AnalyzersData(traversedStates = Vector("one", "two", "three", "four", "five"))
          }
        )
      Post(s"/index_getjenny_english_0/analyzer/playground", evaluateRequest) ~>
        addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[AnalyzerEvaluateResponse]
        response.build should be(true)
        response.buildMessage should be("success")
        response.value should be(1)
      }
      Post(s"/index_getjenny_english_0/analyzer/playground", evaluateRequest2) ~>
        addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[AnalyzerEvaluateResponse]
        response.build should be(true)
        response.buildMessage should be("success")
        response.value should be(0)
      }
    }
  }

  it should {
    "return an HTTP code 200 when guessing the language" in {
      val evaluateRequest: AnalyzerEvaluateRequest =
        AnalyzerEvaluateRequest(
          query = "guess the language of this sentence",
          analyzer = """languageGuesser("lang", "0.8", ["et", "fi", "en"])""",
          data = Option {
            AnalyzersData(
              traversedStates = Vector("one", "two")
            )
          }
        )
      Post(s"/index_getjenny_english_0/analyzer/playground", evaluateRequest) ~> addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[AnalyzerEvaluateResponse]
        response.data.getOrElse(fail).traversedStates should be(Vector("one", "two"))
        response.data.getOrElse(fail).extractedVariables("lang") should be("en")
        response.value should be(1)
      }

      Post(s"/index_getjenny_english_0/analyzer/playground", evaluateRequest.copy(
        analyzer = """languageGuesser("guessed_language", "0.8")""")) ~>
        addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[AnalyzerEvaluateResponse]
        response.data.getOrElse(fail).traversedStates should be(Vector("one", "two"))
        response.data.getOrElse(fail).extractedVariables("guessed_language") should be("en")
        response.value should be(1)
      }

      Post(s"/index_getjenny_english_0/analyzer/playground", evaluateRequest.copy(
        query = "indovina la lingua di questa frase")) ~>
        addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[AnalyzerEvaluateResponse]
        response.data.getOrElse(fail).traversedStates should be(Vector("one", "two"))
        response.data.getOrElse(fail).extractedVariables("lang") should be("it")
        response.value should be(0)
      }

      Post(s"/index_getjenny_english_0/analyzer/playground", evaluateRequest.copy(
        analyzer = """languageGuesser("lng", "1")""")) ~>
        addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[AnalyzerEvaluateResponse]
        response.data.getOrElse(fail).traversedStates should be(Vector("one", "two"))
        response.data.getOrElse(fail).extractedVariables("lng") should be("en")
        response.value should be(0)
      }


    }

    /*"call weather open api an return weather info" in {
      val evaluateRequest: AnalyzerEvaluateRequest =
        AnalyzerEvaluateRequest(
          query = "",
          analyzer = """weather("location=torino", "units=metric")""",
          data = Option {
            AnalyzersData(
              traversedStates = Vector("one", "two")
            )
          }
        )

      Post(s"/index_getjenny_english_0/analyzer/playground", evaluateRequest) ~>
        addCredentials(testUserCredentials) ~> routes ~> check {
        status shouldEqual StatusCodes.OK
        val response = responseAs[AnalyzerEvaluateResponse]
        response.value shouldEqual 1.0
        response.data should not be empty
      }
    }*/

  }

}


