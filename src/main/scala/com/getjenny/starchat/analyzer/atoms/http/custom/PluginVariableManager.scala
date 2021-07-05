package com.getjenny.starchat.analyzer.atoms.http.custom

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.{ContentTypes, HttpMethods, StatusCode, StatusCodes}
import com.getjenny.starchat.analyzer.atoms.http._
import com.getjenny.starchat.entities.io.PluginInputBody
import scalaz.Scalaz._
import spray.json.DefaultJsonProtocol.DoubleJsonFormat
import spray.json._

import scala.util.matching.Regex


/**
 * The plugin will be called this way:
 *
 * plugin("app_id=...", "url=...", "key=...", "user_email=user@example.com")
 *
 * Where:
 *
 * app_id: is the configurator-generated and computer-friendly ID (e.g. `my_organization.email_checker`) which will also be put as prefix of each variable.
 * url: the url where the service developed by the organization's developer was made available example.com:8888)
 * key: the service must be accessible with -H "Authorization: Basic <API_KEY>"
 * user_email is a parameter for which the configurator's mask has all info (type, if it's mandatory etc).
 *
 *
 */
trait PluginVariableManager extends GenericVariableManager with SprayJsonSupport with DefaultJsonProtocol {


  override def validateAndBuild(arguments: List[String],
                                restrictedArgs: Map[String, String],
                                extractedVariables: Map[String, String],
                                query: String): AtomValidation[HttpRequestAtomicConfiguration] = {
    val allVariables = arguments ++ confParamsList
    // Map with key value of the arguments
    val argumentConfiguration = createArgumentConfiguration(allVariables)

    val appId = argumentConfiguration.get("app_id") match {
      case Some(t) => t
      case _ => throw VariableManagerException("appId parameter is mandatory.")
    }

    val url = argumentConfiguration.get("url") match {
      case Some(t) => t
      case _ => throw VariableManagerException("url parameter is mandatory.")
    }

    val key = argumentConfiguration.get("key") match {
      case Some(t) => t
      case _ => throw VariableManagerException("key parameter is mandatory.")
    }

    val fixedParamsRegex: Regex = "^app_id|url|key$".r
    //This goes to the service
    val inputBody = PluginInputBody(
      query = query,
      parameters = argumentConfiguration.filterNot{case(k, _) => fixedParamsRegex.findAllIn(k).nonEmpty}
    )

    val serializer = jsonFormat2(PluginInputBody)
    val jsonBody = inputBody.toJson(serializer).toString

    HttpRequestAtomicConfiguration(
      urlConf = HttpAtomUrlConf(url = url,
        method = HttpMethods.POST,
        contentType =  ContentTypes.`application/json`),
      auth =  ApiKeyAuth(key = "Authorization", token = key, storeTo = StoreOption.HEADER).some,
      inputConf = JsonConf(json = jsonBody).some,
      outputConf = outputConf(appId)
    ).successNel
  }

  val factoryName = s"plugin"

  def outputConf(appId: String): HttpAtomOutputConf = {
    PluginOutput(score = s"$appId.score", prefix = s"""$appId""")
  }

}

case class PluginOutput(override val score: String, prefix: String) extends HttpAtomOutputConf {

  override def bodyParser(body: String, contentType: String, status: StatusCode): Map[String, String] = {
    if (StatusCodes.OK.equals(status) || StatusCodes.Accepted.equals(status)) {
      val json = body.parseJson.asJsObject
      val jsonMap = json.fields
      val extractedJsonProperties = jsonMap.keys.mkString(", ")
      val outputJsonMap = jsonMap.map {
        case (k,v) => s"$prefix.$k" -> v.toString.stripPrefix("\"").stripSuffix("\"")
      }
      val appScore: Int = jsonMap.get("score") match {
        case Some(v) => v.convertTo[Double].toInt
        case _ => 0
      }
      Map(
        score -> appScore.toString,
        s"$prefix.status" -> status.toString,
        s"$prefix.properties" -> extractedJsonProperties
      ) ++ outputJsonMap
    } else {
      Map(
        score -> "0",
        s"$prefix.status" -> status.toString
      )
    }
  }
}
