package com.getjenny.starchat.analyzer.atoms.http.custom

import akka.http.javadsl.model.StatusCodes
import akka.http.scaladsl.model.{StatusCode}
import com.getjenny.starchat.analyzer.atoms.http.AtomVariableReader._
import com.getjenny.starchat.analyzer.atoms.http._
import scalaz.Scalaz._
import spray.json._


/**
  * Author Henri Vuorinen
 *
 * This analyzer gets data from LIME CRM API. It needs following parameters at least to be able to fetch data; email, customer name and company.
 *
 * This analyzer can be used by calling getLimeData() and inserting in data {"email": "customerEmail", "customer": "name here", "company": "company here"}
  */

trait GetLimeDataVariableManager extends GenericVariableManager {

  override def configurationPrefix: Option[String] = Some("http-atom.get-lime-data")

  override def outputConf(configuration: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[HttpAtomOutputConf] = {
    GetLimeDataOutput().successNel
  }
}

  case class GetLimeDataOutput(override val score: String = s"${GetLimeDataOutput.prefix}.score",
                               customerEmail: String = s"${GetLimeDataOutput.prefix}.email",
                               responseStatus: String = s"${GetLimeDataOutput.prefix}.status") extends HttpAtomOutputConf {
    override def bodyParser(body: String, contentType: String, status: StatusCode): Map[String, String] = {
      if(StatusCodes.OK.equals(status)) {
        val json = body.parseJson
        val data = json match {
          case JsArray(elements) =>
            elements
              .flatMap(_.asJsObject.fields.map {case (e,s) => e -> s.toString})
              .toMap
          case _ => throw new IllegalArgumentException("bad json format")
        }
        Map(
          score -> "1",
          customerEmail -> data.getOrElse("customerEmail", ""),
          responseStatus -> status.toString
        )
      } else {
        Map(
          score -> "0",
          responseStatus -> status.toString
        )
      }
    }
  }


  object GetLimeDataOutput{
    val prefix = "getLimeData"
  }