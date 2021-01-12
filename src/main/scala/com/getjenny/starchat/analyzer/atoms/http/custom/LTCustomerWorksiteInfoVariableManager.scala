package com.getjenny.starchat.analyzer.atoms.http.custom

import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import com.getjenny.starchat.analyzer.atoms.http.AtomVariableReader.VariableConfiguration
import com.getjenny.starchat.analyzer.atoms.http.{GenericVariableManager, HttpAtomInputConf, HttpAtomOutputConf}
import scalaz.Scalaz._
import spray.json._

/**
 * Author Henri Vuorinen
 * requires botVariable customerWorksiteNo.result to make the GET request.
 * This variable is set in the request url and it will give in response lot of different values.
 * For this atom to works LTCustomerInfoVariableManager needs to be run first
 *
 */

trait LTCustomerWorksiteInfoVariableManager extends GenericVariableManager {

  override def configurationPrefix: Option[String] = Some("http-atom.ltCustomerWorksiteNo")

  //override def inputConf(configMap: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[Option[HttpAtomInputConf]] = {
//
  //
  //}

  override def outputConf(configuration: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[HttpAtomOutputConf] = {
    LTCustomerWorksiteInfoOutput().successNel
  }
}

case class LTCustomerWorksiteInfoOutput(override val score: String = s"${LTCustomerWorksiteInfoOutput.prefix}.score"
                                       ) extends HttpAtomOutputConf {
  override def bodyParser(body: String, contentType: String, status: StatusCode): Map[String, String] = {
    if(StatusCodes.OK.equals(status)) {
      val json = body.parseJson
      json match{
        case JsArray(elements) =>
          elements
          .flatMap(_.asJsObject.fields.map {case (a,b) => s"${LTCustomerWorksiteInfoOutput.prefix}.$a" -> b.toString})
          .toMap
        case _ => throw new IllegalArgumentException("bad Json or Bad request")
      }
    } else {
      Map(
        score -> "0",
        s"${LTCustomerWorksiteInfoOutput.prefix}.status" -> status.toString
      )
    }
  }

}

object LTCustomerWorksiteInfoOutput{
  val prefix = "ltCustomerWorksiteInfo"
}
