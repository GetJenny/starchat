package com.getjenny.starchat.analyzer.atoms.http.custom

import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import com.getjenny.starchat.analyzer.atoms.http.AtomVariableReader.VariableConfiguration
import com.getjenny.starchat.analyzer.atoms.http.{GenericVariableManager, HttpAtomOutputConf}
import scalaz.Scalaz._
import spray.json._

/**
 * Connect to the Efecte APi to create a ticket with given parameters
 *
 * This atom takes variables customer, customerEmail, requestType, subject, description, categoryLevel1, categoryLevel2, categoryLevel3, contactType, unit, supportGroup
 *
 * Only customer is mandatory, all other variables can be left empty.
 *
 * Atom can be used by efecteApi() and inserting given variables in the data field.
 *
 */

trait UHEfecteApiVariableManager extends GenericVariableManager {

  override def configurationPrefix: Option[String] = Some("http-atom.efecte-api")

  override def outputConf(configuration: VariableConfiguration, findProperty: String => Option[String]): AtomValidation[HttpAtomOutputConf] = {
    UHEfecteApiOutput().successNel
  }
}

  case class UHEfecteApiOutput(override val score: String = s"${UHEfecteApiOutput.prefix}.score",
                               EfecteApiStatus: String = s"${UHEfecteApiOutput.prefix}.status") extends HttpAtomOutputConf {
    override def bodyParser(body: String, contentType: String, status: StatusCode): Map[String, String] = {
      if (StatusCodes.OK.equals(status)) {
        Map(
          score -> "1",
          EfecteApiStatus-> status.toString
        )
      } else {
        Map(
          score -> "0",
          EfecteApiStatus -> status.toString
        )
      }
    }

  }

  object UHEfecteApiOutput{
    val prefix = "effecteApiResponse"
  }
