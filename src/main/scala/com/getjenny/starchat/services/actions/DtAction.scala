package com.getjenny.starchat.services.actions

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 26/04/19.
  */

import akka.event.{Logging, LoggingAdapter}
import com.getjenny.starchat.SCActorSystem
import com.getjenny.starchat.entities.DtActionResult

case class DtActionException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)

trait DtAction {
  protected val log: LoggingAdapter = Logging(SCActorSystem.system, this.getClass.getCanonicalName)
  def apply(indexName: String, stateName: String, params: Map[String, String]): DtActionResult
}

object DtAction {
  val actionPrefix = "com.getjenny.starchat.actions."
  def apply(indexName: String, stateName: String, action: String, params: Map[String, String]): DtActionResult = {
    action match {
      case "com.getjenny.starchat.actions.SendEmailSmtp"  => SendEmailSmtp(indexName, stateName, params)
      case "com.getjenny.starchat.actions.SendEmailGJ"  => SendEmailGJ(indexName, stateName, params)
      case _ => throw DtActionException("Action not implemented: " + action)
    }
  }
}
