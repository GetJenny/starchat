package com.getjenny.starchat.entities.io

/**
 * Created by Angelo Leto <angelo@getjenny.com> on 17/02/20.
 */

import scalaz.Scalaz._

case class StarChatVariablesException(message: String = "", cause: Throwable = None.orNull)
  extends Exception(message, cause)

object StarChatVariables extends Enumeration {
  val GJ_CONV_FEEDBACK_SCORE, /** Conversation feedback score if present */
  GJ_CONVERSATION_ID, /** Conversation ID */
  GJ_LAST_USER_INPUT_TEXT, /** the last user input */
  GJ_CONVERSATION_JSON, /** Conversation content without line-break */
  GJ_CONVERSATION_V1 = StarChatVariables.Value /** Conversation content: question-answer */

  /** normal document, can be returned to the user as response */
  def value(v: String): StarChatVariables.Value =
    values.find(_.toString === v) match {
      case Some(value) => value
      case _ => throw StarChatVariablesException(s"invalid or unsupported StarChat variable: $v")
    }
}
