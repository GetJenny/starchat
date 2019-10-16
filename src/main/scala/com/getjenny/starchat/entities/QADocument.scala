package com.getjenny.starchat.entities

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 01/07/16.
  */

import scalaz.Scalaz._

object Doctypes extends Enumeration {
  val CANNED,

  /** canned document, indexed but retrieved only under particular circumstances */
  HIDDEN,

  /** hidden document, these are indexed but must not be retrieved,
    * use this type for data used just to improve statistic for data retrieval */
  DECISIONTABLE,

  /** does not contains conversation data, used to redirect the
    * conversation to any state of the decision tree */
  METADATA,

  /** used for metadata e.g. conversation medatada */
  NORMAL = Doctypes.Value

  /** normal document, can be returned to the user as response */
  def value(v: String): Doctypes.Value = values.find(_.toString === v).getOrElse(NORMAL)
}

object Agent extends Enumeration {
  val HUMAN_REPLY,

  /** Answer provided by an agent, must be used when the conversation
    * has been escalated and the platform (a human is carrying on the conversation) and is not possible
    * to discriminate between HUMAN_PICKED and HUMAN_REPLY.
    */
  HUMAN_PICKED,

  /** When an agent chooses and answer suggestion provided by smartLayer */
  STARCHAT,

  /** The answer was provided by the connector */
  CONNECTOR,

  /** the answer was provided by StarChat */
  UNSPECIFIED = Agent.Value

  /** when the information is unset/not applicable */
  def value(v: String): Agent.Value = values.find(_.toString === v).getOrElse(UNSPECIFIED)
}

object Escalated extends Enumeration {
  val TRANSFERRED,

  /** when the conversation is being transferred to the customer care */
  UNSPECIFIED = Escalated.Value

  /** usually in the middle of a conversation this value is not set,
    * it is known at the end of the conversation or when the user requests to escalate. */
  def value(v: String): Escalated.Value = values.find(_.toString === v).getOrElse(UNSPECIFIED)
}

object Answered extends Enumeration {
  val ANSWERED,

  /** the answer was provided */
  UNANSWERED,

  /** Question for which no answer was provided i.e. StarChat returns empty list or 404 or the agent didn’t answer */
  UNSPECIFIED = Answered.Value

  /** the information is not applicable */
  def value(v: String): Answered.Value = values.find(_.toString === v).getOrElse(UNSPECIFIED)
}

object Triggered extends Enumeration {
  val BUTTON,

  /** the answer was triggered by a button */
  ACTION,

  /** the answer was triggered by an action */
  UNSPECIFIED = Triggered.Value

  /** usually this information is not applicable except in the other two cases mentioned before. */
  def value(v: String): Triggered.Value = values.find(_.toString === v).getOrElse(UNSPECIFIED)
}

object Followup extends Enumeration {
  val FOLLOWUP,

  /** follow up */
  FOLLOWUP_BY_TIME,

  /** follow up dependant on the time of the day */
  UNSPECIFIED = Followup.Value

  /** not applicable */
  def value(v: String): Followup.Value = values.find(_.toString === v).getOrElse(UNSPECIFIED)
}


case class QADocumentCore(
                           question: Option[String] = None, /* usually what the user of the chat says */
                           questionNegative: Option[List[String]] = None, /* list of sentences different to the main question */
                           questionScoredTerms: Option[List[(String, Double)]] = None, /* terms list in form {"term": "<term>", "score": 0.2121} */
                           answer: Option[String] = None, /* usually what the operator of the chat says */
                           answerScoredTerms: Option[List[(String, Double)]] = None, /* terms list in form {"term": "<term>", "score": 0.2121} */
                           topics: Option[String] = None, /* list of topics */
                           verified: Option[Boolean] = None, /* was the conversation verified by an operator? */
                           done: Option[Boolean] = None /* mark the conversation as done, this field is expected to set once for each conversation */
                         )

case class QADocumentAnnotations(
                                  dclass: Option[String] = None, /* document classes e.g. group0 group1 etc.*/
                                  doctype: Option[Doctypes.Value] = None, /* document type */
                                  state: Option[String] = None, /* eventual link to any of the state machine states or starchat state for logs */
                                  agent: Option[Agent.Value] = None,
                                  escalated: Option[Escalated.Value] = None,
                                  answered: Option[Answered.Value] = None,
                                  triggered: Option[Triggered.Value] = None,
                                  followup: Option[Followup.Value] = None,
                                  feedbackConv: Option[String] = None, /* A feedback provided by the user to the conversation */
                                  feedbackConvScore: Option[Double] = None, /* a field to store the score provided by the user to the conversation */
                                  algorithmConvScore: Option[Double] = None, /* a field to store the score calculated by an algorithm related to the conversation i.e. a sentiment
analysis tool (for future use) */
                                  feedbackAnswerScore: Option[Double] = None, /* description: a field to store the score provided by the user for the answer */
                                  algorithmAnswerScore: Option[Double] = None, /* a field to store the score calculated by an algorithm related to the answer i.e. a sentiment
analysis tool (for future use) */
                                  responseScore: Option[Double] = None, /* score of the response e.g. Starchat response score */
                                  start: Option[Boolean] = None /* event determined when a start state is loaded */
                                )

case class QADocument(id: String, /* unique id of the document */
                      conversation: String, /* ID of the conversation (multiple q&a may be inside a conversation) */
                      indexInConversation: Int = -1, /* the index of the document in the conversation flow */
                      coreData: Option[QADocumentCore] = None, /* core question answer fields */
                      annotations: Option[QADocumentAnnotations] = Some(QADocumentAnnotations()), /* qa and conversation annotations */
                      status: Option[Int] = Some(0), /* tell whether the document is locked for editing or not, useful for
                                              a GUI to avoid concurrent modifications, 0 means no operations pending */
                      timestamp: Option[Long] = None /* indexing timestamp, automatically calculated if not provided */
                     )