package com.getjenny.starchat.entities

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 01/07/16.
  */

case class QADocumentAnnotationsSearch(
                                  dclass: Option[String] = None, /* document classes e.g. group0 group1 etc.*/
                                  doctype: Option[List[Doctypes.Value]] = None, /* document type */
                                  state: Option[String] = None, /* eventual link to any of the state machine states or starchat state for logs */
                                  agent: Option[List[Agent.Value]] = None,
                                  escalated: Option[List[Escalated.Value]] = None,
                                  answered: Option[List[Answered.Value]] = None,
                                  triggered: Option[List[Triggered.Value]] = None,
                                  followup: Option[List[Followup.Value]] = None,
                                  feedbackConv: Option[String] = None, /* A feedback provided by the user to the conversation */
                                  feedbackScoreConvGte: Option[Double] = None, /* a field to store the score provided by the user to the conversation */
                                  feedbackScoreConvLte: Option[Double] = None, /* a field to store the score provided by the user to the conversation */
                                  algorithmScoreConvGte: Option[Double] = None, /* a field to store the score calculated by an algorithm related to the conversation i.e. a sentiment analysis tool (for future use) */
                                  algorithmScoreConvLte: Option[Double] = None, /* a field to store the score calculated by an algorithm related to the conversation i.e. a sentiment analysis tool (for future use) */
                                  feedbackScoreAnswerGte: Option[Double] = None, /* description: a field to store the score provided by the user for the answer */
                                  feedbackScoreAnswerLte: Option[Double] = None, /* description: a field to store the score provided by the user for the answer */
                                  algorithmScoreAnswerGte: Option[Double] = None, /* a field to store the score calculated by an algorithm related to the answer i.e. a sentiment analysis tool (for future use) */
                                  algorithmScoreAnswerLte: Option[Double] = None, /* a field to store the score calculated by an algorithm related to the answer i.e. a sentiment analysis tool (for future use) */
                                  responseScoreGte: Option[Double] = None, /* score of the response e.g. Starchat response score */
                                  responseScoreLte: Option[Double] = None, /* score of the response e.g. Starchat response score */
                                  start: Option[Boolean] = None /* event determined when a start state is loaded */
                                )

case class QADocumentSearch(
                             from: Option[Int] = None,
                             size: Option[Int] = None,
                             minScore: Option[Float] = None,
                             sortByConvIdIdx: Option[Boolean] = None,

                             conversation: Option[List[String]] = None, /* IDs of the conversations (or query) */
                             indexInConversation: Option[Int] = None, /* the index of the document in the conversation flow */
                             coreData: Option[QADocumentCore] = None, /* core question answer fields */
                             annotations: Option[QADocumentAnnotationsSearch] = None, /* qa and conversation annotations */
                             status: Option[Int] = None, /* tell whether the document is locked for editing or not, useful for
                                              a GUI to avoid concurrent modifications, 0 means no operations pending */

                             timestampGte: Option[Long] = None, /* min indexing timestamp, None means no lower bound */
                             timestampLte: Option[Long] = None, /* max indexing timestamp, None means no upper bound*/
                             random: Option[Boolean] = None
                           )