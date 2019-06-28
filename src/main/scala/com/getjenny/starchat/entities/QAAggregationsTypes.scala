package com.getjenny.starchat.entities

/**
  * Created by Angelo Leto <angelo@getjenny.com> on 14/03/19.
  */

import scalaz.Scalaz._

object QAAggregationsTypes extends Enumeration {
  val avgFeedbackConvScore, // the average score for the conversations (transferred and non-transferred)
  avgFeedbackAnswerScore, // the average score for all the Answers (transferred and non-transferred)
  avgAlgorithmConvScore, // the average score for the conversations (transferred and non-transferred), the scores are provided by an algorithm
  avgAlgorithmAnswerScore, // the average score for all the Answers (transferred and non-transferred), the scores are provided by an algorithm
  scoreHistogram, // the distribution of scores, an histogram (transferred and non-transferred)
  scoreHistogramNotTransferred, // the distribution of scores for the non-transferred conversations, an histogram
  scoreHistogramTransferred, // the distribution of scores for the transferred conversations, an histogram
  conversationsHistogram, // time range histogram of the number of conversations on a time range (transferred and non-transferred)
  conversationsNotTransferredHistogram, // time range histogram of the number of conversations on a time range (non-transferred only)
  conversationsTransferredHistogram, // time range histogram of the number of conversations on a time range (transferred only)
  qaPairHistogram, // time range histogram of the Q/A documents (only starchat agent, answered + unanswered)
  qaPairAnsweredHistogram, // time range histogram of the number of answered Q/A documents (only starchat agent)
  qaPairUnansweredHistogram, // time range histogram of the number of unanswered Q/A documents (only starchat agent)
  qaMatchedStatesHistogram, // histogram of matched states
  qaMatchedStatesWithScoreHistogram, // histogram of matched states with score
  avgFeedbackNotTransferredConvScoreOverTime, // average score for the conversations (non-transferred only) on each time interval
  avgFeedbackTransferredConvScoreOverTime, // average score for the conversations (transferred only) on each time interval
  avgAlgorithmNotTransferredConvScoreOverTime, // average score for the conversations (non-transferred only) on each time interval, the scores are provided by an algorithm
  avgAlgorithmTransferredConvScoreOverTime, // average score for the conversations (transferred only) on each time interval, the scores are provided by an algorithm
  avgFeedbackConvScoreOverTime, // average score for the conversations (transferred and non-transferred) on each time interval
  avgAlgorithmAnswerScoreOverTime, // average score for the answers (transferred and transferred) on each time interval
  avgFeedbackAnswerScoreOverTime, // average score for the answers (transferred and non-transferred) on each time interval, the scores are provided by an algorithm
  avgAlgorithmConvScoreOverTime, // average score for the answers (transferred and transferred) on each time interval, the scores are provided by an algorithm
  UNKNOWN = QAAggregationsTypes.Value  /** normal document, can be returned to the user as response */
  def value(v: String): QAAggregationsTypes.Value = values.find(_.toString === v).getOrElse(UNKNOWN)
}
