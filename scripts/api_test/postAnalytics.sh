#!/usr/bin/env bash

PORT=${1:-8888}
INDEX_NAME=${2:-index_getjenny_english_0}
#ROUTE=${3:-prior_data}
ROUTE=${3:-conversation_logs}
#ROUTE=${3:-knowledgebase}

curl -v -H "Authorization: Basic $(echo -n 'admin:adminp4ssw0rd' | base64)" \
 -X POST -H "Content-Type: application/json" -X POST "http://localhost:${PORT}/${INDEX_NAME}/analytics/${ROUTE}" -d "
{
  \"interval\": \"1M\",
  \"aggregations\": [
    \"avgFeedbackConvScore\",
    \"avgFeedbackAnswerScore\",
    \"avgAlgorithmConvScore\",
    \"avgAlgorithmAnswerScore\",
    \"scoreHistogram\",
    \"scoreHistogramNotTransferred\",
    \"scoreHistogramTransferred\",
    \"conversationsHistogram\",
    \"conversationsNotTransferredHistogram\",
    \"conversationsTransferredHistogram\",
    \"qaPairHistogram\",
    \"qaPairAnsweredHistogram\",
    \"qaPairUnansweredHistogram\",
    \"qaMatchedStatesHistogram\",
    \"qaMatchedStatesWithScoreHistogram\",
    \"avgFeedbackNotTransferredConvScoreOverTime\",
    \"avgFeedbackTransferredConvScoreOverTime\",
    \"avgAlgorithmNotTransferredConvScoreOverTime\",
    \"avgAlgorithmTransferredConvScoreOverTime\",
    \"avgFeedbackConvScoreOverTime\",
    \"avgAlgorithmAnswerScoreOverTime\",
    \"avgFeedbackAnswerScoreOverTime\",
    \"avgAlgorithmConvScoreOverTime\"
  ],
  \"minDocInBuckets\": 0
}
"

