#!/usr/bin/env bash

TERM=${1:-"hello"}
ROUTE=${2:-conversation_logs}
#ROUTE=${2:-knowledgebase}
INDEX_NAME=${3:-index_getjenny_english_0}
FIELD=${4:-"question"}
PORT=${5:-8888}
curl -v -H "Authorization: Basic $(echo -n 'test_user:p4ssw0rd' | base64)" \
  -H "Content-Type: application/json" -X GET "http://localhost:${PORT}/${INDEX_NAME}/term_count/${ROUTE}?field=${FIELD}&term=${TERM}"

