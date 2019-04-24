#!/usr/bin/env bash

PORT=${1:-8888}
INDEX_NAME=${2:-index_getjenny_english_0}
ROUTE=${3:-conversation_logs}
#ROUTE=${3:-knowledgebase}
curl -v -H "Authorization: Basic $(echo -n 'test_user:p4ssw0rd' | base64)" \
  -H "Content-Type: application/json" \
  -X GET http://localhost:${PORT}/${INDEX_NAME}/stream/${ROUTE}
