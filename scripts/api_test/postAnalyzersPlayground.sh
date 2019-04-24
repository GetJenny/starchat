#!/usr/bin/env bash

ANALYZER=${1:-"keyword(\\\"test\\\")"}
QUERY=${2:-"this is a test"}
DATA=${3:-"{\"traversedStates\": [], \"extractedVariables\":{}}"}
PORT=${4:-8888}
INDEX_NAME=${5:-index_getjenny_english_0}
ALGORITHM=${6:-AUTO}
curl -H "Authorization: Basic $(echo -n 'admin:adminp4ssw0rd' | base64)" \
  -H "Content-Type: application/json" -X POST "http://localhost:${PORT}/${INDEX_NAME}/analyzer/playground" -d "
{
	\"analyzer\": \"${ANALYZER}\",
	\"query\": \"${QUERY}\",
	\"data\": ${DATA},
	\"searchAlgorithm\": \"${ALGORITHM}\"
}
"

