#!/usr/bin/env bash

QUERY=${1:-"can"}
PORT=${2:-8888}
INDEX_NAME=${3:-index_getjenny_english_0}
ALGORITHM=${4:-NONE}
curl -v -H "Authorization: Basic $(echo -n 'admin:adminp4ssw0rd' | base64)" \
 -H "Content-Type: application/json" -X POST http://localhost:${PORT}/${INDEX_NAME}/autocomplete -d "{
	\"userText\": \"${QUERY}\",
	\"threshold\": 0.0,
	\"maxResults\": 4,
	\"suggesterType\": \"DEFAULT\",
	\"sortAlgorithm\": \"${ALGORITHM}\",
	\"suggestionCategories\": [\"VALID\",\"NOT_VALIDATED\"]
}"
