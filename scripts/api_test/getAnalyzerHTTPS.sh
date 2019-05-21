#!/usr/bin/env bash

PORT=${1:-8443}
INDEX_NAME=${2:-index_getjenny_english_0}
curl --insecure -v -H "Authorization: Basic $(echo -n 'test_user:p4ssw0rd' | base64)" \
  -H "Content-Type: application/json" -X GET "https://localhost:${PORT}/${INDEX_NAME}/decisiontable/analyzer"

