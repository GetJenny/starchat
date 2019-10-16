#!/usr/bin/env bash

PORT=${1:-8888}
INDEX_NAME=${1:-index_getjenny_english_0}

curl -v -H "Authorization: Basic $(echo -n 'admin:adminp4ssw0rd' | base64)" \
  -H "Content-Type: application/json" -X POST "http://localhost:${PORT}/node_dt_update" -d "{
  \"index\": \"INDEX_NAME\"
}"

