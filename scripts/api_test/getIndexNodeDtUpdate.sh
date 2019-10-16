#!/usr/bin/env bash

PORT=${1:-8888}
INDEX_NAME=$([ ! -z "${2}" ] && echo "/${2}" || echo )
curl -v -H "Authorization: Basic $(echo -n 'admin:adminp4ssw0rd' | base64)" \
  -H "Content-Type: application/json" -X GET "http://localhost:${PORT}${INDEX_NAME}/node_dt_update"

