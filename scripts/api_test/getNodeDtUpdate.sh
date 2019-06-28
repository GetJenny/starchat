#!/usr/bin/env bash

PORT=${1:-8888}
VERBOSE=${2:-true}
STRICT=${3:-false}
curl -v -H "Authorization: Basic $(echo -n 'admin:adminp4ssw0rd' | base64)" \
  -H "Content-Type: application/json" -X GET "http://localhost:${PORT}/node_dt_update?verbose=${VERBOSE}&strict=${STRICT}"

