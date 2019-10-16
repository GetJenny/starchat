#!/usr/bin/env bash

curl -u "elastic:rRl4CCEPjYgk9agw" -k -XPUT -H "Content-Type: application/json" https://localhost:9200/_cluster/settings -d'
{
    "persistent" : {
      "action.auto_create_index": "false" 
    }
}'

