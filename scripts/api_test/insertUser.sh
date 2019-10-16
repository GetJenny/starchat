#!/usr/bin/env bash

PORT=${1:-8888}

curl -v -H "Authorization: Basic $(echo -n 'admin:adminp4ssw0rd' | base64)" \
  -H "Content-Type: application/json" -X POST http://localhost:${PORT}/user -d '{
	"id": "test_user",
	"password": "3c98bf19cb962ac4cd0227142b3495ab1be46534061919f792254b80c0f3e566f7819cae73bdc616af0ff555f7460ac96d88d56338d659ebd93e2be858ce1cf9", 
	"salt": "salt",
	"permissions": {
		"index_getjenny_english_0": ["read", "write", "stream"],
		"index_getjenny_english_1": ["read", "write", "stream"],
		"index_getjenny_english_2": ["read", "write", "stream"],
		"index_getjenny_english_3": ["read", "write", "stream"],
		"index_getjenny_english_4": ["read", "write", "stream"],
		"index_getjenny_chinese_0": ["read", "write", "stream"],
		"index_getjenny_english_common_0": ["read", "stream"]
	}
}'

