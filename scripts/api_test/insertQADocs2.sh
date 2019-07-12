#!/usr/bin/env bash

PORT=${1:-8888}
INDEX_NAME=${2:-index_getjenny_english_0}
#ROUTE=${3:-prior_data}
ROUTE=conversation_logs
#ROUTE=${3:-knowledgebase}

#reset database to create a clean start condition
./deleteAllKB.sh

#populate conversation logs

#add new conversion log with id and state
id=1

function addConversationLog () {
 state="$1"
 question="$2"
 body='{
   "id": "'$id'",
   "conversation": "conv:1000",
   "indexInConversation": 1,
   "status": 0,
   "coreData": {
  "question": "'${question}'",
  "answer": "dummy answer",
  "topics": "t1 t2",
  "verified": true
   },
   "annotations": {
  "state": "'$state'",
  "feedbackAnswereScore": 3.0,   
  "doctype": "NORMAL"
   }
 }'
 echo $body > /tmp/post.json
 curl -v -H "Authorization: Basic $(echo -n 'test_user:p4ssw0rd' | base64)" \
   -H "Content-Type: application/json" -X POST http://localhost:${PORT}/${INDEX_NAME}/${ROUTE} --data @/tmp/post.json
 id=$((id+1))
}



addConversationLog licence "howdo I activate the license?"
addConversationLog licence 'my license seems to be broken. What can I do?'
addConversationLog terrible_feedback 'This chat is awful!!'
addConversationLog call_operator 'May I talk to operator'
addConversationLog call_operator 'May I talk to operator'
addConversationLog further_details_access_question 'May I ask you help'
