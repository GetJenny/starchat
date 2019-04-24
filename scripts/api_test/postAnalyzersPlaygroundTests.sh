#!/usr/bin/env bash

PORT=8888
INDEX_NAME=index_getjenny_english_0
ALGORITHM=NGRAM2

# search
DATA="{\"traversedStates\": [], \"extractedVariables\":{}}"
ANALYZER="keyword(\\\"test\\\")"
QUERY="this is a test"
curl -H "Authorization: Basic $(echo -n 'admin:adminp4ssw0rd' | base64)" \
  -H "Content-Type: application/json" -X POST "http://localhost:${PORT}/${INDEX_NAME}/analyzer/playground" -d "
{
	\"analyzer\": \"${ANALYZER}\",
	\"query\": \"${QUERY}\",
	\"data\": ${DATA},
	\"searchAlgorithm\": \"${ALGORITHM}\"
}
" | json_pp
RES=$?
[[ ${RES} -gt 0 ]] && echo "error" && exit 1

# matchPatternRegex
DATA="{\"traversedStates\": [\"one\"], \"extractedVariables\":{}}"
ANALYZER=$(echo 'band(prevTravStateIs("one"),binarize(keyword("on")),matchPatternRegex("[day,month,year](?:(0[1-9]|[12][0-9]|3[01])(?:[- \/\.])(0[1-9]|1[012])(?:[- \/\.])((?:19|20)\d\d))"))' | jq --slurp --raw-input)
QUERY="test on 31-11-1900"
curl -H "Authorization: Basic $(echo -n 'admin:adminp4ssw0rd' | base64)" \
  -H "Content-Type: application/json" -X POST "http://localhost:${PORT}/${INDEX_NAME}/analyzer/playground" -d "
{
	\"analyzer\": ${ANALYZER},
	\"query\": \"${QUERY}\",
	\"data\": ${DATA},
	\"searchAlgorithm\": \"${ALGORITHM}\"
}
" | json_pp
RES=$?
[[ ${RES} -gt 0 ]] && echo "error" && exit 1

#extract email
DATA="{\"traversedStates\": [\"one\"], \"extractedVariables\":{}}"
ANALYZER=$(echo 'band(prevTravStateIs("one"),binarize(keyword("email")),matchPatternRegex("[email](?:([a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+))"))' | jq --slurp --raw-input)
QUERY="my email: is this.is.test@email.com"
curl -H "Authorization: Basic $(echo -n 'admin:adminp4ssw0rd' | base64)" \
  -H "Content-Type: application/json" -X POST "http://localhost:${PORT}/${INDEX_NAME}/analyzer/playground" -d "
{
        \"analyzer\": ${ANALYZER},
        \"query\": \"${QUERY}\",
        \"data\": ${DATA},
        \"searchAlgorithm\": \"${ALGORITHM}\"
}
" | json_pp
RES=$?
[[ ${RES} -gt 0 ]] && echo "error" && exit 1

# matchDateDDMMYYYY
DATA="{\"traversedStates\": [], \"extractedVariables\":{}}"
ANALYZER="matchDateDDMMYYYY(\\\"pattern_for_variables_\\\")"
QUERY="on 31-11-1900"
curl -H "Authorization: Basic $(echo -n 'admin:adminp4ssw0rd' | base64)" \
  -H "Content-Type: application/json" -X POST "http://localhost:${PORT}/${INDEX_NAME}/analyzer/playground" -d "
{
	\"analyzer\": \"${ANALYZER}\",
	\"query\": \"${QUERY}\",
	\"data\": ${DATA},
	\"searchAlgorithm\": \"${ALGORITHM}\"
}
" | json_pp
RES=$?
[[ ${RES} -gt 0 ]] && echo "error" && exit 1

# existsVariable
DATA="{\"traversedStates\": [], \"extractedVariables\":{\"TS\": \"1549986933000\", \"OPEN_HOUR\": \"8\", \"CLOSE_HOUR\": \"18\", \"CURRENT_HOUR\": \"19\"}}"
ANALYZER="existsVariable(\\\"OPEN_HOUR\\\")"
QUERY="whatever query"
curl -H "Authorization: Basic $(echo -n 'admin:adminp4ssw0rd' | base64)" \
  -H "Content-Type: application/json" -X POST "http://localhost:${PORT}/${INDEX_NAME}/analyzer/playground" -d "
{
	\"analyzer\": \"${ANALYZER}\",
	\"query\": \"${QUERY}\",
	\"data\": ${DATA},
	\"searchAlgorithm\": \"${ALGORITHM}\"
}
" | json_pp
RES=$?
[[ ${RES} -gt 0 ]] && echo "error" && exit 1

# hasTravState
DATA="{\"traversedStates\": [\"paperone\", \"pippo\", \"pluto\", \"paperino\"], \"extractedVariables\":{}}"
ANALYZER="hasTravState(\\\"pippo\\\")"
QUERY="whatever query"
curl -H "Authorization: Basic $(echo -n 'admin:adminp4ssw0rd' | base64)" \
  -H "Content-Type: application/json" -X POST "http://localhost:${PORT}/${INDEX_NAME}/analyzer/playground" -d "
{
	\"analyzer\": \"${ANALYZER}\",
	\"query\": \"${QUERY}\",
	\"data\": ${DATA},
	\"searchAlgorithm\": \"${ALGORITHM}\"
}
" | json_pp
RES=$?
[[ ${RES} -gt 0 ]] && echo "error" && exit 1

# lastTravStateIs
DATA="{\"traversedStates\": [\"paperone\", \"pippo\", \"pluto\", \"paperino\"], \"extractedVariables\":{}}"
ANALYZER="lastTravStateIs(\\\"paperino\\\")"
QUERY="whatever query"
curl -H "Authorization: Basic $(echo -n 'admin:adminp4ssw0rd' | base64)" \
  -H "Content-Type: application/json" -X POST "http://localhost:${PORT}/${INDEX_NAME}/analyzer/playground" -d "
{
	\"analyzer\": \"${ANALYZER}\",
	\"query\": \"${QUERY}\",
	\"data\": ${DATA},
	\"searchAlgorithm\": \"${ALGORITHM}\"
}
" | json_pp
RES=$?
[[ ${RES} -gt 0 ]] && echo "error" && exit 1

# prevTravStateIs
DATA="{\"traversedStates\": [\"paperone\", \"pippo\", \"pluto\", \"paperino\"], \"extractedVariables\":{}}"
ANALYZER="prevTravStateIs(\\\"pluto\\\")"
QUERY="whatever query"
curl -H "Authorization: Basic $(echo -n 'admin:adminp4ssw0rd' | base64)" \
  -H "Content-Type: application/json" -X POST "http://localhost:${PORT}/${INDEX_NAME}/analyzer/playground" -d "
{
	\"analyzer\": \"${ANALYZER}\",
	\"query\": \"${QUERY}\",
	\"data\": ${DATA},
	\"searchAlgorithm\": \"${ALGORITHM}\"
}
" | json_pp
RES=$?
[[ ${RES} -gt 0 ]] && echo "error" && exit 1

# distance
DATA="{\"traversedStates\": [\"paperone\", \"pippo\", \"pluto\", \"paperino\"], \"extractedVariables\":{}}"
ANALYZER="distance(\\\"I forgot my password\\\")"
QUERY="I forgot something"
curl -H "Authorization: Basic $(echo -n 'admin:adminp4ssw0rd' | base64)" \
  -H "Content-Type: application/json" -X POST "http://localhost:${PORT}/${INDEX_NAME}/analyzer/playground" -d "
{
	\"analyzer\": \"${ANALYZER}\",
	\"query\": \"${QUERY}\",
	\"data\": ${DATA},
	\"searchAlgorithm\": \"${ALGORITHM}\"
}
" | json_pp
RES=$?
[[ ${RES} -gt 0 ]] && echo "error" && exit 1

# checkDayOfMonth # bug in 5.0 fixed in 5.1
DATA="{\"traversedStates\": [\"paperone\", \"pippo\", \"pluto\", \"paperino\"], \"extractedVariables\":{}}"
ANALYZER="checkDayOfMonth(\\\"21\\\", \\\"GreaterOrEqual\\\", \\\"CET\\\")" # Operator can be any of "LessOrEqual", "Less", "Greater", "GreaterOrEqual", "Equal"
QUERY="I forgot something"
curl -H "Authorization: Basic $(echo -n 'admin:adminp4ssw0rd' | base64)" \
  -H "Content-Type: application/json" -X POST "http://localhost:${PORT}/${INDEX_NAME}/analyzer/playground" -d "
{
	\"analyzer\": \"${ANALYZER}\",
	\"query\": \"${QUERY}\",
	\"data\": ${DATA},
	\"searchAlgorithm\": \"${ALGORITHM}\"
}
" | json_pp
RES=$?
[[ ${RES} -gt 0 ]] && echo "error" && exit 1

# checkDayOfWeek
DATA="{\"traversedStates\": [\"paperone\", \"pippo\", \"pluto\", \"paperino\"], \"extractedVariables\":{}}"
ANALYZER="checkDayOfWeek(\\\"1\\\", \\\"GreaterOrEqual\\\", \\\"CET\\\")" # Operator can be any of "LessOrEqual", "Less", "Greater", "GreaterOrEqual", "Equal"
QUERY="I forgot something"
curl -H "Authorization: Basic $(echo -n 'admin:adminp4ssw0rd' | base64)" \
  -H "Content-Type: application/json" -X POST "http://localhost:${PORT}/${INDEX_NAME}/analyzer/playground" -d "
{
	\"analyzer\": \"${ANALYZER}\",
	\"query\": \"${QUERY}\",
	\"data\": ${DATA},
	\"searchAlgorithm\": \"${ALGORITHM}\"
}
" | json_pp
RES=$?
[[ ${RES} -gt 0 ]] && echo "error" && exit 1

# checkHour
DATA="{\"traversedStates\": [\"paperone\", \"pippo\", \"pluto\", \"paperino\"], \"extractedVariables\":{}}"
ANALYZER="checkHour(\\\"17\\\", \\\"GreaterOrEqual\\\", \\\"CET\\\")" # Operator can be any of "LessOrEqual", "Less", "Greater", "GreaterOrEqual", "Equal"
QUERY="I forgot something"
curl -H "Authorization: Basic $(echo -n 'admin:adminp4ssw0rd' | base64)" \
  -H "Content-Type: application/json" -X POST "http://localhost:${PORT}/${INDEX_NAME}/analyzer/playground" -d "
{
	\"analyzer\": \"${ANALYZER}\",
	\"query\": \"${QUERY}\",
	\"data\": ${DATA},
	\"searchAlgorithm\": \"${ALGORITHM}\"
}
" | json_pp
RES=$?
[[ ${RES} -gt 0 ]] && echo "error" && exit 1

# checkMinute
DATA="{\"traversedStates\": [\"paperone\", \"pippo\", \"pluto\", \"paperino\"], \"extractedVariables\":{}}"
ANALYZER="checkMinute(\\\"17\\\", \\\"GreaterOrEqual\\\", \\\"CET\\\")" # Operator can be any of "LessOrEqual", "Less", "Greater", "GreaterOrEqual", "Equal"
QUERY="I forgot something"
curl -H "Authorization: Basic $(echo -n 'admin:adminp4ssw0rd' | base64)" \
  -H "Content-Type: application/json" -X POST "http://localhost:${PORT}/${INDEX_NAME}/analyzer/playground" -d "
{
	\"analyzer\": \"${ANALYZER}\",
	\"query\": \"${QUERY}\",
	\"data\": ${DATA},
	\"searchAlgorithm\": \"${ALGORITHM}\"
}
" | json_pp
RES=$?
[[ ${RES} -gt 0 ]] && echo "error" && exit 1

# checkMonth
DATA="{\"traversedStates\": [\"paperone\", \"pippo\", \"pluto\", \"paperino\"], \"extractedVariables\":{}}"
ANALYZER="checkMonth(\\\"3\\\", \\\"GreaterOrEqual\\\", \\\"CET\\\")" # Operator can be any of "LessOrEqual", "Less", "Greater", "GreaterOrEqual", "Equal"
QUERY="I forgot something"
curl -H "Authorization: Basic $(echo -n 'admin:adminp4ssw0rd' | base64)" \
  -H "Content-Type: application/json" -X POST "http://localhost:${PORT}/${INDEX_NAME}/analyzer/playground" -d "
{
	\"analyzer\": \"${ANALYZER}\",
	\"query\": \"${QUERY}\",
	\"data\": ${DATA},
	\"searchAlgorithm\": \"${ALGORITHM}\"
}
" | json_pp
RES=$?
[[ ${RES} -gt 0 ]] && echo "error" && exit 1

# checkTimestamp
DATA="{\"traversedStates\": [\"paperone\", \"pippo\", \"pluto\", \"paperino\"], \"extractedVariables\":{}}"
ANALYZER="checkTimestamp(\\\"1553275799\\\", \\\"GreaterOrEqual\\\")" # Operator can be any of "LessOrEqual", "Less", "Greater", "GreaterOrEqual", "Equal"
QUERY="I forgot something"
curl -H "Authorization: Basic $(echo -n 'admin:adminp4ssw0rd' | base64)" \
  -H "Content-Type: application/json" -X POST "http://localhost:${PORT}/${INDEX_NAME}/analyzer/playground" -d "
{
	\"analyzer\": \"${ANALYZER}\",
	\"query\": \"${QUERY}\",
	\"data\": ${DATA},
	\"searchAlgorithm\": \"${ALGORITHM}\"
}
" | json_pp
RES=$?
[[ ${RES} -gt 0 ]] && echo "error" && exit 1

# double -> toDouble in 5.1
DATA="{\"traversedStates\": [\"paperone\", \"pippo\", \"pluto\", \"paperino\"], \"extractedVariables\":{\"OPEN_HOUR\": \"8\"}}"
ANALYZER=${1:-"eq(doubleNumberVariable(\\\"OPEN_HOUR\\\", \\\"7.0\\\"), toDouble(\\\"8.0\\\"))"}
QUERY="I forgot something"
curl -H "Authorization: Basic $(echo -n 'admin:adminp4ssw0rd' | base64)" \
  -H "Content-Type: application/json" -X POST "http://localhost:${PORT}/${INDEX_NAME}/analyzer/playground" -d "
{
	\"analyzer\": \"${ANALYZER}\",
	\"query\": \"${QUERY}\",
	\"data\": ${DATA},
	\"searchAlgorithm\": \"${ALGORITHM}\"
}
" | json_pp
RES=$?
[[ ${RES} -gt 0 ]] && echo "error" && exit 1
