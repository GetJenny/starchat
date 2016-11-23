# *Chat

## *Chat in brief
*Chat ("starchat") is a service for the implementation of workflow based chatbot.

*Chat provide a restful service to implement conversational agents.

*Chat is under development and every contribution is welcome.

## What problem *Chat is solving

*Chat solves the problem of specifying a conversational flow with actionable states and templates, it also solves the pattern matching and retrieval problem with the identification of the closest concept in the workflow in relation with the user input.

*Chat must scale horizontally with the instantiation of new services without any service interruption.
The deployment must be easy

*Chat offers a means to store and retrieve the conversations in consistent way and suitable to perform data analysis.

*Chat implements a state machine mechanism to guide the user in the conversation and to perform actions in relation of the user inputs.
* When no answer is found in the workflow of the state machine, the system, optionally, can fall back to the knowledge base provided by the collection of conversation of the customer service operators.
* Chat is developed using [akka](http://akka.io).
* Chat it is stateless to work in parallel with as many instances as you need. It is configurable by a simple configuration file and it is containerized with docker: deployment require few minutes!

## How does *Chat work?

*Chat stores the conversations on a search engine with special configuration to handle the language of the bot

A special table of the search engine is reserved for the state machine.
The state machine allows to specify templated sentences to send personalized messages to the user

When the user enters a phrase, *Chat search the closest match in the state machine and guide the conversation from a state to another.

When no state matches the input text, optionally, *Chat searches on the knowledge base built with the past conversations.

## Customization of *Chat

*Chat rely on [elasticsearch](https://www.elastic.co) for storing data and for the retrieval functionalities.
Is straightforward to customize the elasticsearch schema for new languages
and adding new analyzers or new fields.

# System Design

## The data schema

* *Chat uses two data types on the same index:
    * state type : for the storage of the state machine, the fields are the following
        * state : the name of the state 
        * queries : the list of queries which trigger the state e.g. ["cannot access account", "problem access account"],
        * bubble : the text to show to the user on UI
        * action : the name of a function to call (implemented by the client)  
        * action_input : a dictionary (key,value) with the parameters of the function (implemented by the client)
        * success_value : the destination state if the action function fail succeed  
        * failure_value : the destination state if the action function fail
    * question type : to store the conversations in question/answer format, the fields are the following
        * doctype: specify a type for the documents
        * state: eventual link to any of the state machine states, useful to redirect the user
        to a path of the state machine 
        * verified: was the conversation verified by an operator?
        * conversation: ID of the conversation (multiple q&a may be inside a conversation)
        * topics: eventually done with LDM or similar
        * question: usually what the user of the chat says
        * answer: usually what the operator of the chat says
        * status: tell whether the document is locked for editing or not, useful for a GUI to avoid concurrent modifications

## The Rest endpoint

The *Chat REST service stands in the middle between the CLIENT which implement the BOT UI
and the dataset and it provide data intelligence.

The behaviour of the state machine is specified
by a csv called **SYSTEM TEMPLATE SCRIPT** which has the format:

* **(R)**: means return value, this field is returned by the REST function
* **(T)**: indicates conversational triggers to the state.
* **(I)**: means internal, this field is not exposed to the API

The csv contains the following columns:

* **state**: a unique name of the state
* **bubble (R)**: the output to be shown to the user, when empty nothing is to be shown. The bubble is a string which may contains template variables like %email% or %link%. The client is responsible to provide these functions
* **action (R)**: a function to be called on the client side. The user is responsible of the implementation of these actions, see Client functions
* **action_input (R)**: this is the input passed to the function in "Action". The text might contains template variables. see get_next_response
* **success_value (R)** (instructions for client): output to return in case of success
* **failure_value (R)** (instructions for client): output to return in case of failure
* **query (T,I)**: this is a list of queries which should trigger the state

## Client functions

The client should implement at least the following set of functions:

* show_buttons: tell the client to render a multiple choice button
    * input: a key/value pair with the key indicating the text to be shown in the button, and the value indicating the state to follow e.g.: {"Forgot Password": "forgot_password", "Account locked": "account_locked", "Specify your problem": "specify_problem", "I want to call an operator": "call_operator", "None of the above": "start"}
    * output: the choice related to the button clicked by the user e.g.: "account_locked"
* input_form: render an input form or collect the input following a specific format
    * input: a dictionary with the list of fields and the type of fields, at least "email" must be supported: e.g.: { "email": "email" } where the key is the name and the value is the type
    * output: a dictionary with the input values e.g.: { "email": "a@b.com" }
* send_password_generation_link: send an email with instructions to regenerate the password
    * input: a valid email address e.g.: "a@b.com"
    * output: a dictionary with the response fields e.g.: { "user_id": "123", "current_state": "forgot_password", "status": "true" }

Other application specific functions can be implemented by the client these functions must be called with the prefix
"priv_" e.g. "priv_retrieve_user_transactions"

See the file doc/sample_state_machine_specification.csv for a sample of the state machine specification.

# Mechanics

* The client implements the functions which appear in the action field of the spreadsheet. We will provide interfaces.
* The client call the rest API "decisiontable" endpoint communicating a state if any, the user input data and other
state variables
* The client receive a response with guidance on what to return to the user and what are the possible next steps
* The client render the message to the user and eventually collect the input, then call again the system to get
instructions on what to do next
* When the "decisiontable" functions does not return any results the user can call the "knowledgebase" endpoint
which contains all the conversations. 
  
# *Chat Api specification

## get_next_response

### Supported method (POST):

#### Body

```json
{
    "user_id": "the user id",
    "user_input": "(Optional)",
    "text" : "the text typed by the user (Optional)",
    "img": "(e.g.) image attached by the user (Optional)",
    "values": "(Optional)",
    "return_value": "the value either in success_value or in failure_value (Optional)",
    "data": "all the variables, e.g. for the STRING TEMPLATEs (Optional)"
}
```

#### example of the input dictionary after the user has clicked "Forgot Password":

```json
{
    "user_id": "1234", 
    "user_input": { "text": "" },
    "values": {
        "return_value":  "forgot_password",
        "data": {}
    }
}
```

#### example of the input dictionary after the user has entered the email in "forgot_password"

```json
{
    "user_id": "1234", 
    "user_input": { "text": "" },
    "values": {
        "return_value": "send_password_generation_link",
        "data": { "email": "a@b.com" }
    }
}
```

#### Output JSON
##### return code: 200

```json
{
    "bubble": "the text for the bubble",
    "action": "the action function which is to be called e.g. input_form",
    "action_input": "the json string containing the input for the action function",
    "data":  "the same data field received in input",
    "success_value": "the value to be returned in case of success",
    "failure_value": "the value to be returned in case of failure"
}
```

#### example of the output dictionary after the user said that he has forgot the password (state "forgot_password"):

```json
{
    "bubble": "We can reset your password by sending you a message to your registered e-mail address. Please tell me your address so I may send you the new password generation link.",
    "action": "input_form",
    "action_input": { "email": "email"},
    "data": { "email": "a@b.com" },
    "success_value": "send_password_generation_link",
    "failure_value": "dont_understand"
}
```

#### example of output dictionary after the CLIENT has sent to the SYSTEM the email address where to send the reset link (state "send_password_generation_link")

```json
{
    "bubble": "Thank you. An e-mail will be sent to this address: a@b.com with your account details and the necessary steps for you to reset your password.",
    "action": "send_password_generation_link",
    "action_input": { "template": "somebody requested a regenaration of your password, if you requested the password reset follow the link: https://www.restorepassword.com/blabla", "email": "a@b.com" },
    "data": { "email": "a@b.com" },
    "success_value": "any_further",
    "failure_value": "call_operator"
}
```

##### return code: 204

this code is returned when no response was found by the engine

##### Errors

The function in case of success will return the following codes and data structures:

* return code: 500
    * meaning: internal server error
* return code: 400: bad request
    * meaning: the input data structure is not valid
    * output data: no data returned
* return code: 422
    *  meaning: bad request data, the input data is formally valid but there is some issue with data interpretation
    * output data: the output data structure is a json dictionary with two fields: code and message. The following code are supported:
        * code: 100
        * message: "error evaluating the template strings, bad values"
* return code: 404
    * meaning: not found
    * output data: no data returned

## Decisiontable

### Method GET

Get a document by ID

Output JSON

return code: 200

Sample call

```bash
# retrieve one or more entries with given ids; ids can be specified multiple times
curl -v -H "Content-Type: application/json" "http://localhost:8888/decisiontable?ids=further_details_access_question"
```

Sample output

```json
{
    "hits": [
        {
            "document": {
                "action": "show_buttons",
                "action_input": {
                    "Account locked": "account_locked",
                    "Forgot Password": "forgot_password",
                    "I want to call an operator": "call_operator",
                    "None of the above": "start",
                    "Payment problem": "payment_problem",
                    "Specify your problem": "specify_problem"
                },
                "bubble": "What seems to be the problem exactly?",
                "failure_value": "dont_understand",
                "queries": [
                    "cannot access account",
                    "problem access account"
                ],
                "state": "further_details_access_question",
                "success_value": "eval(show_buttons)"
            },
            "score": 0.0
        }
    ],
    "max_score": 0.0,
    "total": 1
}
```

### Method PUT

Output JSON

return code: 201

Sample call

```bash
# update the "further_details_access_question" entry in the DT
curl -v -H "Content-Type: application/json" -X PUT http://localhost:8888/decisiontable/further_details_access_question -d '{
  "queries": ["cannot access account", "problem access account", "unable to access to my account"]
}'
```

Sample output
```json
{
    "created": false,
    "dtype": "state",
    "id": "further_details_access_question",
    "index": "jenny-en-0",
    "version": 2
}
```

### Method POST

Insert a new document.

Output JSON

return code: 201

Sample call
```bash
curl -v -H "Content-Type: application/json" -X POST http://localhost:8888/decisiontable -d '{
  "state": "further_details_access_question",
  "queries": ["cannot access account", "problem access account"],
  "bubble": "What seems to be the problem exactly?",
  "action": "show_buttons",
  "action_input": {"Forgot Password": "forgot_password", "Account locked": "account_locked", "Payment problem": "payment_problem", "Specify your problem": "specify_problem", "I want to call an operator": "call_operator", "None of the above": "start"},
  "success_value": "eval(show_buttons)",
  "failure_value": "dont_understand"
}'
```

Sample output

```json
{
    "created": true,
    "dtype": "state",
    "id": "further_details_access_question",
    "index": "jenny-en-0",
    "version": 1
}
```

### Method DELETE

Delete a document by ID

Output JSON

return code: 200

Sample call
```bash
curl -v -H "Content-Type: application/json" -X DELETE http://localhost:8888/decisiontable/further_details_access_question
```

Sample output

```json
{
    "dtype": "state",
    "found": true,
    "id": "further_details_access_question",
    "index": "jenny-en-0",
    "version": 3
}
```

### decisiontable_search: Method POST

Update a document

Output JSON

return code: 200

Sample call
```bash
curl -v -H "Content-Type: application/json" -X POST http://localhost:8888/decisiontable_search -d '{
  "queries": "cannot access my account"
}'
```

## Knowledgebase

knowledgebase: Method GET

Return a document by ID

Output JSON

return code: 200

Sample call
```bash
# retrieve one or more entries with given ids; ids can be specified multiple times
curl -v -H "Content-Type: application/json" "http://localhost:8888/knowledgebase?ids=0"
```

Sample response

```json
{
    "hits": [
        {
            "document": {
                "answer": "you are welcome!",
                "conversation": "832",
                "doctype": "normal",
                "id": "0",
                "index_in_conversation": 11,
                "question": "thank you",
                "state": "",
                "status": 0,
                "topics": "",
                "verified": false
            },
            "score": 0.0
        }
    ],
    "max_score": 0.0,
    "total": 1
}
```

### knowledgebase: Method POST

Insert a new document

Sample call

Output JSON

return code: 201

```bash
curl -v -H "Content-Type: application/json" -X POST http://localhost:8888/starchat-en/knowledgebase -d '{
    "answer": "you are welcome!",
    "conversation": "832",
    "doctype": "normal",
    "id": "0",
    "index_in_conversation": 11,
    "question": "thank you",
    "state": "",
    "status": 0,
    "topics": "",
    "verified": true
}'
```

Sample response

```json
{
    "hits": [
        {
            "document": {
                "answer": "you are welcome!",
                "conversation": "832",
                "doctype": "normal",
                "id": "0",
                "index_in_conversation": 11,
                "question": "thank you",
                "state": "",
                "status": 0,
                "topics": "",
                "verified": true
            },
            "score": 0.0
        }
    ],
    "max_score": 0.0,
    "total": 1
}
```

### knowledgebase: Method DELETE

Delete a document by ID

Output JSON

return code: 200

Sample call

curl -v -H "Content-Type: application/json" -X DELETE http://localhost:8888/knowledgebase/0

Sample output

```bash
{
    "dtype": "question",
    "found": false,
    "id": "0",
    "index": "jenny-en-0",
    "version": 5
}
```

### knowledgebase: Method PUT

Update an existing document

Output JSON

return code: 201

Sample call

```bash
curl -v -H "Content-Type: application/json" -X PUT http://localhost:8888/starchat-en/knowledgebase/                                                   e9d7c04d0c539415620884f8c885fef93e9fd0b49bbea23a7f2d08426e4d185119068365a0c1c4a506c5c43079e1e8da4ef7558a7f74756a8d850cb2d14e5297 -d '{
    "answer": "you are welcome!",
    "conversation": "832",
    "doctype": "normal",
    "index_in_conversation": 11,
    "question": "thank yoy",
    "state": "",
    "status": 0,
    "topics": "",
    "verified": false
}'
```

Sample response

```json
{
    "created": false,
    "dtype": "question",
    "id": "e9d7c04d0c539415620884f8c885fef93e9fd0b49bbea23a7f2d08426e4d185119068365a0c1c4a506c5c43079e1e8da4ef7558a7f74756a8d850cb2d14e5297",
    "index": "jenny-en-0",
    "version": 3
}
```

### knowledgebase_search: Method POST

Output JSON

return code: 200

Sample call

```bash
curl -v -H "Content-Type: application/json" -X POST http://localhost:8888/knowledgebase_search -d '{
  "question": "thank you",
  "verified": true,
  "doctype": "normal"
}'
```

Sample output

```json
{
    "hits": [
        {
            "document": {
                "answer": "you are welcome",
                "conversation": "4346",
                "doctype": "normal",
                "id": "10",
                "index_in_conversation": 6,
                "question": "thank you",
                "state": "",
                "status": 0,
                "topics": "",
                "verified": true
            },
            "score": 3.5618982315063477
        }
    ],
    "max_score": 3.5618982315063477,
    "total": 1
}
```

# Build and run the service 

## run directly in the build environment
 
Clone the repository and enter the starchat directory.

Initialize the elasticsearch instance.

Run the service:

```bash
sbt compile run
```

The service binds on the port 8888 by default.

# Setup 

## docker

* generate a packet distribution
```bash
sbt dist
```
* extract the packet into the docker-starchat folder
```bash
unzip target/universal/starchat-0.1.zip && mv starchat-0.1 docker-starchat
```
* enter the directory docker-starchat 
```bash
cd  docker-starchat
```
* review the configuration files
    * edit the file starchat-0.1/config/application.conf and modify the ip where elasticsearch is bind
* run the services (both startchat and elasticsearch)
```bash
docker-compose up -d
```

After these steps the services will be up and running and you can initialize the ES schemas.

##Prepare ES

* enter the directory scripts/index_management/\<lang\> e.g. enter the directory scripts/index_management/english 
* create the analyzers
```bash
./create_analyzer.sh
```
* create the data types
```bash
./set_question_type.sh
./set_state_type.sh
```

##Test

A set of test script is present inside scripts/api_test

##UI

The user-interface is written in R, to use it, please make sure R is properly installer, and go to /ui.

```bash
Rscript run_shiny_ui.R
```

Then the ui will appear on localhost:9000
