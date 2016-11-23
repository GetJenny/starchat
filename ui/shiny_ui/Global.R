library(RCurl)
library(RJSONIO)

decision_get<- function(id){
  api<- "http://localhost:8888/decisiontable"
  getForm(api, ids = id)
}

decision_search<- function(query){
  postForm("http://localhost:8888/decisiontable_search",
           .opts = list(postfields = toJSON(list(queries = query)),
                        httpheader = c('Content-Type' = 'application/json', Accept = 'application/json'),
                        ssl.verifypeer = FALSE))
}

knbase_search<- function(question){
  postForm("http://localhost:8888/knowledgebase_search",
           .opts = list(postfields = toJSON(list(question = question)),
                        httpheader = c('Content-Type' = 'application/json', Accept = 'application/json'),
                        ssl.verifypeer = FALSE))
}

parse_req<- function(req){
  req<- fromJSON(req)
  if (req$total == 0){ ## No answers
    list(
      answer = "Sorry, at this moment, our chatbot is not able to answer your question, would you mind if we take you question for further research?",
      action = "insert_KB",
      action_input = NULL,
      type = "NA"
    )
  }else if ("bubble" %in% names(req$hits[[1]]$document)){ ## From DT
    list(
      answer = req$hits[[1]]$document$bubble,
      action = req$hits[[1]]$document$action,
      action_input = req$hits[[1]]$document$action_input,
      type = "DT"
    )
  }
  else if ("answer" %in% names(req$hits[[1]]$document)){ ## From KD
    list(
      answer = req$hits[[1]]$document$answer,
      action = NULL,
      action_input = NULL,
      type = "KB"
    )
  }
}