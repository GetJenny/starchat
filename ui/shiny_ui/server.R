library(shiny)

shinyServer(function(input, output){
  defalt_req<- parse_req(decision_get("further_details_access_question"))
  v<- reactiveValues(
    answer = defalt_req$answer,
    action = defalt_req$action,
    action_input = defalt_req$action_input
  )

  output$answer<- renderText({
    v$answer
  })
  
  ## set response button
  output$no_ans<- renderUI({
    if(v$action == "insert_KB"){
      actionButton("insert_kb", label = "Yes", icon = icon("circle-o", lib = "font-awesome"))
    }else{
      NULL
    }
  })
  
  output$input_info<- renderUI({
    if(v$action == "input_form"){
      textInput("email_ad", label = "Your email", width = "400px")
    }else{
      NULL
    }
  })
  
  output$send_mail<- renderUI({
    if(v$action == "input_form"){
      actionButton("send_mail", label = "send", icon = icon("send-o", lib = "font-awesome"))
    }else{
      NULL
    }
  })
  
  output$fur_ans<- renderUI({
    if(v$action == "show_buttons"){
      selectInput("change_state", label = "Choose from below", choices = c("", unlist(v$action_input)), selected="")
    }else{
      NULL
    }
  })
  
  observeEvent(input$send, {
      req<- parse_req(decision_search(input$question))
      if (req$type == "NA"){ ## No answers in decision table
        req<- parse_req(knbase_search(input$question)) ## search from knowledge base
      }
      v<- reactiveValues(
        answer = req$answer,
        action = req$action,
        action_input = req$action_input
      )
      
      output$answer<- renderText({
        v$answer
      })
      
      ## set response button
      output$no_ans<- renderUI({
        if(v$action == "insert_KB"){
          actionButton("insert_kb", label = "Yes", icon = icon("circle-o", lib = "font-awesome"))
        }else{
          NULL
        }
      })

      output$input_info<- renderUI({
        if(v$action == "input_form"){
          textInput("email_ad", label = "Your email", width = "400px")
        }else{
          NULL
        }
      })
      
      output$send_mail<- renderUI({
        if(v$action == "input_form"){
          actionButton("send_mail", label = "send", icon = icon("send-o", lib = "font-awesome"))
        }else{
          NULL
        }
      })
      
      output$fur_ans<- renderUI({
        if(v$action == "show_buttons"){
          selectInput("change_state", label = "Choose from below", choices = c("", unlist(v$action_input)), selected="")
        }else{
          NULL
        }
      })
  })
  
  observeEvent(input$insert_kb, {
    output$answer<- renderText({
      "Thenk you, your question is safely stored"
    })

    output$no_ans<- renderUI({
      NULL
    })
  })
  
  observeEvent(input$change_state, {
    if(input$change_state != ""){
      req<- parse_req(decision_get(input$change_state))
      v<- reactiveValues(
        answer = req$answer,
        action = req$action,
        action_input = req$action_input
      )
      
      output$answer<- renderText({
        v$answer
      })
      
      ## set response button
      output$no_ans<- renderUI({
        if(v$action == "insert_KB"){
          actionButton("insert_kb", label = "Yes", icon = icon("circle-o", lib = "font-awesome"))
        }else{
          NULL
        }
      })
      
      output$input_info<- renderUI({
        if(v$action == "input_form"){
          textInput("email_ad", label = "Your email", width = "400px")
        }else{
          NULL
        }
      })
      
      output$send_mail<- renderUI({
        if(v$action == "input_form"){
          actionButton("send_mail", label = "send", icon = icon("send-o", lib = "font-awesome"))
        }else{
          NULL
        }
      })
      
      output$fur_ans<- renderUI({
        if(v$action == "show_buttons"){
          selectInput("change_state", label = "Choose from below", choices = c("", unlist(v$action_input)), selected="")
        }else{
          NULL
        }
      })
    }else{
      NULL
    }
    
  })
  
})