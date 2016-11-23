library(shiny)

shinyUI(
  fluidPage(
    titlePanel("*Chat: We finds you the answers"),
    
    sidebarLayout(
      sidebarPanel(
        textInput(inputId = "question", 
                  label = "Type your question here", 
                  value = "I cannot access account!"
        ),
        actionButton("send", label = "Ask *Chat!", icon = icon("comment-o", lib = "font-awesome"))
      ),
      
      mainPanel(
        h3("Answer"),
        hr(),
        textOutput('answer'),
        hr(),
        uiOutput("fur_ans"),
        uiOutput("no_ans"),
        uiOutput("input_info"),
        uiOutput("send_mail")
      )
    )
  )
)
