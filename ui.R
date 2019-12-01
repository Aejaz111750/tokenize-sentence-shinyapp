# Basic Sentence Tokenizer

library(shiny)
library(tokenizers)

shinyUI(
  fluidPage(
    titlePanel("Basic Sentence Tokenizer"),
    
    # Sidepane for inputs
    sidebarPanel(
      fileInput("inputFile", "Upload the text file")
    ),
    
    # Main Panel
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Overview", h4(p("How to use this App")),
                           p("To use this app you need a document corpus  in txt file format", align="justify"),
                           verbatimTextOutput("start")),
                  tabPanel("Sentence Tokens", 
                           verbatimTextOutput("output"))
                  )
    )
  )
)