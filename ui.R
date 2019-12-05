# Basic Sentence Tokenizer

library(dplyr)
library(shiny)
library(ggplot2)
library(stringr)
library(tidytext)
library(wordcloud)

shinyUI(
  fluidPage(
    titlePanel("Basic Sentence Tokenizer"),
    
    # Sidepane for inputs
    sidebarPanel(
      fileInput("inputFile", "Upload the text file"),
      textInput('keywords', 'Keywords', placeholder = "Comma seperated words")
    ),
    
    # Main Panel
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Overview", h4(p("How to use this App")),
                           p("To use this app you need a document corpus  in txt file format", align="justify"),
                           verbatimTextOutput("start")),
                  tabPanel("Sentence Tokens", 
                           h3(p("All the sentences")),
                           tableOutput("output"),
                           h4(p("Given keywords")),
                           verbatimTextOutput("outKeys")),
                  tabPanel("Filtered Sentences",
                           h4(p("Filtered Sentences for given keywords")),
                           tableOutput("filteredOutput")),
                  tabPanel("Keyword Frequency", 
                           h4(p("Frequency of occurrence of keywords in corpus")),
                           tableOutput("keyWordFreqOutput"),
                           h4(p("Relative Frequency Bar-Chart")),
                           plotOutput("relativePlot")),
                  tabPanel("Keyword Frequency Word Cloud",
                           h4(p('Word Cloud as per relative frequency')),
                           plotOutput("relativeWordCloud"))
      )
    )
  )
)