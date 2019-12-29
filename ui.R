# Basic Sentence Tokenizer
#" Required packages for the application "
library(dplyr)
library(shiny)
library(tools)
library(ggplot2)
library(stringr)
library(tidytext)
library(wordcloud)

shinyUI(
  fluidPage(
    titlePanel("Keyword Search"),
    
    # Sidepane for inputs
    sidebarPanel(
      # " Input file for processing "
      fileInput("inputFile", "Upload the corpus file (txt/csv)", multiple=FALSE, 
                    accept=c('.csv',
                             '.txt')),
      # " Case sensitive check box "
      checkboxInput("isCaseSensitive", "Case Sensitive Search", value = FALSE),
      # " Dynamic field based on input file "
      tags$div(id="csvColumns"),
      # " Keywords to be searched in corpus "
      textInput('keywords', 'Keywords', placeholder = "Comma seperated words"),
    ),
    
    # Main Panel
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Overview", h4(p("How to use this App")),
                           p("Provide a corpus file and keywords to be looked up. 
                             This app, will filter the sentences for the given keywords (Filtered Sentences),
                             will evaluate the word frequency from corpus and display them in a word cloud", align="justify"),
                           # verbatimTextOutput("start")
                           ),
                  tabPanel("Sentence Tokens", 
                           h3(p("All the sentences")),
                           tableOutput("output"),
                           h4(p("Given keywords")),
                           verbatimTextOutput("outKeys")),
                  tabPanel("Filtered Sentences",
                           h4(p("Filtered Sentences for given keywords")),
                           h5(p("Count of filtered sentences: ")),
                           verbatimTextOutput("filteredSentencesCount"),
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