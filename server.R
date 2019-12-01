# Basic Sentence Tokenizer
shinyServer(function(input, output, session) {
  dataSet <- reactive({
    if(is.null(input$inputFile)){
      return(NULL)
    }
    else{
      document <- readLines(input$inputFile$datapath)
      doc.id <- seq(1:length(document))
      calid <- data.frame(doc.id, document)
      #tokens <- tokenize_sentences(document)
      return(calid)
    }
  })
  output$output <- renderPrint({
    if(is.null(input$inputFile)){
      return(NULL)
    }
    else{
      dataSet()$calid
    }
  })
})