# Basic Sentence Tokenizer
shinyServer(function(input, output, session) {
  dataSet <- reactive({
    if(is.null(input$inputFile)){
      return(NULL)
    }
    else{
      document <- readLines(input$inputFile$datapath, encoding = "latin1")
      return(document)
    }
  })
  keySet <- reactive({
    if(is.null(input$keywords)){
      return(NULL)
    }
    else{
        givenKeys <- input$keywords
        givenKeys <- givenKeys %>% str_split(',') %>% lapply(str_trim)
        return(givenKeys)
    }
  })

  sentenceArray <- reactive({
    sentenceArray <- tibble(Document=dataSet()) %>%
                        mutate(Id=row_number()) %>% select(Id, Document, everything()) %>%
                        unnest_tokens(Sentences, Document, token="sentences", to_lower=TRUE, drop=FALSE)
    return(sentenceArray)
  })
  
  filteredSenctences <- reactive({
    givenKeys <- unlist(keySet())
    if(length(givenKeys) > 0 ){
      giveSentences <- sentenceArray()
      result <- Reduce('|', lapply(givenKeys, function(key) grepl(key, giveSentences$Sentences)))
      filteredSenctences <- giveSentences[result,, drop=FALSE]$Sentences
      return(filteredSenctences)
    }
    else{
      return(NULL)
    }
  })
  
  wordFrequency <- reactive({
    givenKeys <- unlist(keySet())
    if(length(givenKeys) > 0 ){
      bagOfWords <- tibble(Document=dataSet()) %>%
                      unnest_tokens(Words, Document, to_lower=TRUE)
      keyWordFreq <- bagOfWords %>% count(Words, sort=TRUE, name='Frequency')
      keyWordFreq <- keyWordFreq %>% filter(Words %in% givenKeys)
      return(keyWordFreq)
    }
  })
  
  relativeFrequency <- reactive({
    wordFrequency <- wordFrequency()
    total <- sum(wordFrequency$Frequency)
    wordFrequency$RelativeFrequency <- (wordFrequency$Frequency /total) * 100
    relativeFrequency <- wordFrequency[c('Words','RelativeFrequency')]
    return(relativeFrequency)
  })
  
  output$output <-  renderTable({
    if(is.null(input$inputFile)){
      return(NULL)
    }
    else{
      sentenceArray()
    }
  })
  output$outKeys <- renderText({
    if(is.null(input$keywords)){
      return(NULL)
    }
    else{
      toString(unlist(keySet()))
    }
  })
  output$filteredOutput <- renderTable({
    if(is.null(input$keywords)){
      return(NULL)
    }
    else{
      filteredSenctences()
    }
  })
  output$keyWordFreqOutput <- renderTable({
    if(is.null(input$keywords)){
      return(NULL)
    }
    else{
      relativeFrequency()
    }
  })
  output$relativePlot <- renderPlot({
    if(is.null(input$keywords)){
      return(NULL)
    }
    else{
      relativeFrequency() %>% ggplot(aes(x= reorder(Words, -RelativeFrequency), y=RelativeFrequency)) + geom_col() + xlab(NULL)
    }
  })
  output$relativeWordCloud <- renderPlot({
    if(is.null(input$keywords)){
      return(NULL)
    }
    else{
      wordcloud(relativeFrequency()$Words, relativeFrequency()$RelativeFrequency, colors=brewer.pal(8, "Dark2"))
    }
  })
})