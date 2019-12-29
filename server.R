# Basic Sentence Tokenizer
shinyServer(function(input, output, session) {
  # " adds feature columns for count of key words "
  getKeywordCounts <- function(sentence, key){
    return(stri_count_fixed(sentence, key))
  }
  # " file encoding from UI "
  fileEncoding <- reactive({
    return(input$fileEncoding)
  })
  # " Dynamic dropdown div values "
  colSelector_added = c()
  # " csv column headers, if .csv file if provided "
  csvColumnsFromFile <- reactive({
    if(is.null(input$inputFile)){
      return(NULL)
    }
    else if(!is.null(input$inputFile) && inFileExt()=='csv'){
      return(names(dataSet()))
    }
  })
  # " CSV column selected from drop down "
  csvSelectedValue <- reactive({
    if(!is.null(input$inputFile) && inFileExt()=='csv'){
      return(c(input[["csvColumns_exists"]]))
    }
    else{
      return(NULL)
    }
  })
  csvHeaders <- reactive({
    if(!is.null(input$inputFile) && inFileExt()=='csv'){
      if(isCSVWithHeaders()){
        return(csvColumnsFromFile())
      }
      else{
        return(seq(1, ncol(dataSet()), by=1))
      }
    }
    else{
      return(NULL)
    }
  })
  # " Observer for the dynamic field to capture column name "
  observeEvent(input$inputFile, {
    if(length(colSelector_added)==0 && inFileExt()=='csv'){
      ranNum = sample(1:50, 1, replace = FALSE)
      # id <- "csvColumns"
      id <- paste0("csvColumns", sep='_', ranNum)
      insertUI(
        selector = "#csvColumns",
        # selector = paste0("#csvColumns", sep='_', ranNum),
        ui = tags$div(
          selectInput(
          # selectizeInput(
            inputId = paste0("csvColumns",'_exists'),
            label = 'Choose the text column',
            choices = csvHeaders(),
            selected = NULL,
          ),
          id = id
        )
      )
      colSelector_added <<- c(id, colSelector_added)
    }
    else if (length(colSelector_added)!=0 && inFileExt()=='txt'){
      removeUI(selector = paste0('#', colSelector_added[length(colSelector_added)]), multiple = TRUE, immediate=TRUE)
      colSelector_added <<- colSelector_added[-length(colSelector_added)]
    }
  })
  observeEvent(input$isCSVWithHeaders,{
    values = csvSelectedValue()
    updateSelectInput(session, paste0("csvColumns",'_exists'), 
                      choices = csvHeaders(),
                      selected = NULL)
    values = csvSelectedValue()
  })
  # " Does the CSV had headers "
  isCSVWithHeaders <- reactive({
    return(input$isCSVWithHeaders)
  })
  # " Case sensitive flag "
  isCaseSensitive <- reactive({
    return(input$isCaseSensitive)
  })
  # " File extension of the provided file "
  inFileExt <- reactive({
    if(is.null(input$inputFile)){
      return(NULL)
    }
    else{
      inFileExt <- file_ext(input$inputFile$name)
      return(inFileExt)
    }
  })
  # " Data from the provided file "
  dataSet <- reactive({
    if(is.null(input$inputFile)){
      return(NULL)
    }
    else{
      if(inFileExt()=='txt'){
        txtData <- readLines(input$inputFile$datapath, encoding = fileEncoding())
        return(txtData)
      }
      else if (inFileExt()=='csv'){
        csvData <- read.csv(input$inputFile$datapath, encoding = fileEncoding())
        return(csvData)
      }
    }
  })
  # " Parsing the given keywords "
  keySet <- reactive({
    if(is.null(input$keywords)){
      return(NULL)
    }
    else{
        givenKeys <- input$keywords
        givenKeys <- givenKeys %>% str_split(',') %>% lapply(str_trim)
        skipValues = lapply(givenKeys, function(k) k!='')
        tempValues = tibble(keys=unlist(givenKeys))
        givenKeys = tempValues[unlist(skipValues),,]$keys
        if(isCaseSensitive()){
          return(givenKeys)
        }
        else{
          givenKeys <- givenKeys %>% lapply(tolower)
          return(givenKeys)
        }
    }
  })
  # " Tokenizing the corpus to sentences "
  sentenceArray <- reactive({
    if(inFileExt()=='csv'){
      if(is.null(csvSelectedValue())){
        data = NULL
      }
      else{
        if(isCSVWithHeaders() && suppressWarnings(is.na(as.integer(csvSelectedValue())))){
          data = dataSet()[,csvSelectedValue()]
        }
        else if (suppressWarnings(!is.na(as.integer(csvSelectedValue())))){
          data = dataSet()[,as.integer(csvSelectedValue())]
        }
        else{
          return(NULL)
        }
      }
    }
    else{
      data = dataSet()
    }
    if(isCaseSensitive()){
      sentenceArray <- tibble(Document=data) %>%
                          mutate(Id=row_number()) %>% select(Id, Document, everything()) %>%
                          unnest_tokens(Sentences, Document, token="sentences", to_lower=FALSE, drop=FALSE)
      return(sentenceArray)
    }
    else{
      sentenceArray <- tibble(Document=data) %>%
                        mutate(Id=row_number()) %>% select(Id, Document, everything()) %>%
                        unnest_tokens(Sentences, Document, token="sentences", to_lower=TRUE, drop=FALSE)
      return(sentenceArray)
    }
  })
  # " Filtering the sentences based on given key words "
  filteredSenctences <- reactive({
    givenKeys <- unlist(keySet())
    if(length(givenKeys) > 0 && !is.null(sentenceArray())){
      giveSentences <- sentenceArray()
      result <- Reduce('|', lapply(givenKeys, function(key) grepl(key, giveSentences$Sentences, fixed=TRUE)))
      filteredSenctences <- giveSentences[result,, drop=FALSE]$Sentences
      return(tibble('Filtered Senctences'=filteredSenctences))
    }
    else{
      return(NULL)
    }
  })
  relativeFrequency <- reactive({
    givenKeys <- unlist(keySet())
    if((length(givenKeys) >= 1 && givenKeys != "") && 
       (!is.null(filteredSenctences()) && nrow(filteredSenctences())>0)){
      featureDF <- filteredSenctences()
      for(key in givenKeys){
        featureDF <- featureDF %>% mutate(!!as.name(key):=getKeywordCounts(`Filtered Senctences`, key))
      }
      featureDF <- featureDF[-1]
      keyFrequency <- data.frame('Words'=colnames(featureDF), 'Frequency'=colSums(featureDF))
      rownames(keyFrequency)<-NULL
      total <- sum(keyFrequency$Frequency)
      keyFrequency$RelativeFrequency <- (keyFrequency$Frequency /total) * 100
      relativeFrequency <- keyFrequency[c('Words','RelativeFrequency')]
      return(relativeFrequency)
    }
  })
  # " Outputs "
  output$output <-  renderTable({
    if(is.null(input$inputFile) || (inFileExt()=='csv' && is.null(csvSelectedValue()))){
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
  output$filteredSentencesCount <- renderText({
    if(length(unlist(keySet()))==1 && unlist(keySet())==""){
      return(NULL)
    }
    else{
      nrow(filteredSenctences())
  }
  })
  output$filteredOutput <- renderTable({
    if(is.null(input$keywords) || (inFileExt()=='csv' && is.null(csvSelectedValue())) || 
       (length(unlist(keySet()))==1 && unlist(keySet())=="")){
      return(NULL)
    }
    else{
      filteredSenctences()
    }
  })
  output$keyWordFreqOutput <- renderTable({
    if(is.null(input$keywords) || (inFileExt()=='csv' && is.null(csvSelectedValue())) || 
       (is.null(relativeFrequency()) || nrow(relativeFrequency())==0)){
      return(NULL)
    }
    else{
      relativeFrequency()
    }
  })
  output$relativePlot <- renderPlot({
    if(is.null(input$keywords) || (inFileExt()=='csv' && is.null(csvSelectedValue())) || 
       (is.null(relativeFrequency()) || nrow(relativeFrequency())==0)){
      return(NULL)
    }
    else{
      relativeFrequency() %>% ggplot(aes(x= reorder(Words, -RelativeFrequency), y=RelativeFrequency)) + geom_col() + xlab(NULL)
    }
  })
  output$relativeWordCloud <- renderPlot({
    if(is.null(input$keywords) || (length(unlist(keySet()))==1 && unlist(keySet())=="") || 
       (inFileExt()=='csv' && is.null(csvSelectedValue())) || 
       (is.null(relativeFrequency()) || nrow(relativeFrequency())==0)){
      return(NULL)
    }
    else{
      wordcloud(relativeFrequency()$Words, relativeFrequency()$RelativeFrequency, colors=brewer.pal(8, "Dark2"))
    }
  })
})