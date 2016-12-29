

verbose <<- TRUE
debug <- TRUE


library(shiny)

options(shiny.trace = debug)
source("../utils.R", local = TRUE)
load("data/ngramLookupTables.en_US.RData")


shinyServer(function(input, output) {
  output$suggest <- renderText({
    log(paste("input:",input$text))
    if(nchar(input$text)>0){
    endOfWord <- grepl("[^a-z]$", input$text)
    text <- preProcessData(input$text)
    
      suggestion <- predictSuggest(text, ngramLookupTables, completeWord = endOfWord)
      paste(suggestion$lookup, suggestion$suggest)
    }else{
      ""  
    }
  })
})
