

verbose <<- TRUE
debug <- TRUE


library(shiny)


options(shiny.trace = debug)
source("utils.R", local = TRUE)
load("data/ngramLookupTables.en_US.RData")


shinyServer(function(input, output) {
  output$suggest <- renderText({
    updateTypeahead(input$text)
  })
  
  output$debug = renderText({
    updateTypeahead(input$mydata)
  })
  
  updateTypeahead <- function(text){
    log(paste("input:",text))
    if(nchar(text)>0){
      endOfWord <- grepl("[^a-z]$", text)
      text <- preProcessData(text)
      if(nchar(text)>0){
        suggestion <- predictSuggest(text, ngramLookupTables, completeWord = endOfWord)
        paste(suggestion$lookup, suggestion$suggest)
      }else{
        ""
      }
    }else{
      ""  
    }
  }
})
