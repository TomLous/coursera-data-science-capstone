

verbose <<- TRUE
debug <- TRUE


library(shiny)
#library(shinyjs)


options(shiny.trace = debug)
source("utils.R", local = TRUE)
load("data/ngramLookupTables.en_US.RData")


shinyServer(function(input, output, session) {
  output$suggest <- renderText({
    updateTypeahead(input$text)
  })
  
  output$debug = renderText({
    updateTypeahead(input$typedText)
  })
  
  updateTypeahead <- function(text){
    log(paste("input:",text))
    if(!is.null(text) && nchar(text)>0){
      endOfWord <- grepl("[^a-z]$", text)
      text <- preProcessData(text)
      if(nchar(text)>0){
        #suggestion <- predictSuggest(text, ngramLookupTables, completeWord = endOfWord)
        suggestions <- predictTop(text, ngramLookupTables, completeWord = endOfWord)
        #update_typeahead
        session$sendCustomMessage(type = "updateSuggestions", suggestionsToTypeAhead(text,suggestions,endOfWord))
        #paste(suggestion$lookup, suggestion$suggest)
      }else{
        ""
      }
    }else{
      ""  
    }
  }
  
  suggestionsToTypeAhead <- function(text, suggestions, endOfWord){
    suggestions$sentence <- sapply(suggestions$lookup, concatWithOverlap, str1=text)
    if(endOfWord){
      suggestions$suggestion <- suggestions$suggest
    }else{
      suggestions$suggestion <- paste(suggestions$lookup, suggestions$suggest)
    }
    suggestions$token <- as.numeric(rownames(suggestions))
    suggestions[,c("token","suggestion", "sentence")]
  }
  
  
})
