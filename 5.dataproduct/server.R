

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
      preprocessedText <- preProcessString(text)
      if(nchar(preprocessedText)>0){
        #suggestion <- predictSuggest(text, ngramLookupTables, completeWord = endOfWord)
        suggestions <- predictTop(preprocessedText, ngramLookupTables, completeWord = endOfWord)
        log(suggestions[[1]])
        
        dataset <- suggestionsToTypeAhead(text,suggestions,endOfWord)
        
        tokens <- unname(sapply(dataset$sentence, strsplit, split=" "))
        session$sendCustomMessage(type = "updateSuggestions", list(
          id="text"
          ,placeholder="Type a sentence"
          ,local=dataset
          ,valueKey="sentence"
          ,tokens=tokens
          ,template = HTML("<p class='suggest'>{{suggestion}}</p>")
          )
        )
        paste(dataset[1,"sentence"])
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
      suggestions$sentence <- paste0(suggestions$sentence, suggestions$suggest)
    }else{
      suggestions$suggestion <- paste(suggestions$lookup, suggestions$suggest)
      suggestions$sentence <- paste(suggestions$sentence, suggestions$suggest)
    }
    
    suggestions[,c("suggestion", "sentence")]
  }
  
  
})
