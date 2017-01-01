#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
if(!"shinysky" %in% rownames(installed.packages())){
  devtools::install_github("ShinySky","AnalytixWare")
}


library(shiny)
library(shinysky)
#library(shinyjs)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  #useShinyjs(),
  tags$head(
    tags$script(src="extra.js"),
    tags$link(rel = "stylesheet", type = "text/css", href = "extra.css")
  ),
  
  mainPanel(
    titlePanel("KILOS™ Predictive Text"),
    p("Below is a text input based on a simplified predictive text model using n-grams. It allows for non-character input, but works better without."),
    p("The tool is named KILOS™ because it contains thousands of grams."),
    p("Use up/down arrow keys & enter to select a suggestion or the right arrow key to tigger the autocomplete."),
    textInput.typeahead(
      id="text"
      ,placeholder="Type a sentence"
      ,local=data.frame(sentence=c(),suggestion=c())
      ,valueKey="sentence"
      ,tokens=c()
      ,template = HTML("<p class='suggest'>{{suggestion}}</p>")
    )
    ,br(),br(),
    verbatimTextOutput("suggest"),
    verbatimTextOutput("debug")
  )
  
))
