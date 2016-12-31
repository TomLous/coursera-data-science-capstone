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
    titlePanel("Predictive Text"),
    textInput.typeahead(
      id="text"
      ,placeholder="Type a sentence"
      ,local=data.frame(sentence=c(),suggestion=c())
      ,valueKey="sentence"
      ,tokens=c()
      ,template = HTML("<p class='suggest'>{{suggestion}}</p>")
    ),
    br(),br(),
    verbatimTextOutput("suggest"),
    verbatimTextOutput("debug")
  )
  
))
