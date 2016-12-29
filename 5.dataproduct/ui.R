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

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  tags$head(
    tags$script(src="extra.js"),
    tags$link(rel = "stylesheet", type = "text/css", href = "extra.css")
  ),
  
  mainPanel(
    titlePanel("Predictive Text"),
    textInput.typeahead(
      id="text"
      ,placeholder="Type a sentence"
      ,local=data.frame(name=c("name1","name2"))
      ,valueKey = "name"
      ,tokens=c(1,2)
      ,template = HTML("<p class='repo-name'>{{name}}</p>")
    ),
    br(),br(),
    verbatimTextOutput("suggest"),
    verbatimTextOutput("debug")
  )
  
))
