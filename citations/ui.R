library(shiny)

shinyUI(fluidPage(
 h2("Citations"),
 downloadLink('downloadcitationCSV', 'table in csv format'),
 br(),
 downloadLink('downloadDateLifeCitations', 'How to cite DateLife, in csv format'),
 br(),
 tableOutput("citations")
))
