library(shiny)

shinyUI(fluidPage(
 h2("Citations"),
 downloadLink('downloadcitationCSV', 'table in csv format'),
 tableOutput("citations")
))
