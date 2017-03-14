library(shiny)

shinyUI(fluidPage(
 h2("Citations"),
 tableOutput("citations"),
 h2("Downloads"),
downloadLink('downloadCSV', 'table in csv format')

))
