library(shiny)
library(datelife)
library(strap)
library(ape)

shinyServer(function(input, output, session) {

data(contributorcache)
 output$authors<- renderTable(author.pretty, output.format="data.frame")
 output$curators<- renderTable(curator.pretty, output.format="data.frame")
})
