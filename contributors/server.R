datelife.exists <- suppressWarnings(library("datelife", quietly = TRUE, logical.return=TRUE))
if(!datelife.exists) {
  devtools::install_github("phylotastic/datelife")
  library(datelife)
}
library(shiny)
library(strap)
library(ape)

shinyServer(function(input, output, session) {

data(contributorcache)
data(treebasecache)
 output$authors<- renderDataTable(author.pretty, options = list(lengthMenu = list(c(25, 50, 100,-1), c("25", "50", "100", "All")) , order = list(1, 'desc')))
 output$curators<- renderDataTable(curator.pretty, options = list(lengthMenu = list(c(25, 50, 100,-1), c("25", "50", "100", "All")), order = list(1, 'desc')))
 output$tbauthors<- renderDataTable(tb.author.pretty, options = list(lengthMenu = list(c(25, 50, 100,-1), c("25", "50", "100", "All")) , order = list(1, 'desc')))
 output$downloadauthorsCSV <- downloadHandler(
   filename = "authors.csv",
   content = function(file) {
     write.csv(author.pretty, file=file)
   }
 )
 output$downloadcuratorsCSV <- downloadHandler(
   filename = "curators.csv",
   content = function(file) {
     write.csv(curator.pretty, file=file)
   }
 )
 output$downloadtbauthorsCSV <- downloadHandler(
   filename = "treebase_authors.csv",
   content = function(file) {
     write.csv(tb.author.pretty, file=file)
   }
 )
})
