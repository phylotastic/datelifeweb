library(shiny)
library(datelife)
library(strap)
library(ape)
data(opentree_chronograms)

shinyServer(function(input, output, session) {

utils::data(opentree_chronograms)
citation.info <- data.frame(reference=names(opentree_chronograms$trees), dois=opentree_chronograms$dois, curators=opentree_chronograms$curators, authors=opentree_chronograms$authors, stringsAsFactors=FALSE)



 output$citations <- renderTable(citation.info, output.format="data.frame"))


 output$downloadCSV <- downloadHandler(
   filename = "citations.csv",
   content = function(file) {
     write.csv(citation.info, output.format="data.frame"), file)
   }
 )
})
