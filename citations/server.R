library(shiny)
library(datelife)
library(strap)
library(ape)
library(phyloch)
data(opentree_chronograms)

shinyServer(function(input, output, session) {

 utils::data(opentree_chronograms)
 curators <- rep(NA, length(opentree_chronograms$curators))
 for (i in sequence(length(curators))) {
   curators[i] <- paste(unlist(opentree_chronograms$curators[[i]]), collapse=" & ")
 }
 authors <- rep(NA, length(opentree_chronograms$authors))
 for (i in sequence(length(authors))) {
   authors[i] <- paste(unlist(opentree_chronograms$authors[[i]]), collapse=" & ")
 }
 citation.info <- data.frame(reference=paste(names(opentree_chronograms$trees)), DOI = paste(opentree_chronograms$dois), curator=curators, author=authors, stringsAsFactors=FALSE)
 citation.html <- citation.info

 ConvertToAHref <- function(x) {
   return(paste("<a href='", x, "'>",x, "</a>", sep=""))
 }
#citation.html$DOI <- ConvertToAHref(citation.html$DOI )

 output$citations <- renderTable(citation.html, output.format = "data.frame")

 output$downloadcitationCSV <- downloadHandler(
   filename = "citations.csv",
   content = function(file) {
     write.csv(citation.info, file=file)
   }
 )
})
