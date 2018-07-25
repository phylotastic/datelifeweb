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

 datelife.web <- reactive({
    web.citation <- paste0("wwww.datelife.web, entered on ", Sys.Date())
    web.citation
 })

 DateLifeCitations <- data.frame(x = "Sanchez-Reyes LL and O'Meara B. DateLife: Leveraging Databases to Reavel the Dated Tree of Life. In prep.",
                                       y = datelife.web())

 output$downloadDateLifeCitation <- downloadHandler(
   filename = "DateLifeCitations.csv",
   content = function(file) {
     write.csv(DateLifeCitations, file=file)
   }
 )
})
