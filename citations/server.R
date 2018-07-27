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
 citation.info <- data.frame(Reference = paste(names(opentree_chronograms$trees)),
 DOI = paste(opentree_chronograms$dois), Curators = curators, Authors = authors, stringsAsFactors=FALSE)
 citation.html <- citation.info

 ConvertToAHref <- function(x) {
   return(paste("<a href='", x, "'>",x, "</a>", sep=""))
 }
#citation.html$DOI <- ConvertToAHref(citation.html$DOI )

 output$citations <- renderTable(citation.html)  # removed , output.format = "data.frame"

 output$downloadcitationCSV <- downloadHandler(
   filename = "citations.csv",
   content = function(file) {
     write.csv(citation.info, file=file)
   }
 )

 datelifeweb.cit <- reactive({
   now <- Sys.Date()
   now <- format(now, format="%B %d %Y")
   web.citation <- paste0("DateLife Web (entered on ", now, ") at wwww.datelife.org")
   web.citation
 })

 output$datelife.citations <- renderTable({
   datelifeweb.reference <- datelifeweb.cit()
   # datelifepackage.citation <- citation(package = "datelife")
   datelife.citation.info <-  data.frame(Reference = c("Sanchez-Reyes LL, O'Meara B. DateLife: Leveraging Databases to Reavel the Dated Tree of Life. In prep.", "datelifepackage.citation", datelifeweb.reference),
            DOI = c("doi:", "doi:", "doi:"), Authors = c("Sanchez-Reyes LL & O'Meara B", "O'Meara B, Sanchez-Reyes LL, ...", "O'Meara B, Sanchez-Reyes LL, ..."), stringsAsFactors=FALSE)
   datelife.citation.info
   }) # removed output.format = "data.frame"

 output$downloadDateLifeCitations <- downloadHandler(
   filename = "DateLifeCitations.csv",
   content = function(file) {
     write.csv(datelife.citation.info, file=file)
   }
 )
})
