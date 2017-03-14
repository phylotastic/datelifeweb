library(shiny)
library(datelife)
library(strap)
library(ape)
data(opentree_chronograms)

#from http://stackoverflow.com/questions/32872222/how-do-you-pass-parameters-to-a-shiny-app-via-url

# example: 127.0.0.1:5767/?symbol=BBB,AAA,CCC,DDD&date_start=2005-01-02&period_select=2&smaLen=153&usema=1


shinyServer(function(input, output, session) {



observe({
 query <- parseQueryString(session$clientData$url_search)

 for (i in 1:(length(reactiveValuesToList(input)))) {
  nameval = names(reactiveValuesToList(input)[i])
  valuetoupdate = query[[nameval]]

  if (!is.null(query[[nameval]])) {
    if (is.na(as.numeric(valuetoupdate))) {
      updateTextInput(taxa, nameval, value = valuetoupdate)
    }
    else {
      updateTextInput(taxa, nameval, value = as.numeric(valuetoupdate))
    }
  }

 }

 })
 get.filtered.results <- reactive({GetFilteredResults(input=input$taxa, partial=input$partial)})
 get.consensus.tree <- reactive({
   temp.tree <- SummarizeResults(get.filtered.results(), output.format="phylo.median")
   temp.tree$root.time <- max(branching.times(temp.tree))
   temp.tree
   })

 tryCatch({output$age <- renderTable(SummarizeResults(get.filtered.results(), output.format="data.frame"))})
 tryCatch({output$medianPlot <- renderPlot(geoscalePhylo(get.consensus.tree(), cex.tip=2, cex.ts=2, cex.age=2, units=c("Era", "Period"), boxes="Period"))})
 tryCatch({output$citations <- dump.citations()})

 tryCatch({output$downloadCSV <- downloadHandler(
   filename = "DatelifeTable.csv",
   content = function(file) {
     write.csv(SummarizeResults(get.filtered.results(), output.format="data.frame"), file)
   }
 )})
})
