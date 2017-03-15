library(shiny)
library(datelife)
library(strap)
library(phangorn)
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
 get.filtered.results <- reactive({GetFilteredResults(input=input$taxa, partial=input$partial, usetnrs=input$usetnrs, approximatematch=input$approximatematch)})
 get.consensus.tree <- reactive({
   temp.tree <- SummarizeResults(get.filtered.results(), output.format="phylo.median")
   temp.tree$root.time <- max(branching.times(temp.tree))
   temp.tree
   })
get.all.trees <- reactive({
  noisy.trees <- SummarizeResults(get.filtered.results(), output.format="phylo.all")
  tree.vector <- c()
  tree.vector.names <- c()
  for (i in sequence(length(noisy.trees))) {
    if(class(noisy.trees[[i]])=="phylo") {
      if(length(tree.vector)>0) {
        tree.vector <- ape::c.phylo(tree.vector, noisy.trees[[i]])
        tree.vector.names <- c(tree.vector.names, names(noisy.trees)[i])
      } else {
        tree.vector <- ape::c.phylo(noisy.trees[[i]])
        tree.vector.names <- names(noisy.trees)[i]
      }
    }
  }
  names(tree.vector) <- tree.vector.names
  tree.vector
  })
 output$age <- renderTable(SummarizeResults(get.filtered.results(), output.format="data.frame"))
 output$medianPlot <- renderPlot(strap::geoscalePhylo(get.consensus.tree(), cex.tip=2, cex.ts=2, cex.age=2, units=c("Era", "Period"), boxes="Period"))
 output$allPlot <- renderPlot({
   all.trees <- get.all.trees()
   max.depth <- max(sapply(all.trees, ape::branching.times))
   par(mfcol=c(length(all.trees), 1))
   for (i in sequence(length(all.trees))) {
     local.tree <- all.trees[[i]]
     ape::plot.phylo(local.tree, main=paste(strwrap(names(all.trees)[i]), collapse="\n"))
     ape::axisPhylo()
   }
  }, width=600, height=2000)

 output$downloadCSV <- downloadHandler(
   filename = "DatelifeTable.csv",
   content = function(file) {
     write.csv(SummarizeResults(get.filtered.results(), output.format="data.frame"), file)
   }
 )
})
