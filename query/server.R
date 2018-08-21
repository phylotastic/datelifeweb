library(shiny)
library(datelife)
library(strap)
library(phangorn)
library(ape)
library(phyloch)
library(shinycssloaders)
options(shiny.sanitize.errors = FALSE)
# devtools::load_all("~/Desktop/datelife")
utils::data(opentree_chronograms)
utils::data("strat2012", package = "phyloch")
# source("allplots.R")
oma1_f <- function(tree){
  tipnum <- ape::Ntip(tree)
  if(tipnum == 2){
    oma1 <- 10  # tried to increase to 10.5, so numbers wont overlap with
    # axisGeo, but they still overlap. It appears to be an issue from axisGeo function
  } else if (tipnum == 3){
    oma1 <- 8
  } else if (tipnum == 4){
    oma1 <- 7
  } else if (tipnum >= 5 & tipnum <= 7){
    oma1 <- 6
  } else if (tipnum >= 8 & tipnum <= 10){
    oma1 <- 5
  } else {
    oma1 <- 4
  }
  oma1
}
tree.height <- function(tree){
  tipnum <- ape::Ntip(tree)
  if(tipnum > 10){
    hei <- 50 + (30 * tipnum)
  } else {
    hei <- 300
  }
  hei
}

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

    get.filtered.results <- reactive({get_datelife_result(input = input$taxa,
       partial = input$partial, use_tnrs = input$usetnrs,
       approximate_match = input$approximatematch,
       get_spp_from_taxon = input$highertaxon)
    })

    summ.table <- reactive({
       datelife::summarize_datelife_result(datelife_result = get.filtered.results(), summary_format = "data_frame", partial = input$partial)
    })

    output$age <- renderTable({
       summ.table()
    })

    get.consensus.tree <- reactive({
       temp.tree <- summarize_datelife_result(datelife_result = get.filtered.results(), summary_format = "phylo_median")
       temp.tree$root.time <- max(ape::branching.times(temp.tree))
       temp.tree
    })

    get.sdm.tree <- reactive({
       temp.tree <- summarize_datelife_result(datelife_result = get.filtered.results(), summary_format = "phylo_sdm")
       temp.tree$root.time <- max(ape::branching.times(temp.tree))
       temp.tree
    })

    get.all.trees <- reactive({
      noisy.trees <- summarize_datelife_result(datelife_result = get.filtered.results(), summary_format = "phylo_all")
      tree.vector <- c()
      tree.vector.names <- c()
      for (i in sequence(length(noisy.trees))) {
        if(class(noisy.trees[[i]]) == "phylo") {
          if(length(tree.vector) > 0) {
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

    max.tree.age <- reactive({
      x <- list(get.consensus.tree())
      x <- c(x, list(get.sdm.tree()))
      x <- c(x, get.all.trees())
      max.age <- round(max(unlist(sapply(x, ape::branching.times))) + 5, digits = -1)
      max.age
    })

    max.tip.label <- reactive({
      x <- list(get.consensus.tree())
      x <- c(x, list(get.sdm.tree()))
      x <- c(x, get.all.trees())
      tip.label.length <- unique(unlist(sapply(x, "[", "tip.label")))
      ind <- which.max(nchar(tip.label.length))
      nchar(tip.label.length[ind])  # use strWidth?
    })


   output$medianPlot <- renderPlot({
     mar.tips <- max.tip.label() * 0.6  # to control the margin on the side of tip labels
     median.tree <- get.consensus.tree()
     max.depth <- max.tree.age()
     median.tree$root.edge <- max.depth - max(ape::branching.times(median.tree))
     par(xpd = TRUE)
     par(oma = c(oma1_f(median.tree),0,0,0))  #
     par(mar = c(2,0,2,mar.tips))
     # plot_chronogram.phylo(median.tree, cex = 1.5, edge.width = 2, label.offset = 0.5,
       # x.lim = c(0, max.depth), root.edge = TRUE, root.edge.color = "white")
     ape::plot.phylo(median.tree, cex = 1.5, edge.width = 2, label.offset = 0.5,
       x.lim = c(0, max.depth), root.edge = TRUE)
     phyloch::axisGeo(GTS = strat2012, unit = c("period","epoch"),
       col = c("gray80", "white"), gridcol = c("gray80", "white"), cex = 1.5,
       gridty = "twodash")
     mtext("Time (MYA)", cex = 1.5, side = 1, font = 2, line = oma1_f(median.tree) - 1, outer = TRUE)
     }, height = function(){
           tree <- get.consensus.tree()
           hei <- tree.height(tree)
           hei
        }
    )

    output$sdmPlot <- renderPlot({
      mar.tips <- max.tip.label() * 0.6
      sdm.tree <- get.sdm.tree()
      max.depth <- max.tree.age()
      sdm.tree$root.edge <- max.depth - max(ape::branching.times(sdm.tree))
      par(xpd = TRUE)
      par(oma = c(oma1_f(sdm.tree),0,0,0))  #
      par(mar = c(2,0,2,mar.tips))
      # plot_chronogram.phylo(median.tree, cex = 1.5, edge.width = 2, label.offset = 0.5,
        # x.lim = c(0, max.depth), root.edge = TRUE, root.edge.color = "white")
      ape::plot.phylo(sdm.tree, cex = 1.5,
        edge.width = 2, label.offset = 0.5, x.lim = c(0, max.depth), root.edge = TRUE)
      mtext("Time (MYA)", cex = 1.5, side = 1, font = 2, line = oma1_f(sdm.tree) - 1, outer = TRUE)
      phyloch::axisGeo(GTS = strat2012, unit = c("period","epoch"),
        col = c("gray80", "white"), gridcol = c("gray80", "white"), cex = 1.5,
        gridty = "twodash")
      }, height = function(){
            tree <- get.sdm.tree()
            hei <- tree.height(tree)
            hei
         }
     )


    output$densiTreePlotAll <- renderPlot({
       all.trees <- get.all.trees()
       mar.tips <- max.tip.label() * 0.6
       par(xpd = TRUE)
       par(oma = c(4,0,0,0))  #
       # par(mai = c(1,1,1,2))
       par(mar = c(2,0,2,0))
       max.depth <- max.tree.age()
       all.trees <- lapply(all.trees, function(x) {
           x$root.edge <- max.depth - max(ape::branching.times(x))
           x
         }
       )
       # densiTree function only works with trees with more than two tips when consensus tree is not provided
       # so we did our own function in datelife:
       plot_densitree(trees = all.trees, include_all = TRUE, cex = 1.5, edge.width = 2, label.offset = 0.01)
       mtext("Time (MYA)", cex = 1.5, side = 1, font = 2, line = 2, outer = TRUE)
     }, height = function(){
       tree <- get.consensus.tree()
       hei <- tree.height(tree)
       hei
     }
   )

   output$allPlots <- renderUI({
     plot_output_list <- vector(mode = "list")
     for ( i in 1:length(get.all.trees()) ){
       plottitlename <- paste0("plot_title", i)
       plot_output_list <- c(plot_output_list, list(h4(textOutput(plottitlename))))
       plotname <- paste0("plot", i)
       plot_output_list <- c(plot_output_list, list(withSpinner(ui_element = plotOutput(plotname, height = "auto"), type = 4)))
       plot_output_list <- c(plot_output_list, list(br()), list(br()), list(br()))  # three blank rows between plots
     }
     # Convert the list to a tagList - this is necessary for the list of items
     # to display properly.
     # Call renderPlot for each one. Plots are only actually generated when they
     # are visible on the web page.
     i <- 1
     max_n <- length(get.all.trees())
     for (i in 1:max_n) {
       # Need local so that each item gets its own number. Without it, the value
       # of i in the renderPlot() will be the same across all instances, because
       # of when the expression is evaluated.
       local({
         my_i <- i  # this is important
         plotname <- paste("plot", my_i, sep="")
         plottitlename <- paste0("plot_title", my_i)
         output[[plottitlename]] <- renderText({
           names(get.all.trees())[my_i]
           # paste("plot_", i)
         })
         output[[plotname]] <- renderPlot({
           all.trees <- get.all.trees()
           max_n <- length(all.trees)
           tree <- all.trees[[my_i]]
           max.depth <- max.tree.age()
               mar.tips <- max.tip.label() * 0.6
               par(xpd = TRUE)
               par(oma = c(oma1_f(tree), 0, 0, 0))  #
               par(mar = c(2, 0, 2, mar.tips))
               # max.depth <- max.tree.age()
               # tree <- get.all.trees()[[1]]
               tree$root.edge <- max.depth - max(ape::branching.times(tree))
               # tip.label.area <- max.tip.label()
               # plot_chronogram.phylo(median.tree, cex = 1.5, edge.width = 2, label.offset = 0.5,
                 # x.lim = c(0, max.depth), root.edge = TRUE, root.edge.color = "white")
               ape::plot.phylo(tree, cex = 1.5,
                 edge.width = 2, label.offset = 0.5, x.lim = c(0, max.depth), root.edge = TRUE)
               mtext("Time (MYA)", cex = 1.5, side = 1, font = 2, line = oma1_f(tree) - 1, outer = TRUE)
               phyloch::axisGeo(GTS = strat2012, unit = c("period","epoch"),
                 col = c("gray80", "white"), gridcol = c("gray80", "white"), cex = 1.5,
                 gridty = "twodash")
           }, height = function(){
                 all.trees <- get.all.trees()
                 tree <- all.trees[[my_i]]
                 hei <- tree.height(tree)
                 hei
             }
          )
       })
     }
     do.call(tagList, plot_output_list)
   })

   output$downloadCSV <- downloadHandler(
     filename = "DatelifeTable.csv",
     content = function(file) {
       write.csv(summarize_datelife_result(datelife_result = get.filtered.results(), summary_format = "data_frame"), file = file)
     }
   )

   output$downloadMedian <- downloadHandler(
     filename = "SummaryTreeMedian.tre",
     content = function(file) {
       write.tree(phy = get.consensus.tree(), file = file)
     }
   )

   output$downloadSDM <- downloadHandler(
     filename = "SummaryTreeSDM.tre",
     content = function(file) {
       write.tree(phy = get.sdm.tree(), file = file)
     }
   )

   output$downloadCitations <- downloadHandler(
     filename = "AllTreesCitations.txt",
     content = function(file) {
       write(unique(summarize_datelife_result(datelife_result = get.filtered.results(), summary_format = "citations")), file = file)
     }
   )

   outputOptions(output, "age", suspendWhenHidden = FALSE)
   outputOptions(output, "medianPlot", suspendWhenHidden = FALSE)
   outputOptions(output, "sdmPlot", suspendWhenHidden = FALSE)
   outputOptions(output, "densiTreePlotAll", suspendWhenHidden = FALSE)
   outputOptions(output, "allPlots", suspendWhenHidden = FALSE)
})
