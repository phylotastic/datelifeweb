library(shiny)
library(datelife)
library(strap)
library(phangorn)
library(ape)
library(phyloch)
library(shinycssloaders)

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

    get.filtered.results <- reactive({get_datelife_result(input = input$taxa,
       partial = input$partial, use_tnrs = input$usetnrs,
       approximate_match = input$approximatematch,
       get_spp_from_taxon = input$highertaxon)
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

    summ.table <- reactive({
       summarize_datelife_result(datelife_result = get.filtered.results(), summary_format = "data_frame", partial = input$partial)
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

    output$age <- renderTable({
       summ.table()
    })

   output$medianPlot <- renderPlot({
     mar.tips <- max.tip.label() * 0.6  # to control the margin on the side of tip labels
     par(xpd = TRUE)
     par(oma = c(8,0,0,0))  #
     # par(mai = c(1,1,1,2))
     par(mar = c(2,0,2,mar.tips))
     median.tree <- get.consensus.tree()
     max.depth <- max.tree.age()
     median.tree$root.edge <- max.depth - max(ape::branching.times(median.tree))
     # tip.label.area <- max.tip.label()
     ape::plot.phylo(median.tree, cex = 1.5,
       edge.width = 2, label.offset = 0.5, x.lim = c(0, max.depth), root.edge = TRUE)
     data("strat2012", package = "phyloch")
     phyloch::axisGeo(GTS = strat2012, unit = c("period","epoch"),
       col = c("gray80", "white"), gridcol = c("gray80", "white"), cex = 1.5,
       gridty = "twodash")
     mtext("Time (MYA)", cex = 1.5, side = 1, font = 2, line = 7, outer = TRUE)
     }, height = function(){
          tipnum <- ape::Ntip(get.consensus.tree())
            if(tipnum > 10){
              hei <- 50 + (30 * tipnum)
            } else {
              hei <- 300
            }
          hei
        }
    )
    output$sdmPlot <- renderPlot({
      mar.tips <- max.tip.label() * 0.6
      par(xpd = TRUE)
      par(oma = c(8,0,0,0))  #
      # par(mai = c(1,1,1,2))
      par(mar = c(2,0,2,mar.tips))
      sdm.tree <- get.sdm.tree()
      max.depth <- max.tree.age()
      sdm.tree$root.edge <- max.depth - max(ape::branching.times(sdm.tree))
      # tip.label.area <- max.tip.label()
      ape::plot.phylo(sdm.tree, cex = 1.5,
        edge.width = 2, label.offset = 0.5, x.lim = c(0, max.depth), root.edge = TRUE)
      mtext("Time (MYA)", cex = 1.5, side = 1, font = 2, line = 7, outer = TRUE)
      data("strat2012", package = "phyloch")
      phyloch::axisGeo(GTS = strat2012, unit = c("period","epoch"),
        col = c("gray80", "white"), gridcol = c("gray80", "white"), cex = 1.5,
        gridty = "twodash")
      # mtext("Time (MYA)", side = 1, font = 2, line = 8, outer = TRUE)  #
                 # box("plot", col = "red")
                 # box("figure", col = "green")
                 # # box("inner", col = "purple")
                 # box("outer", col = "blue")

      }, height = function(){
           tipnum <- ape::Ntip(get.consensus.tree())
             if(tipnum > 6){
               hei <- 50 + (20 * tipnum)
             } else {
               hei <- 300
             }
           hei
         }
     )
     output$densiTreePlot <- renderPlot({
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
        # densiTree function only works with trees with more than two tips:
        all.trees <- all.trees[sapply(all.trees, function (x) ape::Ntip(x)) > 2]
        class(all.trees) <- "multiPhylo"
        phangorn::densiTree(x = all.trees, cex = 1.5, edge.width = 2, label.offset = 0.01)
        mtext("Time (MYA)", cex = 1.5, side = 1, font = 2, line = 3, outer = TRUE)
        # data("strat2012", package = "phyloch")
        # phyloch::axisGeo(GTS = strat2012, unit = c("period","epoch"),
        #                  col = c("gray80", "white"), gridcol = c("gray80", "white"), cex = 1.5,
        #                  gridty = "twodash")
        # # mtext("Time (MYA)", side = 1, font = 2, line = 8, outer = TRUE)  #
        # box("plot", col = "red")
        # box("figure", col = "green")
        # # box("inner", col = "purple")
        # box("outer", col = "blue")
      }, height = function(){
        tipnum <- ape::Ntip(get.consensus.tree())
        if(tipnum > 10){
          hei <- 50 + (30 * tipnum)
        } else {
          hei <- 300
        }
        hei
      }
    )

  #  output$allPlot <- reactive({
  #    all.trees <- get.all.trees()
  #    max.depth <- max(unlist(sapply(all.trees, ape::branching.times)))  # max(sapply(all.trees, function (x) {max(ape::branching.times(x))}))
  #    # par(mfcol=c(length(all.trees), 1), xpd = TRUE)
  #    graph.height <- 100 + unlist(sapply(all.trees, ape::Ntip)) * 50
  #    for (i in sequence(length(all.trees))){
  #      renderPlot({
  #        par(xpd = TRUE)
  #        par(oma=c(0,0,6,0))  #
  #        par(mar=c(0,0,0,0))
  #        par(mai=c(1,1,0.25,3))
  #        ape::plot.phylo(all.trees[[i]], main = "", cex = 2,
  #            edge.width = 2, label.offset = 1, x.lim = c(0, round(max.depth + 5, digits = -1)))
  #            ape::axisPhylo(cex.lab = 2, cex = 0.5)
  #            mtext (paste(strwrap(names(all.trees)[i]), collapse = "\n"), side = 3, outer = TRUE, line = 0)
  #            mtext("Time (myrs)", side = 1, font = 2, line = 3, at = max.depth/2)
  #            box("plot", col = "red")
  #            box("figure", col = "green")
  #            box("inner", col = "purple")
  #            box("outer", col = "blue")
  #      }, height = graph.height[i])
  #    }
  # })
   # for (i in sequence(length(all.trees))){
   #   iplot <- renderPlot({
   #     ape::plot.phylo(all.trees[[i]], main = paste(strwrap(names(all.trees)[i]), collapse = "\n"), cex = 2,
   #     edge.width = 2, label.offset = 2, x.lim = c(0, ceiling(max.depth)))
   #     ape::axisPhylo(cex.lab = 2, cex = 2)
   #     mtext("Time (myrs)", side = 1, font = 2, line = 2)
   #     box("figure", col = "green")
   #     box("outer", col = "blue")
   #   })
   #    output$allPlot <- c( output$allPlot, list(iplot))
   # }
    all.trees.height <- function(){
      all.trees <- get.all.trees()
      x <- sum(unlist(sapply(all.trees, ape::Ntip)))
      return((100 * length(all.trees)) + (x * 30))
    }

   output$allPlot <- renderPlot({
     all.trees <- get.all.trees()
     max.depth <- round(max(unlist(sapply(all.trees, ape::branching.times))) + 5, digits = -1) # max(sapply(all.trees, function (x) {max(ape::branching.times(x))}))
     par(mfcol=c(length(all.trees), 1))
            par(xpd = TRUE)
            par(oma=c(0,0,3,0))  #
            par(mar=c(0,0,0,0))
            par(mai=c(1,1,1,2))
     for (i in sequence(length(all.trees))) {
       local.tree <- all.trees[[i]]
       ape::plot.phylo(local.tree, main = "", cex = 2,
       edge.width = 2, label.offset = 1, x.lim = c(0, max.depth))
       ape::axisPhylo(cex.lab = 2, cex = 2)
       mtext (paste(strwrap(names(all.trees)[i]), collapse = "\n"), side = 3, outer = F, line = 1, at = max.depth/2)
       mtext("Time (MYA)", side = 1, font = 2, line = 3, at = max.depth/2)
       # box("figure", col = "gray", bty = "u")
     }
     box("outer", col = "gray")

    }, height = all.trees.height)

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

})
