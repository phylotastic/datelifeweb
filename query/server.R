
# from http://stackoverflow.com/questions/32872222/how-do-you-pass-parameters-to-a-shiny-app-via-url
# we actually do not need that, the input will be automatically updated
# example: 127.0.0.1:5767/?symbol=BBB,AAA,CCC,DDD&date_start=2005-01-02&period_select=2&smaLen=153&usema=1


shinyServer(function(input, output, session) {
  output$dimension_display <- renderText({
      paste(input$dimension[1], input$dimension[2], input$dimension[2]/input$dimension[1])
  })
      rv <- reactiveValues(input_taxa = isolate(input$taxa),
                          input_partial = isolate(input$partial),
                          input_usetnrs = isolate(input$usetnrs),
                          input_approximatematch = isolate(input$approximatematch),
                          input_highertaxon = isolate(input$highertaxon),
                          input_dim1 = isolate(input$dimension[1])#, redraw = FALSE
                          )

      # from https://stackoverflow.com/questions/31051133/how-do-i-make-sure-that-a-shiny-reactive-plot-only-changes-once-all-other-reacti
      # observe({
      #     invalidateLater(1500, session)
      #     rv$input_dim1 <- input$dimension[1]
      #     if(rv$redraw){
      #
      #     }
      #     rv$redraw <- FALSE
      # })


      # debounce(r = observe({
      #     rv$input_dim1 <- input$dimension[1]
      #   }), millis = 1000)

      # wait one second after modifying window to update plot width value
      tree_plot_wid <- reactive({
        input$dimension[1] * 0.95
      }) %>% debounce(1000)  # requires stringr

      # slightly slower:
      # tree_plot_wid <- debounce(r = reactive({
      #   input$dimension[1] * 0.95
      # }), 1000)

      observeEvent(input$refresh, {
          query <- parseQueryString(session$clientData$url_search)

          for (i in 1:(length(reactiveValuesToList(input)))) {
            nameval = names(reactiveValuesToList(input)[i])
            valuetoupdate = query[[nameval]]
            if (!is.null(valuetoupdate)) {
              if (is.na(as.numeric(valuetoupdate))) {
                updateTextInput(taxa, nameval, value = valuetoupdate)
              }
              else {
                updateTextInput(taxa, nameval, value = as.numeric(valuetoupdate))
              }
            }
          }
          rv$input_taxa <- input$taxa
          rv$input_partial <- input$partial
          rv$input_usetnrs <- input$usetnrs
          rv$input_approximatematch <- input$approximatematch
          rv$input_highertaxon <- input$highertaxon
      })



      get.filtered.results <- reactive({get_datelife_result(input = rv$input_taxa,  #input$taxa,
         partial = rv$input_partial, use_tnrs = rv$input_usetnrs,
         approximate_match = rv$input_approximatematch,
         get_spp_from_taxon = rv$input_highertaxon)
      })

      summ.table <- reactive({
         datelife::summarize_datelife_result(datelife_result = get.filtered.results(), summary_format = "data_frame", partial = rv$input_partial)
      })

      get.median.tree <- reactive({
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
        max.age <- round(max(summarize_datelife_result(datelife_result = get.filtered.results(),
            summary_format = "mrca")) + 5, digits = -1)
        max.age
      })

      max.tip.label <- reactive({
        tip.label.length <- unique(unlist(sapply(
          c(list(get.median.tree()), list(get.sdm.tree()), get.all.trees()),
          "[", "tip.label")))
        ind <- which.max(nchar(tip.label.length))
        nchar(tip.label.length[ind])  # use strWidth?
      })

      output$age <- renderTable({
          summ.table()
      })

      output$medianPlot <- renderPlot({
          mar.tips <- max.tip.label() * 0.6  # to control the margin on the side of tip labels
          median.tree <- get.median.tree()
          max.depth <- max.tree.age()
          median.tree$root.edge <- max.depth - max(ape::branching.times(median.tree))
          par(xpd = TRUE)
          par(oma = c(oma1_f(median.tree),0,0,0))  #
          par(mar = c(2,0,2,mar.tips))
          ape::plot.phylo(median.tree, cex = 1.5, edge.width = 2, label.offset = 0.5,
           x.lim = c(0, max.depth), root.edge = TRUE)
          phyloch::axisGeo(GTS = strat2012, unit = c("period","epoch"),
           col = c("gray80", "white"), gridcol = c("gray80", "white"), cex = 1.5,
           gridty = "twodash")
          mtext("Time (MYA)", cex = 1.5, side = 1, font = 2, line = oma1_f(median.tree) - 1, outer = TRUE)
          }, height = function(){
               tree <- get.median.tree()
               hei <- tree_plot_height(tree)
               hei
          }, width = function() {
               wid <- tree_plot_wid() # in pixels # function() 0.9 * isolate({input$dimension[1]})
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
                hei <- tree_plot_height(tree)
                hei
          }, width = 900
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
           })
          # densiTree function only works with trees with more than two tips when consensus tree is not provided
          # so we did our own function in datelife:
          plot_densitree(trees = all.trees, include_all = TRUE, cex = 1.5, edge.width = 2, label.offset = 0.01)
          mtext("Time (MYA)", cex = 1.5, side = 1, font = 2, line = 2, outer = TRUE)
          }, height = function(){
                tree <- get.median.tree()
                hei <- tree_plot_height(tree)
                hei
          }
      )

      output$allPlots <- renderUI({
          plot_output_list <- vector(mode = "list")
          for (i in 1:length(get.all.trees())){
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
                       tree$root.edge <- max.depth - max(ape::branching.times(tree))
                       ape::plot.phylo(tree, cex = 1.5,
                         edge.width = 2, label.offset = 0.5, x.lim = c(0, max.depth), root.edge = TRUE)
                       mtext("Time (MYA)", cex = 1.5, side = 1, font = 2, line = oma1_f(tree) - 1, outer = TRUE)
                       phyloch::axisGeo(GTS = strat2012, unit = c("period","epoch"),
                         col = c("gray80", "white"), gridcol = c("gray80", "white"), cex = 1.5,
                         gridty = "twodash")
                   }, height = function(){
                         all.trees <- get.all.trees()
                         tree <- all.trees[[my_i]]
                         hei <- tree_plot_height(tree)
                         hei
                   })
              })
          }
          do.call(tagList, plot_output_list)
      })

      output$downloadCSV <- downloadHandler(
          filename = "DatelifeTable.csv",
          content = function(file) {
              write.csv(summ.table(), file = file)
          }
      )

      output$downloadMedian <- downloadHandler(
          filename = "SummaryTreeMedian.tre",
          content = function(file) {
              write.tree(phy = get.median.tree(), file = file)
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
              write(unique(summarize_datelife_result(datelife_result = get.filtered.results(),
              summary_format = "citations")), file = file)
          }
      )
      outputOptions(output, "medianPlot", suspendWhenHidden = FALSE)
      outputOptions(output, "sdmPlot", suspendWhenHidden = FALSE)
      # outputOptions(output, "densiTreePlotAll", suspendWhenHidden = FALSE)
      # outputOptions(output, "allPlots", suspendWhenHidden = FALSE)
})
