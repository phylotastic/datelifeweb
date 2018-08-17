library(shiny)
library(shinycssloaders)
# source("allplots.R")
text_width <- "100%"

shinyUI(fluidPage(
  # work around textAreaInput not spanning 100% of webpage when width = 100%
  tags$style(HTML("
    .shiny-input-container:not(.shiny-input-container-inline) {
    width: 100%;
  }")),
  tags$head(includeScript("google-analytics.js")),
  h1("DateLife Query"),
  # textInput does not allow controlling height of box.
  # textInput("taxa", "Taxa (comma delimited; spaces or underscores in binomials are ok) or a Tree (Newick format, make sure to end with a semicolon)",
  # "Rhea americana, Pterocnemia pennata, Struthio camelus", width = text_width),
  textAreaInput("taxa", "Taxa (comma delimited; spaces or underscores in binomials are ok) or a Tree (Newick format, make sure to end with a semicolon)",
  "Rhea americana, Pterocnemia pennata, Struthio camelus", width = text_width, height = "auto"),
   checkboxInput('partial',
   'Show all studies. Includes studies with a subset of desired taxa, perhaps resulting in underestimate of maximum age',
   TRUE, width = text_width),
   # br(),
   checkboxInput('highertaxon', 'Search species within higher taxa. Several higher taxa can be searched at a time, comma separated',
   TRUE, width = text_width),
   # br(),
   checkboxInput('usetnrs', 'Use name resolution to deal with changes in taxonomy or misspellings (slows run considerably)',
   FALSE, width = text_width),
   # br(),
   checkboxInput('approximatematch', 'Use approximate matching for name resolution (can handle mismatches, but makes runs MUCH slower)',
   FALSE, width = text_width),
   br(),

   h1("DateLife Results, yay!"),
   # h3("Downloads"),
   # downloadLink('downloadCSV', 'Table in csv format, including Newick of trees'),
   # br(),
   # downloadLink('downloadMedian', 'Summary tree from median of ages, in Newick format'),
   # br(),
   # downloadLink('downloadSDM', 'Summary tree from supertree method, in Newick format'),
   # br(),
   # downloadLink('downloadCitations', 'All trees citations in txt format'),
   #
   # h2("Table of ages and sources"),
   # withSpinner(ui_element = tableOutput("age"), type = 4),
   # br(),
   #
   # h2("Plot of tree from median ages"),
   # p("Note that if partial matching is set up, branch lengths might not result in an ultrametric tree (since different trees may affect each edge"),
   # withSpinner(ui_element = plotOutput("medianPlot", height = "auto"), type = 4),
   # br(),
   # br(),
   #
   # h2("Plot of tree from supertree method (sdm)"),
   # p(" "),
   # withSpinner(ui_element = plotOutput("sdmPlot", height = "auto"), type = 4),
   # br(),
   # br(),
   #
   # h2("Plot of all source trees"),  # subset
   # h3("Overlay plots"),  #
   # p("Only chronograms with > 2 tips"),
   # withSpinner(ui_element = plotOutput("densiTreePlot", height = "auto"), type = 4),
   # br(),
   # br(),
   #
   # h3("Individual plots"),  # subset
   # withSpinner(ui_element = plotOutput("allPlot", height = "auto"), type = 4)

   tabsetPanel(type = "tabs",   # for (i in 1:length(output$))
      tabPanel(h4("Table of ages and sources"), br(),
        withSpinner(ui_element = tableOutput("age"), type = 4),
        br()
      ),
      tabPanel(h4("Summary trees"), br(),
         h2("Plot of tree from median ages"),
         p("Note that if partial matching is set up, branch lengths might not result in an ultrametric tree (since different trees may affect each edge)"),
         withSpinner(ui_element = plotOutput("medianPlot", height = "auto"), type = 4),
         br(),
         br(),
         h2("Plot of tree from supertree method (sdm)"),
         p(" "),
         withSpinner(ui_element = plotOutput("sdmPlot", height = "auto"), type = 4),
         br()
      ),
      tabPanel(h4("All source trees"), br(),
        h2("Overlay plots"),  # with densiTree plot function
        # p("Only chronograms with > 2 tips"),
        # withSpinner(ui_element = plotOutput("densiTreePlotSome", height = "auto"), type = 4),
        # br(),
        # br(),
        p("All chronograms"),
        withSpinner(ui_element = plotOutput("densiTreePlotAll", height = "auto"), type = 4),
        br(),
        br(),
        h2("Individual plots"),  # subset
        br(),
        # withSpinner(ui_element = plotOutput("allPlot", height = "auto"), type = 4),
        # studyPlotUI("name"),
        withSpinner(ui_element = uiOutput("allPlots"), type = 4),
        br()
      ),
      tabPanel(h4("Downloads"), br(),
        downloadLink('downloadCSV', 'Table in csv format, including Newick of trees'),
        br(),
        downloadLink('downloadMedian', 'Summary tree from median of ages, in Newick format'),
        br(),
        downloadLink('downloadSDM', 'Summary tree from supertree method, in Newick format'),
        br(),
        downloadLink('downloadCitations', 'All trees citations in txt format'),
        br()
      )
   )
))
