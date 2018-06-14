library(shiny)

shinyUI(fluidPage(
tags$head(includeScript("google-analytics.js")),
h1("Query"),
textInput("taxa", "Taxa (comma delimited, spaces are ok) or Tree (Newick format, make sure to end with a semicolon)", "Rhea americana, Pterocnemia pennata, Struthio camelus", width="100%"),
 checkboxInput('partial', 'Include studies with a subset of desired taxa, perhaps resulting in underestimate of ages', TRUE),
 br(),
 checkboxInput('highertaxon', 'Search all species within higher taxa', FALSE),
 br(),
 checkboxInput('usetnrs', 'Use name resolution to deal with changes in taxonomy (slows run considerably)', FALSE),
 br(),
 checkboxInput('approximatematch', 'Use approximate matching for name resolution (can handle mismatches, but makes runs MUCH slower)', FALSE),
 br(),
 h1("Results"),
 h2("Downloads"),
 downloadLink('downloadCSV', 'Table in csv format, including Newick of trees'),
 h2("Table of ages and sources"),
 tableOutput("age"),
 h2("Plot of tree from median ages"),
 p("Note that if partial matching is set up, branch lengths might not result in an ultrametric tree (since different trees may affect each edge"),
 plotOutput("medianPlot"),
 h2("Plot of all trees"),
 plotOutput("allPlot")
))
