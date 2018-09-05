shinyUI(fluidPage(
  # work around textAreaInput not spanning 100% of webpage when width = 100%
  tags$style(HTML("
    .shiny-input-container:not(.shiny-input-container-inline) {
    width: 100%;
  }")),
  tags$head(includeScript("google-analytics.js")),
  # https://stackoverflow.com/questions/36995142/get-the-size-of-the-window-in-shiny
  tags$head(tags$script('var dimension = [0, 0];
    $(document).on("shiny:connected", function(e) {
        dimension[0] = window.innerWidth;
        dimension[1] = window.innerHeight;
        Shiny.onInputChange("dimension", dimension);
    });
    $(window).resize(function(e) {
        dimension[0] = window.innerWidth;
        dimension[1] = window.innerHeight;
        Shiny.onInputChange("dimension", dimension);
    });')),
  verbatimTextOutput("dimension_display"),
  h1('DateLife Query'),
  # textInput does not allow controlling height of box, use textAreaInput instead
  textAreaInput('taxa', "Taxa (comma delimited; spaces or underscores in binomials are ok) or a Tree (Newick format, make sure to end with a semicolon)",
  "Rhea americana, Pterocnemia pennata, Struthio camelus", width = text_width, height = "auto"),
  # submitButton(text = " Refresh DateLife"),  # is not recommended by RStudio devs anymore. It does what I want, but has a weird behaviour.
   checkboxInput('partial',
   'Show all studies. Includes studies with a subset of desired taxa, perhaps resulting in underestimate of maximum age',
   TRUE, width = text_width),
   checkboxInput('highertaxon', 'Search species within higher taxa. Several higher taxa can be searched at a time, comma separated',
   TRUE, width = text_width),
   checkboxInput('usetnrs', 'Use name resolution to deal with changes in taxonomy or misspellings (slows run considerably)',
   FALSE, width = text_width),
   checkboxInput('approximatematch', 'Use approximate matching for name resolution (can handle mismatches, but makes runs MUCH slower)',
   FALSE, width = text_width),
   fluidRow(
     column(2,
       br(),
       actionButton(inputId = 'refresh', label = ' Refresh DateLife',
         class = "btn btn-primary", #icon = icon("paper-plane"), #"search" is a magnifier, "paper-plane" is also cool, maybe for submit or send.
         # style="color: #fff; background-color: #337ab7; border-color: #2e6da4"), #color: is for letters
         style = "color: #fff; background-color: #3399ff")
      ),
      column(2,
        textInput(inputId = 'plotwidth', label = '  Chronogram Width', value = 100,
        width = "50%", placeholder = "a number"), offset = 0
      )
   ),
   br(),

   h1("DateLife Results"), br(),
   tabsetPanel(type = "tabs",
      tabPanel(h4("Table of ages and sources"), br(),
        withSpinner(ui_element = tableOutput("age"), type = 4), br()
      ),
      tabPanel(h4("Summary trees"), br(),
         h2("Plot of tree from median ages"),
         p("Note that if partial matching is set up, branch lengths might not result in an ultrametric tree (since different trees may affect each edge)"),
         shinycssloaders::withSpinner(ui_element = plotOutput("medianPlot", width = "100%", height = "auto"), type = 4),
         br(), br(),
         h2("Plot of tree from supertree method (sdm)"), p(" "),
         shinycssloaders::withSpinner(ui_element = plotOutput("sdmPlot", width = "25%", height = "auto"), type = 4),
         br()
      ),
      tabPanel(h4("All source trees"), br(),
        h2("Overlay plots"),  # with densiTree plot function
        p("All chronograms"),
        shinycssloaders::withSpinner(ui_element = plotOutput("densiTreePlotAll", width = "auto", height = "auto"), type = 4),
        br(), br(),
        h2("Individual plots"), br(),
        withSpinner(ui_element = uiOutput("allPlots"), type = 4), br()
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
   ),
   br(), #p("Powered by DateLife version 1")  #could try with tags$footer
   tags$footer("DateLife 2018", align = "center", style = "
            # position:absolute;
            bottom:0;
            width:100%;
            height:35px;   /* Height of the footer */
            color: gray;
            padding: 7px;
            background-color: #4975B198;
            z-index: 1000;")

))
