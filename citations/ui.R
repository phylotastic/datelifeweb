library(shiny)

shinyUI(fluidPage(
  h2("Cite DateLife"),
  p('DateLife publications, R package and web page citations'),
  downloadLink('downloadDateLifeCitations', 'Download table in csv format'),
  br(),
  # downloadLink('downloadDateLifeReferencesBib', 'Download references in bib format'),
  # br(),
  tableOutput("datelife.citations"),
  br(),
  br(),
  h2("Primary Studies Citations"),
  downloadLink('downloadcitationCSV', 'Download table in csv format'),
  br(),
  # downloadLink('downloadReferencesBib', 'Download references in bib format'),
  # br(),
  tableOutput("citations")
))
