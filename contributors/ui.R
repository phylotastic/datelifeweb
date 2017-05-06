library(shiny)

shinyUI(fluidPage(
 h2("Contributors"),
 p("Work on reusing trees requires people to do the work of doing the research and releasing their results (authors) and curating them for later reuse (curators). The tables below are the authors and curators of studies used in OpenTree's synthetic tree (and so doesn't reflect deposited trees that are not marked for synthesis or which have been uploaded since the last synthetic tree was computed) and the authors of trees in TreeBase. You can search and sort in the tables."),
 h3("OpenTree Authors"),
 downloadLink('downloadauthorsCSV', 'OpenTree author table in csv format'),
 dataTableOutput("authors"),
 h3("OpenTree Curators"),
 downloadLink('downloadcuratorsCSV', 'OpenTree curator table in csv format'),
 dataTableOutput("curators"),
 h3("TreeBase Authors"),
 downloadLink('downloadtbauthorsCSV', 'TreeBase author table in csv format'),
 dataTableOutput("tbauthors")
))
