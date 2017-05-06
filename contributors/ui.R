library(shiny)

shinyUI(fluidPage(
 h2("Contributors"),
 p("Work on reusing trees requires people to do the work of doing the research and releasing their results (authors) and curating them for later reuse (curators). The tables below are the authors and curators of studies used in OpenTree's synthetic tree (and so doesn't reflect deposited trees that are not marked for synthesis or which have been uploaded since the last synthetic tree was computed) and the authors of trees in TreeBase."),
 h3("OpenTree Authors"),
 dataTableOutput("authors"),
 h3("OpenTree Curators"),
 dataTableOutput("curators"),
 h3("TreeBase Authors"),
 dataTableOutput("tbauthors")
))
