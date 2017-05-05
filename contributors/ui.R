library(shiny)

shinyUI(fluidPage(
 h2("Contributors"),
 p("Work on reusing trees requires people to do the work of doing the research and releasing their results (authors) and curating them for later reuse (curators). The tables below are the authors and curators of studies used in OpenTree's synthetic tree."),
 h3("Authors"),
 tableOutput("authors"),
 h3("Curators"),
 tableOutput("curators")
))
