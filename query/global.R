library(shiny)
library(shinycssloaders)
# devtools::install("~/Desktop/datelife")
library(datelife)
library(strap)
library(phangorn)
library(ape)
library(phyloch)
library(stringr)
library(promises)
library(future)
plan(multiprocess)

options(shiny.sanitize.errors = FALSE)
text_width <- "100%"
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
# the following gets height in pixels
tree_plot_height <- function(tree){
  tipnum <- ape::Ntip(tree)
  if(tipnum > 10){
    hei <- 50 + (30 * tipnum)
  } else {
    hei <- 300
  }
  hei
}
