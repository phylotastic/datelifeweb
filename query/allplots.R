# internal functions for server

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
tree_plot_height <- function(tree){
  tipnum <- ape::Ntip(tree)
  if(tipnum > 10){
    hei <- 50 + (30 * tipnum)
  } else {
    hei <- 300
  }
  hei
}

reactive({
    rv$summ.table <- summarize_datelife_result(datelife_result = rv$get_filtered_results,
    summary_format = "data_frame")
    rv$get.median.tree <- summarize_datelife_result(datelife_result = rv$get_filtered_results,
        summary_format = "phylo_median")
    rv$get.median.tree$root.time <- max(ape::branching.times(rv$get.median.tree))

    rv$get.sdm.tree <- summarize_datelife_result(datelife_result = rv$get_filtered_results,
        summary_format = "phylo_sdm")
    rv$get.sdm.tree$root.time <- max(ape::branching.times(rv$get.sdm.tree))

    noisy.trees <- summarize_datelife_result(datelife_result = rv$get_filtered_results,
        summary_format = "phylo_all")
    tree.vector <- c()
    tree.vector.names <- c()
    for (i in sequence(length(noisy.trees))) {
      if(class(noisy.trees[[i]]) == "phylo") {
        if(length(tree.vector) > 0) {
          tree.vector <- ape:::c.phylo(tree.vector, noisy.trees[[i]])
          tree.vector.names <- c(tree.vector.names, names(noisy.trees)[i])
        } else {
          tree.vector <- ape:::c.phylo(noisy.trees[[i]])
          tree.vector.names <- names(noisy.trees)[i]
        }
      }
    }
    names(tree.vector) <- tree.vector.names
    rv$get.all.trees <- tree.vector

    rv$max.tree.age <- round(max(unlist(sapply(
      c(list(rv$get.median.tree), list(rv$get.sdm.tree), rv$get.all.trees),
      ape::branching.times))) + 5, digits = -1)

    tip.label.length <- unique(unlist(sapply(
      c(list(rv$get.median.tree), list(rv$get.sdm.tree), rv$get.all.trees),
      "[", "tip.label")))
    ind <- which.max(nchar(tip.label.length))
    rv$max.tip.label <- nchar(tip.label.length[ind])  # use strWidth?
})
