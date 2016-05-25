#' Get citations for all the trees currently in the cache
#' @return A message with the list of publications
#' @export
GetAllCitations <- function() {
  data(datelife::opentree_chronograms)
  publications <- paste('<ul>', paste('<li>',names(datelife.cache$trees), '</li>', sep="", collapse=""), '</ul>', sep="")
  return(list(message=publications))
}

#' The core function to interact with DateLife via the web or API
#' @param input The string to pass to R
#' @param format The return format desired
#' @param partialfield To be liberal or conservative with returning trees that only match some of the taxa
#' @param usetnrsfield Yes or no to use taxonomic name resolution
#' @param approximatematchfield Yes or no to use approximate name matching
#' @return A message in the appropriate format
#' @export
run <- function(input, format="mrca", partialfield="liberal", usetnrsfield="no", approximatematchfield="no") {
  #taxa <- c("Rhea americana", "Pterocnemia pennata", "Struthio camelus")
  #results.list <- lapply(datelife.cache$trees,GetSubsetArrayDispatch, taxa=taxa, phy=NULL)
#  filtered.results <- datelife::ProcessResultsList(results.list, taxa, TRUE)
#  return(list(message=paste(datelife::SummarizeResults(filtered.results, output.format=format, cache=datelife.cache))))
  partial.input <- TRUE
  if(partialfield == "conservative") {
    partial.input <- FALSE
  }
  usetnrs.input <- FALSE
  if(usetnrsfield == "yes") {
    usetnrs.input <- TRUE
  }
  approximatematch.input <- FALSE
  if(approximatematchfield == "yes") {
    approximatematch.input <- TRUE
  }
  return(list(message=EstimateDates(input, output.format=format, usetnrs=usetnrs.input, partial=partial.input, approximatematch=approximatematch.input)))
}
