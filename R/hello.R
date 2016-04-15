hello <- function (myname = "")
{
    if (myname == "") {
        stop("Tell me your name!")
    }
    list(message = paste("hello", myname, "! This is", R.Version()$version.string))
}

GetAllCitations <- function() {
  data(datelife::opentree_chronograms)
  return(list(message=names(datelife.cache$trees)))
}

run <- function(...) {
  data(datelife::opentree_chronograms)
  taxa <- c("Rhea americana", "Pterocnemia pennata", "Struthio camelus")
  results.list <- lapply(datelife.cache$trees,GetSubsetArrayDispatch, taxa=taxa, phy=NULL)
  filtered.results <- datelife::ProcessResultsList(results.list, taxa, TRUE)
  return(list(message=paste(datelife::SummarizeResults(filtered.results, output.format="mrca", datelife.cache=datelife.cache))))
}
