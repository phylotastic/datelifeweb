hello <- function (myname = "")
{
    if (myname == "") {
        stop("Tell me your name!")
    }
    list(message = paste("hello", myname, "! This is", R.Version()$version.string))
}

GetAllCitations <- function() {
  data(datelife::opentree_chronograms)
  publications <- paste('<ul>', paste('<li>',names(datelife.cache$trees), '</li>', sep="", collapse=""), '</ul>', sep="")
  return(list(message=publications))
}

run <- function(input, format, partialfield="liberal", usetnrsfield="no", approximatematchfield="no") {
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
