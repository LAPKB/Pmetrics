#' See changelog for Pmetrics
#'
#' @title Pmetrics changelog
#' @param PMversion Default is the current version, otherwise a character string with the starting version you wish to see up to 
#' the current, e.g. \dQuote{0.21}.  Use \dQuote{all} for all versions.
#' @return The changelog for the requested version.
#' @author Michael Neely
#' @examples
#' PMnews()
#' @export

PMnews <- function(PMversion=packageVersion("Pmetrics")){
  if(as.character(PMversion)=="all"){
    chlog <- news(package="Pmetrics")
  } else {chlog <- news(query = Version>=PMversion,package="Pmetrics")}
  chlog
}

