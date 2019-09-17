#' Set user options for Pmetrics
#'
#' This function will set user options for Pmetrics.  
#'
#' @title Set Pmetrics User Options
#' @param sep The field separator character; \dQuote{,} by default, but could be \dQuote{;}
#' @param dec The decimal separator character; \dQuote{.} by default, but could be \dQuote{,}
#' @return The user preferences file will be updated.  This will persist from session to session.
#' @author Michael Neely
#' @export



setPMoptions <- function(sep,dec){
  if(length(grep("rjson",installed.packages()[,1]))==0){
    install.packages("rjson",repos="http://cran.cnr.Berkeley.edu",dependencies=T)
  }
  
  rjson.installed <- require(rjson,quietly=TRUE)
  if(!rjson.installed){
    cat("rjson package not installed.  Cannot set options.  Connect to internet and re-run makePost to download and install rjson package.\n")
    flush.console()
    return()
  } 
  #read old values first
  PMopts <- getPMoptions()
  #update/add options
  if(!missing(sep)) PMopts$sep <- sep
  if(!missing(dec)) PMopts$dec <- dec
  #set the options
  options(PMopts)
  #store the options
  PMoptionsFile <- paste(path.package("Pmetrics"),"/PMoptions.json",sep="")
  writeLines(toJSON(PMopts),PMoptionsFile)  
}