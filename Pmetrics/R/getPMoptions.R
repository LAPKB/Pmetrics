#' Get user options for Pmetrics
#'
#' This function will get user options for Pmetrics.  Current user options are
#' \itemize{
#' \item sep Field separator in data files
#' \item dec Decimal separator in numbers
#' }  
#'
#' @title Get Pmetrics User Options
#' @param opt The option to retrieve.  If omitted, all option values will be returned.
#' @return The user options file will be updated.  This will persist from session to session.
#' @author Michael Neely
#' @export



getPMoptions <- function(opt){
  require(rjson)
  #options file name
  PMoptionsFile <- paste(path.package("Pmetrics"),"/PMoptions.json",sep="")
  #if it doesn't exist, create it with defaults
  if(!file.exists(PMoptionsFile)){
    PMopts <- list(sep=",",
                   dec=".")
    options(PMopts)
    writeLines(toJSON(PMopts),PMoptionsFile) 
  }
  #read the options file
  PMopts <- fromJSON(file=PMoptionsFile)
  if(missing(opt)){
    return(PMopts)
  } else {
    index <- which(names(PMopts)==opt)
    if(length(index)==0){return(NULL)} else {return(PMopts[[index]])}
  }
}