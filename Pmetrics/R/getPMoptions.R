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



getPMoptions <- function(opt) {

  #options file name
  PMoptionsFile <- paste(system.file("options", package = "Pmetrics"),"PMoptions.json", sep = "/")
  #if it doesn't exist, create it with defaults
  if (!file.exists(PMoptionsFile)) {
    PMopts <- list(sep = ",",
                   dec = ".",
                   server_address = "http://localhost:5000")
    options(PMopts)
    jsonlite::write_json(PMopts, path = PMoptionsFile, auto_unbox=T)
  }
  #read the options file
  PMopts <- jsonlite::read_json(path = PMoptionsFile)
  if (missing(opt)) {
    return(PMopts)
  } else {
    index <- which(names(PMopts) == opt)
    if (length(index) == 0) { return(NULL) } else { return(PMopts[[index]]) }
  }
}