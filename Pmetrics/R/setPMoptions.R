#' Set user options for Pmetrics
#'
#' This function will set user options for Pmetrics.  
#'
#' @title Set Pmetrics User Options
#' @param sep The field separator character; \dQuote{,} by default, but could be \dQuote{;}
#' @param dec The decimal separator character; \dQuote{.} by default, but could be \dQuote{,}
#' @param server_address Specify address of server for remote runs.  Server must be set up separately.
#' This functionality is coming soon.
#' @return The user preferences file will be updated.  This will persist from session to session.
#' @author Michael Neely
#' @export



setPMoptions <- function(sep, dec, server_address) {
  checkRequiredPackages("rjson")
  #read old values first
  PMopts <- getPMoptions()
  #update/add options
  if (!missing(sep)) PMopts$sep <- sep
  if (!missing(dec)) PMopts$dec <- dec
  if (!missing(server_address)) PMopts$server_address <- server_address
  #set the options
  options(PMopts)
  #store the options
  PMoptionsFile <- paste(system.file("options", package = "Pmetrics"),"PMoptions.json", sep = "/")
  writeLines(toJSON(PMopts), PMoptionsFile)
}