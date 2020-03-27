#' The following functions are deprecated in Pmetrics.
#' 
#' 
#' @title Deprecated functions.
#' @aliases PMcheckMatrix, PMfixMatrix, NPload, ITload, NPreport, ITreport, PMdiag
#' @author Michael Neely
#' @export



PMcheckMatrix <- function(){
  cat("\nPMcheckMatrix is deprecated.  Use PMcheck() instead to check data and model files.\n")
}

PMfixMatrix <- function(){
  cat("\nPMfixMatrix is deprecated.  Use PMcheck(...,fix=T) instead to fix data files.\n")
}

NPload <- function(){
  cat("\nNPload is deprecated.  Use PMload() instead to load results of NPAG or IT2B runs.\n")
}

ITload <- function(){
  cat("\nITload is deprecated.  Use PMload() instead to load results of NPAG or IT2B runs.\n")
}

NPreport <- function(){
  cat("\nNPreport is deprecated.  Use PMreport() instead to process completed NPAG or IT2B runs.\n")
}

ITreport <- function(){
  cat("\nITreport is deprecated.  Use PMreport() instead to process completed NPAG or IT2B runs.\n")
}

PMdiag <- function(){
  cat("\nPMdiag is deprecated.  Use makeNPDE() instead to internally validate a model.\n")
}

PMreadDefaults <- function(){
  cat("\nPMreadDefaults is deprecated.  Use getPMoptions() instead to read Pmetrics options.\n")
}

PMwriteDefaults <- function(){
  cat("\nPMwriteDefaults is deprecated.  Use setPMoptions() instead to set Pmetrics options.\n")
}

makeNPDE <- function(){
  cat("\nmakeNPDE is deprecated.  Use makeValid() instead.\n")
}

PMupdate <- function(){
  cat("\nPMupdate is deprecated. Please restart Rstudio and do not load Pmetrics.  Then use the command devtools::install_github(\"LAPKB/Pmetrics\")")
}
