#' The following functions are deprecated in Pmetrics.
#'
#'
#' @title Deprecated functions.
#' @aliases PMFortranConfig, PMcheckMatrix, PMfixMatrix, NPload, ITload, NPreport, ITreport,
#' PMdiag, PMreadDefaults, PMwriteDefaults, makeNPDE, PMupdate
#' @author Michael Neely
#' @export

#' @export
PMFortranConfig <- function(...) {
  cat("\nPMFortranConfig is deprecated.  Use setPMoptions instead to change the compilation arguments.\n")
}

#' @export
PMcheckMatrix <- function(...) {
  cat("\nPMcheckMatrix is deprecated.  Use PMcheck() instead to check data and model files.\n")
}

#' @export
PMfixMatrix <- function(...) {
  cat("\nPMfixMatrix is deprecated.  Use PMcheck(...,fix=T) instead to fix data files.\n")
}

#' @export
NPload <- function(...) {
  cat("\nNPload is deprecated.  Use PMload() instead to load results of NPAG or IT2B runs.\n")
}

#' @export
ITload <- function(...) {
  cat("\nITload is deprecated.  Use PMload() instead to load results of NPAG or IT2B runs.\n")
}

#' @export
NPreport <- function(...) {
  cat("\nNPreport is deprecated.  Use PMreport() instead to process completed NPAG or IT2B runs.\n")
}

#' @export
ITreport <- function(...) {
  cat("\nITreport is deprecated.  Use PMreport() instead to process completed NPAG or IT2B runs.\n")
}

#' @export
PMdiag <- function(...) {
  cat("\nPMdiag is deprecated.  Use makeNPDE() instead to internally validate a model.\n")
}

#' @export
PMreadDefaults <- function(...) {
  cat("\nPMreadDefaults is deprecated.  Use getPMoptions() instead to read Pmetrics options.\n")
}

#' @export
PMwriteDefaults <- function(...) {
  cat("\nPMwriteDefaults is deprecated.  Use setPMoptions() instead to set Pmetrics options.\n")
}

#' @export
makeNPDE <- function(...) {
  cat("\nmakeNPDE is deprecated.  Use $validate() on a PM_result object or makeValid() instead.\n")
}

#' @export
PMupdate <- function(...) {
  cat("\nPMupdate is deprecated. Please restart Rstudio and do not load Pmetrics.  Then use the command devtools::install_github(\"LAPKB/Pmetrics\")")
}
