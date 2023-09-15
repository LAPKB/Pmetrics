#' `r lifecycle::badge('deprecated')`
#' 
#' These functions are deprecated in Pmetrics.
#'
#' @title Deprecated functions.
#' @param ... Not used
#' @author Michael Neely
#' @export

PMFortranConfig <- function(...) {
  cat("\nPMFortranConfig is deprecated.  Use setPMoptions() instead to change the compilation arguments.\n")
}

#' @rdname PMFortranConfig
#' @export
PMcheckMatrix <- function(...) {
  cat("\nPMcheckMatrix is deprecated.  Use PM_data$new() and PM_model$new() instead to check data and model files.\n")
}

#' @rdname PMFortranConfig
#' @export
PMfixMatrix <- function(...) {
  cat("\nPMfixMatrix is deprecated.  Use PM_data$new() instead to check data files.\n")
}

#' @rdname PMFortranConfig
#' @export
NPload <- function(...) {
  cat("\nNPload is deprecated.  Use PM_load() instead to load results of NPAG or IT2B runs.\n")
}

#' @rdname PMFortranConfig
#' @export
ITload <- function(...) {
  cat("\nITload is deprecated.  Use PM_load() instead to load results of NPAG or IT2B runs.\n")
}

#' @rdname PMFortranConfig
#' @export
NPreport <- function(...) {
  cat("\nNPreport is deprecated.  Use PMreport() instead to process completed NPAG or IT2B runs.\n")
}

#' @rdname PMFortranConfig
#' @export
ITreport <- function(...) {
  cat("\nITreport is deprecated.  Use PMreport() instead to process completed NPAG or IT2B runs.\n")
}

#' @rdname PMFortranConfig
#' @export
PMdiag <- function(...) {
  cat("\nPMdiag is deprecated.  Use $validate() on a PM_result object or makeValid() instead.\n")
}

#' @rdname PMFortranConfig
#' @export
PMreadDefaults <- function(...) {
  cat("\nPMreadDefaults is deprecated.  Use getPMoptions() instead to read Pmetrics options.\n")
}

#' @rdname PMFortranConfig
#' @export
PMwriteDefaults <- function(...) {
  cat("\nPMwriteDefaults is deprecated.  Use setPMoptions() instead to set Pmetrics options.\n")
}

#' @rdname PMFortranConfig
#' @export
makeNPDE <- function(...) {
  cat("\nmakeNPDE is deprecated.  Use $validate() on a PM_result object or makeValid() instead.\n")
}

