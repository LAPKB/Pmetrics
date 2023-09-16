
#' @title Configure Fortran
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' Use setPMoptions() instead.
#' @param ... Not used
#' @author Michael Neely
#' @keywords internal
#' @export
PMFortranConfig <- function(...) {
  lifecycle::deprecate_warn("2.1.0", "PMFortranConfig()", details = "See ?setPMoptions.")
}

#' @title Check data matrix
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' Use PM_data$new() and PM_model$new() instead.
#' @param ... Not used
#' @author Michael Neely
#' @keywords internal
#' @export
PMcheckMatrix <- function(...) {
  lifecycle::deprecate_warn("1.3.0", "PMcheckMatrix()", details = "See ?PM_data and ?PMcheck.")
}

#' @title Fix data matrix
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' Use PM_data$new() and PM_model$new() instead.
#' @param ... Not used
#' @author Michael Neely
#' @keywords internal
#' @export
PMfixMatrix <- function(...) {
  lifecycle::deprecate_warn("1.3.0", "PMfixMatrix()", details = "See ?PM_data and ?PMcheck.")
}

#' @title Load NPAG run
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' Use PM_load() instead.
#' @param ... Not used
#' @author Michael Neely
#' @keywords internal
#' @export
NPload <- function(...) {
  lifecycle::deprecate_warn("1.3.0", "NPload()", details = "See ?PM_load.")
}

#' @title Load IT2B run
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' Use PM_load() instead.
#' @param ... Not used
#' @author Michael Neely
#' @keywords internal
#' @export
ITload <- function(...) {
  lifecycle::deprecate_warn("1.3.0", "ITload()", details = "See ?PM_load.")
}

#' @title NPAG run report
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' Use PMreport() instead.
#' @param ... Not used
#' @author Michael Neely
#' @keywords internal
#' @export
NPreport <- function(...) {
  lifecycle::deprecate_warn("1.5.0", "NPreport()", details = "See ?PMreport.")
}

#' @title IT2B run report
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' Use PMreport() instead.
#' @param ... Not used
#' @author Michael Neely
#' @keywords internal
#' @export
ITreport <- function(...) {
  lifecycle::deprecate_warn("1.5.0", "ITreport()", details = "See ?PMreport.")
}

#' @title Diagnostics
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' Use $validate() on a PM_result object or make_valid() instead.
#' @param ... Not used
#' @author Michael Neely
#' @keywords internal
#' @export
PMdiag <- function(...) {
  lifecycle::deprecate_warn("1.5.0", "PMdiag()", details = "See ?PM_result or ?make_valid.")
}

#' @title Read Defaults
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' Use getPMoptions() instead.
#' @param ... Not used
#' @author Michael Neely
#' @keywords internal
#' @export
PMreadDefaults <- function(...) {
  lifecycle::deprecate_warn("1.5.0", "PMreadDefaults()", details = "See ?getPMoptions.")
}

#' @title Write Defaults
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' Use setPMoptions() instead.
#' @param ... Not used
#' @author Michael Neely
#' @keywords internal
#' @export
PMwriteDefaults <- function(...) {
  lifecycle::deprecate_warn("1.5.0", "PMwriteDefaults()", details = "See ?setPMoptions.")
}

#' @title makeNPDE
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' Use $validate() on a PM_result object or make_valid() instead.
#' @param ... Not used
#' @author Michael Neely
#' @keywords internal
#' @export
makeNPDE <- function(...) {
  lifecycle::deprecate_warn("2.0.0", "makeNPDE()", details = "See ?PM_result or ?make_valid.")
}

#' @title Save Pmetrics objects
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' No longer applies as Pmetrics R6 objects have their own save methods.
#' @param ... Not used
#' @author Michael Neely
#' @keywords internal
#' @seealso [PM_result], [PM_sim], [PM_pta]
#' @export
PMsave <- function(...){
  lifecycle::deprecate_warn("2.1.0", "PMsave()", details = "See ?PM_result, ?PM_sim, or ?PM_pta for current save methods.")
}

