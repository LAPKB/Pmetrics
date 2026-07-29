
getFixedColNames <- function() {
  # set current names of fixed columns in data file

  c(
    "id", "evid", "time", "dur", "dose", "addl",
    "ii", "input", "out", "outeq", "cens", "c0", "c1", "c2", "c3"
  )
}


# getFixedColNum ------------------------------------------------------------------

#' @title Number of fixed columns
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Returns the number of fixed columns (non-covariate) in Pmetrics data objects.
#' @return An integer with the number of fixed columns.
#'
#' @export
#' @examples
#' \dontrun{
#' getFixedColNum()
#' }

#' @author Michael Neely
getFixedColNum <- function() {
  # set current number of fixed columns in data file
  length(getFixedColNames())
}
