# Returns environment variables used throughout Pmetrics

getPMpath <- function() {
  # checkRequiredPackages("purrr")
  # return Pmetrics installation folder
  paste(.libPaths(), "Pmetrics", sep = "/") %>%
    keep(file.exists) %>%
    pluck(1) # ensure only one path returned (should be unnecessary)
}

getBits <- function() {
  # figure out 32 or 64 bit
  if (length(grep("32-bit", utils::sessionInfo())) > 0) {
    return(32)
  } else {
    return(64)
  }
}

# getFixedColNames ------------------------------------------------------------------

#' @title Names of fixed columns  
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Returns the names of fixed columns (non-covariate) in Pmetrics data objects.
#' @return A vector of fixed column names: 
#' `c("id", "evid", "time", "dur", "dose", "addl", "ii", "input", "out", "outeq", "c0", "c1", "c2", "c3")`
#' 
#' @export
#' @examples
#' getFixedColNames()
#' @author Michael Neely
getFixedColNames <- function() {
  # set current names of fixed columns in data file

  c(
    "id", "evid", "time", "dur", "dose", "addl",
    "ii", "input", "out", "outeq", "c0", "c1", "c2", "c3"
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
#' getFixedColNum()
#' @author Michael Neely
getFixedColNum <- function() {
  # set current number of fixed columns in data file
  length(getFixedColNames())
}

.getApiKey <- function() {
  return("qoc+7YRUCCK7BmOJrkzNRY6gKzXIGzPPR6IoefaZpOtPkEsKwO48vkCPM18G97Y9")
}
