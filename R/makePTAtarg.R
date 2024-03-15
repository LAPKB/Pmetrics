#' @title Make a Percent Target Attainment (PTA) Target
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Generates an object of class *PMpta.targ* which can
#' be used in the [makePTA] method for [PM_pta] or [PM_sim] for targets sampled from a distribution.
#'
#' @title Make PTA target object
#' @param x A data.frame or name of .csv file in working directory whose first two
#' columns are targets and the number of samples for each target.  An example can be
#' seen for Staphylococcus aureus susceptibility to vancomycin at
#' [EUCAST](http://mic.eucast.org/Eucast2/regShow.jsp?Id=1214).
#' @return A data frame with two columns named targets and n, of class *PMpta.targ*.
#' @seealso [makePTA]
#' @examples
#' makePTAtarget(mic1)
#' @export

makePTAtarget <- function(x) {
  ptaTarg <- x
  if (is.character(x)) {
    ptaTarg <- read.table(x, sep = ",")
  } else {
    if (!is.data.frame(ptaTarg)) stop("PTA target must be a data frame.\n")
  }
  names(ptaTarg)[1:2] <- c("target", "n")
  class(ptaTarg) <- c("PMpta.targ", "data.frame")
  return(ptaTarg)
}
