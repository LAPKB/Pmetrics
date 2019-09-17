#' Make a Percent Target Attainment (PTA) Target
#'
#' \code{makePTAtarget} generates an object of class \emph{PMpta.targ} which can
#' be used in the \code{\link{makePTA command}} for targets sampled from a distribution.
#'
#' @title Make PTA target object
#' @param x A data.frame or name of .csv file in working directory whose first two
#' columns are targets and the number of samples for each target.  An example can be
#' seen for Staphylococcus aureus susceptibility to vancomycin at the EUCAST website
#' at \link{http://mic.eucast.org/Eucast2/regShow.jsp?Id=1214}.
#' @return A data frame with two columns named targets and n, of class \emph{PMpta.targ}.
#' @seealso \code{\link{makePTA}}

makePTAtarget <- function(x){
  ptaTarg <- x
  if(is.character(x)){
    ptaTarg <- read.table(x,sep=",")
  } else {
    if(!is.data.frame(ptaTarg)) stop("PTA target must be a data frame.\n")
  }
  names(ptaTarg)[1:2] <- c("target","n")
  class(ptaTarg) <- c("PMpta.targ","data.frame")
  return(ptaTarg)
}
