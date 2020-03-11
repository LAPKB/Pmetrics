#' Summarize a Pmetrics D-optimal object
#'
#' Summarize observations, predictions and errors in a PMdopt object made by \code{\link{Dopt}}.
#'
#' @title Summarize PMdopt objects
#' @method summary PMdopt
#' @param x A PMdopt object made by \code{\link{Dopt}}.
#' @return The weighted mean D-optimal times.
#' @author Michael Neely
#' @seealso \code{\link{makeOP}}
#' @export

summary.PMdopt <- function(x){
  cat("Weighted mean D-optimal sample times are:\n")
  cat("________________________________________\n")
  cat(paste(x$mean,collapse="\n"))
}