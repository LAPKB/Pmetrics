#' Summarize a Pmetrics D-optimal object
#'
#' Summarize observations, predictions and errors in a PMdopt object made by \code{\link{Dopt}}.
#'
#' @title Summarize PMdopt objects
#' @method summary PMdopt
#' @param object A PMdopt object made by \code{\link{Dopt}}.
#' @param \dots Other parameters which are not necessary.
#' @return The weighted mean D-optimal times.
#' @author Michael Neely
#' @seealso \code{\link{makeOP}}
#' @export

summary.PMdopt <- function(object,...){
  cat("Weighted mean D-optimal sample times are:\n")
  cat("________________________________________\n")
  cat(paste(object$mean,collapse="\n"))
}