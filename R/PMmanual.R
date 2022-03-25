#' Opens the Pmetrics User Manual and function libraries
#'
#' Help for Pmetrics.
#'
#' @title Open user and function manuals.
#' @export
#
PMmanual <- function() {
  openHTML(paste(path.package("Pmetrics"), "/manual/index.html", sep = ""))
}


