#' Opens the Pmetrics User Manual and function libraries
#'
#' Help for Pmetrics.
#'
#' @title Open user and function manuals.
#' @export
#
PMmanual <- function() {
  OS <- getOS()
  if (OS == 1 | OS == 3) {
    system(paste("open ", normalizePath(getPMpath(), winslash = "/"), "/doc/Pmetrics-manual.pdf", sep = ""))
    system(paste("open ", normalizePath(getPMpath(), winslash = "/"), "/doc/PM_User_manual.pdf", sep = ""))
  }
  if (OS == 2) {
    shell(paste("start acrord32.exe ", shQuote(normalizePath(paste(getPMpath(), "/doc/PM_User_manual.pdf", sep = ""), winslash = "\\")), sep = ""))
    shell(paste("start acrord32.exe ", shQuote(normalizePath(paste(getPMpath(), "/doc/Pmetrics-manual.pdf", sep = ""), winslash = "\\")), sep = ""))
  }
}


