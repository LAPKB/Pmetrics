#' Opens the Pmetrics User reference online
#'
#' Help for Pmetrics.
#'
#' @title Open user and function manuals.
#' @export
#
PMmanual <- function() {
  
  ip <- tryCatch(curl::nslookup("www.r-project.org"), error = function(e) NULL)
  if(is.null(ip)){
    cat(paste0(crayon::red("Note: "),"you are not connected to the internet.\n"))
    cat("Browsing package vignettes...")
    browseVignettes("Pmetrics")
  } else {
    browseURL("https://lapkb.github.io/Pmetrics/")
  }
  #openHTML(paste(path.package("Pmetrics"), "/manual/index.html", sep = ""))
}


