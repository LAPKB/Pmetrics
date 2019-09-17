#' Learn Pmetrics R code with user friendly graphical interfaces in the default browser.
#'
#' PMcode provides a graphical user interface to learn many of the Pmetrics functions and their arguments
#' using the Shiny package. A graphical user interface will launch in the default browser.  This GUI enables a point
#' and click approach to generating Pmetrics code (which can be pasted into the R script) and plot previews.
#' The idea is for users to learn the R code in an intuitive and easier manner.  There are more options available for Pmetrics
#' functions that are served by the GUI, but it is sufficiently powerful to serve basic needs.  To stop the shiny browser GUI, click
#' the stop buttton in Rstudio (upper left corner of console window) or ESC or CTRL-C may work when the R window is active.
#'
#' @title Pmetrics GUI Tutor
#' @param func Quoted name of a function family used in Pmetrics.  Currently, these are limited to \dQuote{run}, 
#' for \code{\link{NPrun}}, \code{\link{ITrun}} and \dQuote{plot}.  For the first two, make sure that the model and data files are in your
#' working directory before calling the function.  
#' @return Nothing is returned, but the user interface is launched in the default browser.  Appropriate R code to execute
#' Pmetrics commands is generated depending on defaults and user-selected input.  For plotting, the resulting plot is previewed
#' directly in the browser.
#' 
#' @author Michael Neely

PMcode <- function(func){
  if(!suppressWarnings(require(shiny,quietly=T))){
    resp <- readline("\nYou do not have the required 'shiny' package. Download? ")
    if(grepl("^y",tolower(resp))) {
      install.packages("shiny")
      cat("\nNow execute PMcode again.\n")
      return(invisible(NULL))
    }
  }
  
  run <- grep("run",tolower(func))
  if(length(run)>0) {func <- "run"}
  
  ShinyAppDir <- paste(normalizePath(get("PmetricsPath",envir=PMenv),winslash="/"),"/Pmetrics/PMcode/",func,sep="")
  
  if(func=="run"){
    file.copy(from=list.files(ShinyAppDir,pattern="\\.R$",full.names=T),to=getwd(),overwrite=T)
    runApp()
  } else {
    runApp(ShinyAppDir)
  }
  
}
