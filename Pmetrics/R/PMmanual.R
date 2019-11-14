#' Opens the Pmetrics User Manual and function libraries
#'
#' Help for Pmetrics.
#'
#' @title Open user and function manuals.
#
PMmanual <- function(){
  OS <- getOS()
  if(OS==1 | OS==3){
    system(paste("open ",normalizePath(getPMpath(),winslash="/"),"/Pmetrics/doc/Pmetrics-manual.pdf",sep=""))
    system(paste("open ",normalizePath(getPMpath(),winslash="/"),"/Pmetrics/doc/PM_User_manual.pdf",sep=""))
  }
  if(OS==2){
    shell(paste("start acrord32.exe ",shQuote(normalizePath(paste(getPMpath(),"/Pmetrics/doc/PM_User_manual.pdf",sep=""),winslash="\\")),sep=""))
    shell(paste("start acrord32.exe ",shQuote(normalizePath(paste(getPMpath(),"/Pmetrics/doc/Pmetrics-manual.pdf",sep=""),winslash="\\")),sep=""))
  }
}


