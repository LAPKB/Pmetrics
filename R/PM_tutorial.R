#' @title Pmetrics tutorial
#' @description
#' `r lifecycle::badge("stable")`
#' Introductory tutorial to Pmetrics
#' @details
#' This function will create an Examples folder in the current working
#' directory. The folder will contain all files and folder necessary to
#' conduct the tutorial. After the Examples folder is created, open the
#' *examples.R* script to begin the tutorial.
#' @param wd The working directory in which to create the Examples folder.
#' Default is current working directory.
#' @return NULL
#' @export
#' 
PM_tutorial <- function(wd = getwd()){
  cat("This copies to your current working directory an \"Examples\" folder,
  which contains all the files and folders you need to work through an
  introductory tutorial.\n")
  cat("\nYour current working directory:",getwd(),"\n")
  ans <- readline("Press \"q\" to quit, \"return\" to use your current working directory, or enter a new one. ")
  if(tolower(substr(ans,1,1))=="q"){
    cat("No files copied.\n")
    return(invisible())
  } else if(ans!=""){
    if(dir.exists(ans)){
      stop(paste0(ans, " already exists.\n"))
    } else {dir.create(ans)
      wd <- ans}
  }
  if(dir.exists(paste0(wd,"/Examples"))){
    ans2 <- readline("\"Examples\" already exists in this location. Overwrite (y/n)? ")
    if(tolower(substr(ans2,1,1))=="n"){
      cat("No files copied.\n")
      return(invisible())
    }
  }
  
  file.copy(system.file("Examples", package="Pmetrics"), wd, recursive = T)
  script <- readr::read_lines(paste0(wd,"/Examples/Rscript/examples.R")) %>%
    stringr::str_replace_all("##WD##",paste0(wd, "/Examples"))
  readr::write_lines(script,paste0(wd,"/Examples/Rscript/examples.R"))
  cat("Open the Examples/Rscript/examples.R file to proceed with the tutorial.\n")
  return(invisible())
}
