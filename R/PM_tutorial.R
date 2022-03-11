
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
  file.copy(system.file("Examples", package="Pmetrics"), wd, recursive = T)
  script <- readr::read_lines(paste0(wd,"/Examples/Rscript/examples.R")) %>%
    stringr::str_replace_all("##WD##",wd)
  readr::write_lines(script,paste0(wd,"/Examples/Rscript/examples.R"))
}
