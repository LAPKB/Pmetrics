#' Download and install Pmetrics updates from LAPK website
#'
#' @title Download and install Pmetrics updates
#' @param force Boolean operator to force downloading and installing.  Default is false.
#' @return The latest system-specific Pmetrics update will be downloaded to a temporary
#' folder and then installed.  You need to restart R (Rstudio) and then reload Pmetrics with
#' the \code{library(Pmetrics)} command to complete the installation.
#' @author Michael Neely

PMupdate <- function(force=F){
  
  currentVersion <- package_version(suppressWarnings(
    tryCatch(scan("http://www.lapk.org/software/Pmetrics/PmetricsVersion.txt",what="character",quiet=T), 
             error = function(e) e <-"0.1")))  
  if(currentVersion=="0.1"){cat("LAPKB server not available. Check your internet connection.\n");return(invisible(FALSE))}
  installedVersion <- packageVersion("Pmetrics")
  if(!force & installedVersion >= currentVersion){
    cat("You have the most current version of Pmetrics.\n")
    return(invisible(FALSE))
  } else {
    install.packages(pkgs="Pmetrics",repos="http://www.lapk.org/software/Pmetrics/Repos")
    cat("\nIf installation was successful, restart R/Rstudio to complete the process.\n")
    
    OS=getOS()
    if(OS==2){
      cat("\nNOTE: Windows users may have trouble updating.\nIf you were not able to install, you can do the following.\n")
      if(Sys.getenv("RSTUDIO")==1){ #Rstudio
        cat("\n1.Paste the following into the R console: install.packages(pkgs=\"Pmetrics\",repos=\"http://www.lapk.org/software/Pmetrics/Repos\")\n")
        cat("2. Allow Rstudio to restart session.\n")
        cat("3. Rstudio will complete installation.\n")
        cat("4. Restart Rstudio when installation is complete.\n")
      } else { #not Rstudio
        cat("\n1. Restart R.\n")
        cat("2. Do NOT load Pmetrics with the library command.\n")
        cat("3. Paste the following into the R console: install.packages(pkgs=\"Pmetrics\",repos=\"http://www.lapk.org/software/Pmetrics/Repos\")\n")
        cat("4. Restart R when installation is complete.\n")
      }
    }
  
  }
  
  
  return(invisible(TRUE))
  
} 

