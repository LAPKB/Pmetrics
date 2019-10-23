#' Download and install Pmetrics updates from LAPK website
#'
#' @title Download and install Pmetrics updates
#' @return The latest system-specific Pmetrics update  will be installed after you
#' supply a valid LAPK username (email address) and password
#' @author Michael Neely

PMupdate <- function(auto=T){
    
  currentVersion <- package_version(suppressWarnings(
    tryCatch(scan("http://www.lapk.org/PmetricsVersion.txt",what="character",quiet=T), 
             error = function(e) e <-"0.1")))  
  if(currentVersion=="0.1"){cat("LAPK server not available. Check your internet connection.\n");return(invisible(FALSE))}
  installedVersion <- packageVersion("Pmetrics")
  if(installedVersion >= currentVersion){
    cat("You have the most current version of Pmetrics.\n")
    return(invisible(FALSE))
  } else {
    update <- readline(paste("Version",currentVersion,"is available.  Update? y/n \n"))
    if(tolower(update)=="y"){
      if(!exists("user")) user <<- readline("Enter your LAPK registered email address: \n")
      if(!exists("pwd")) pwd <<- readline("Enter your LAPK password: \n")
      cat("Contacting LAPK server...\n")
      flush.console()
      updateURL <- scan(paste("http://www.lapk.org/PMupdate.php?email=",user,"&password=",pwd,sep=""),skip=5,what="character",nlines=1,quiet=T)
      if(updateURL=="NoConnect") stop("Unable to connect to LAPK server.  Please try again later.\n")
      if(updateURL=="NotFound"){
        while(updateURL=="NotFound" & tolower(update)=="y"){
          cat("Error: user email and/or password not recognized.  Visit http://www.lapk.org for recovery.\n")
          flush.console()
          update <- readline("Try again? y/n \n")
          if(tolower(update)=="y"){
            user <<- readline("Enter your LAPK registered email address: \n")
            pwd <<- readline("Enter your LAPK password: \n")
            cat("Contacting LAPK server...\n")
            flush.console()
            updateURL <- scan(paste("http://www.lapk.org/PMupdate.php?email=",user,"&password=",pwd,sep=""),skip=5,what="character",nlines=1,quiet=T)
          }
        }
      } 
      if(updateURL != "NotFound" & update=="y"){
        OS <- switch(.Platform$OS.type, unix = 1, windows = 2)
        tempdir <- paste(Sys.getenv("PmetricsPath"),"/Pmetrics/temp",sep="")
        dir.create(tempdir,showWarnings=F)
        download <- paste(updateURL,"?path=Pmetrics_",currentVersion,".",c("tgz","zip")[OS],sep="")
        cat("\nEmail address and password recognized. Downloading Pmetrics (~4 Mb)...Please wait.\n")
        flush.console()
        dl <- try(download.file(download,paste(tempdir,"/Pmetrics.",c("tgz","zip")[OS],sep=""),quiet=T),silent=T)
        if(!is.null(attr(dl,"condition"))) stop("The LAPK server is busy.  Please try later.\n")      
        if(OS==1) system(paste("R CMD INSTALL ",tempdir,"/Pmetrics.",c("tgz","zip")[OS],sep=""))
        if(OS==2) {
          detach("package:Pmetrics")
          shell(paste("R CMD INSTALL ",tempdir,"/Pmetrics.",c("tgz","zip")[OS],sep=""))
        }
        cat("\nRestart R to complete the update.\n")
        flush.console()
        unlink(tempdir,recursive=T)
        
        return(invisible(TRUE))
      }
      
    } else {return(invisible(FALSE))}
  }
}
