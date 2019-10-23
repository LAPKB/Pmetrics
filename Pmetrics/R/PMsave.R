#' Saves Pmetrics objects 
#' 
#' Any objects that are made during the course of analysis in R can be added to the saved data
#' that are automatically generated at the end of an NPAG or IT2B run and loaded with \code{\link{PMload}}.
#' Objects with the same run number will be saved as a group.  So if a user has made a new object called
#' lm.1 that contains regressions related to run 1, it will be saved with any other object
#' that also has .1 at the end. 
#' 
#' Additionally, other objects can be saved via the \dots argument.  For exmaple PMsave(1,lm) will
#' save any object with .1 at the end, plus an object named "lm".  All objects will be suffixed
#' with the run number when loaded back with \code{\link{PMload}}.
#'
#' @title Save Pmetrics objects
#' @param run The numerical value of the run number of the objects to be saved.
#' This parameter must be specified, as it also determines where to save the revised output.
#' @param \dots Additional objects to be saved, which do not need to be suffixed with the run number,
#' e.g. var1, var2, var3.
#' @param quiet Suppress written report.  Default is \code{FALSE}.
#' @author Michael Neely
#' @seealso \code{\link{PMload}}


PMsave <- function(run,...,quiet=F){
  if(missing(run)){
    stop("\nPlease specify a run number.")
  }
  if(!file.exists(as.character(run))) {stop(paste(run," is not a folder in the current working directory.\n",sep=""))}
  #get the file name
  npag <- length(list.files(paste(run,"outputs",sep="/"),pattern="NPAGout.Rdata"))>0
  it2b <- length(list.files(paste(run,"outputs",sep="/"),pattern="IT2Bout.Rdata"))>0
  
  if(npag){file.name <- "NPAGout.Rdata"
  } else {
    if(it2b){
      file.name <- "IT2Bout.Rdata"
    } else {stop(paste("Neither NPAGout.Rdata or IT2Bout.Rdata are in ",run,"/outputs",sep=""))}
  }
  
  
  obj <- ls(name=1,pattern=glob2rx(paste("*.",run,sep="")))
  otherObj <- unlist(deparse(substitute(...)))

  if(otherObj!="NULL"){
    allObj <- c(obj,otherObj)
  } else {allObj <- obj  }

  if(!quiet) {cat(paste("The following objects were saved to ",paste(run,"outputs",file.name,sep="/"),":\n",paste(allObj,collapse="\n"),sep=""))}
  
  #strip the numbers and save
  if(npag){
    NPAGout <- lapply(allObj,get)
    names(NPAGout) <- gsub(paste("\\.",run,sep=""),"",allObj)
    save(NPAGout,file=paste(run,"outputs/NPAGout.Rdata",sep="/"))
  } 
  if(it2b){
    IT2Bout <- lapply(allObj,get)
    names(IT2Bout) <- gsub(paste("\\.",run,sep=""),"",allObj)
    save(IT2Bout,file=paste(run,"outputs/IT2Bout.Rdata",sep="/"))
  } 
  
  
}

