#' \code{PMmb2csv} will convert old style, single drug .mb or USC*PACK files into
#'  a single .csv matrix file.
#'
#' IDs will be suffixed with .1 to .9 for <10 subjects, .01 to .99 for <100 subjects and .001 to .999 for <1000 subjects,
#' as needed to ensure unique ID numbers.
#'
#' @title Convert Old .mb or USC*PACK Files to .csv Matrix File
#' @param oldFiles A character vector of files in the current working directory to convert.  
#' This could be easily obtained with \code{\link{list.files}}.
#' @param newFile A single character vector with the basename (without any file extension) of the new file to be created.
#' @return A new file will be created with the name equal to \code{newFile} and
#'  an extension of \dQuote{csv}.
#' @author Michael Neely

PMmb2csv <- function(oldFiles,newFile="data"){
  OS <- getOS()
  #read or define the Fortran compiler
  fortSource <- switch(OS,"~/.config/Pmetrics/compiledFortran",
                       paste(Sys.getenv("APPDATA"),"\\Pmetrics\\compiledFortran",sep=""),
                       "~/.config/Pmetrics/compiledFortran")
  if(!file.exists(fortSource)){
    PMbuild()
  }
  convertFile <- "mb2csv.exe"
  #copy the OS-specific conversion file to the current wd
  file.copy(file.path(fortSource,convertFile),getwd())
  new <- data.frame()
  fmtStr <- paste("%0",4,"d",sep="")
  nsub <- length(oldFiles)
  for (i in 1:nsub){
    #make the filename of the .wrk file
    tempFilename <- paste("temp",sprintf(fmtStr,i),".wrk",sep="")
    writeLines(paste(oldFiles[i],tempFilename,sep="\n"),"tempCtrl")
    #execute the conversion to working copy
    if(OS==1 | OS==3){ #Unix/Linux
      system(paste("./",convertFile," < tempCtrl",sep=""),ignore.stdout=T)
    } else { #Windows
      shell(paste(convertFile,"< tempCtrl"),show.output.on.console=F)
    }
  
  }
  #now convert from .wrk to .csv
  PMwrk2csv(prefix="temp",nsub=nsub,ext="wrk")
  #clean up
  file.rename("temp.csv",paste(newFile,".csv",sep=""))
  file.remove(Sys.glob("temp*.wrk"))
  file.remove(convertFile)
  file.remove("tempCtrl")
}


