#' \code{PMBuild} will ensure all dependent packages are installed and compile
#' Fortran source code for permanent Pmetrics modules
#'
#' @title Build Pmetrics
#' @author Michael Neely

PMbuild <- function(){
  
  #load necessary packages
  packages <- packageDescription("Pmetrics")$Suggests
  packages <- gsub("\n","",packages)
  packages <- unlist(strsplit(packages,","))
  cat("\nChecking for required packages...\n")
  for(i in packages){
    if(system.file(package=i)==""){
      if(getOption("repos")[1]=="") {setRepositories()}
      install.packages(i,repos=getOption("repos"),dependencies=T)
    }
    
  }
  currwd <- getwd()
  OS <- getOS()
  compiler <- PMFortranConfig()
  #try again just in case redefined
  compiler <- PMFortranConfig()
  #check if parallel is possible  
  if(length(compiler)==2 & get("PmetricsBit",envir=PMenv)=="64"){
    parallel <- T
  } else {parallel <- F}
  sourcedir <- system.file("code",package="Pmetrics")
  destdir <- switch(OS,"~/.config/Pmetrics/compiledFortran",
                    paste(Sys.getenv("APPDATA"),"\\Pmetrics\\compiledFortran",sep=""),
                    "~/.config/Pmetrics/compiledFortran")
  #remove old files if present
  oldfiles <- c(Sys.glob(paste(destdir,"*.o",sep="/")),Sys.glob(paste(destdir,"*.exe",sep="/")))

  if(length(oldfiles)>0) {file.remove(oldfiles)}
  #compile new files
  setwd(sourcedir)
  if(!file.exists(destdir)) dir.create(destdir,showWarnings=F)
  PMfiles <- data.frame(filename=as.character(c("NPprep","NPeng","ITprep","ITeng","ITerr","SIMeng","DOprep","DOeng","mb2csv")))
  PMfiles$path <- sapply(PMfiles$filename,function(x) 
    shQuote(list.files(getwd(),pattern=as.character(paste(x,"_[[:digit:]]+\\.f",sep="")))))
  
  
  for(i in 1:nrow(PMfiles)){
    cat(paste("\nCompiling ",i," of ",nrow(PMfiles),": ",PMfiles$filename[i],"...",sep=""))
    flush.console()
    if(PMfiles$filename[i] %in% c("DOprep","mb2csv")){ #list of compiled and linked files
      serialCommand <- sub("<exec>",paste(PMfiles$filename[i],".exe",sep=""),compiler[1])
      serialCommand <- sub("<files>",PMfiles$path[i],serialCommand)
    } else {
      serialCommand <- sub("<exec>",paste("s",PMfiles$filename[i],".o -c",sep=""),compiler[1])
      serialCommand <- sub("<files>",PMfiles$path[i],serialCommand)
    }
    serialFortstatus <- suppressWarnings(system(serialCommand,intern=T,ignore.stderr=F))
    if(!is.null(attr(serialFortstatus,"status"))){
      unlink(switch(OS,"~/.config/Pmetrics",
                    paste(Sys.getenv("APPDATA"),"\\Pmetrics",sep=""),
                    "~/.config/Pmetrics"),recursive=T)
      stop(paste("\nThere was an error compiling ",PMfiles$filename[i],".\nDid you select the right fortran compiler?  If yes, try reinstalling fortran.\nFor gfortran, log into www.lapk.org and access system-specific tips on the Pmetrics installation page (step 5).\n",sep=""))
    }
    if(i==2 & parallel){ # parallel compilation for NPAG only
      parallelCommand <- sub("<exec>",paste("p",PMfiles$filename[i],".o -c",sep=""),compiler[2])
      parallelCommand <- sub("<files>",PMfiles$path[i],parallelCommand)
      parallelFortstatus <- suppressWarnings(system(parallelCommand,intern=T,ignore.stderr=F))
      if(!is.null(attr(parallelFortstatus,"status"))){
        unlink(switch(OS,"~/.config/Pmetrics",
                      paste(Sys.getenv("APPDATA"),"\\Pmetrics",sep=""),
                      "~/.config/Pmetrics"),recursive=T)
        stop(paste("\nThere was an error compiling ",PMfiles$filename[i],".\nDid you select the right fortran compiler?  If yes, try reinstalling fortran.\nFor gfortran, log into www.lapk.org and access system-specific tips on the Pmetrics installation page (step 5).\n",sep=""))
      }
    }
    
  }
  
  
  cat("\nAll packages installed and permanent Fortran modules compiled.\n")
  flush.console()
  invisible(file.copy(from=Sys.glob(c("*.o","*.exe")),to=destdir))
  invisible(file.remove(Sys.glob(c("*.o","*.exe"))))
  fort <- paste(system.file("config",package="Pmetrics"),"newFort.txt",sep="/")
  writeLines("0",fort) #reset to zero
  setwd(currwd)
}

