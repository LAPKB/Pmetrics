#' \code{PMBuild} will ensure all dependent packages are installed and compile
#' Fortran source code for permanent Pmetrics modules
#'
#' @title Build Pmetrics
#' @author Michael Neely

PMbuild <- function(){
  # These should be passed in:
  ODEsolver <- c("dvode_v1.f90") # stand alone package, w/module included in f90 file
  NPAGutils <- c("npag_utils.f90") # must be compiled ( -c ) prior to compiling NPAG
  
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
  #try again just in case redefined (i.e. \n was inserted above, get rid of it w/another call)
  compiler <- PMFortranConfig()
  #check if parallel is possible  
  if(length(compiler)==2 & getBits()=="64"){
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
#  PMfiles <- data.frame(filename=as.character(c("NPprep","NPeng")))
  PMfiles$path <- sapply(PMfiles$filename,function(x) 
    shQuote(list.files(getwd(),pattern=as.character(paste(x,"_[[:digit:]]+\\.f",sep="")))))
  
# Create ODEsolver object files (serial and parallel)
  serialCommand <- sub("<exec>",paste("s","ODEsolver",".o -c",sep=""),compiler[1])
  serialCommand <- sub("<files>",ODEsolver,serialCommand)
  serialFortstatus <- suppressWarnings(system(serialCommand,intern=T,ignore.stderr=F))
  if(parallel){
    parallelCommand <- sub("<exec>",paste("p","ODEsolver",".o -c",sep=""),compiler[2])
    parallelCommand <- sub("<files>",ODEsolver,parallelCommand)
    parallelFortstatus <- suppressWarnings(system(parallelCommand,intern=T,ignore.stderr=F))
    if(!is.null(attr(parallelFortstatus,"status"))){
      unlink(switch(OS,"~/.config/Pmetrics",
                    paste(Sys.getenv("APPDATA"),"\\Pmetrics",sep=""),
                    "~/.config/Pmetrics"),recursive=T)
      stop(paste("\nThere was an error compiling ",ODEsolver,".\nDid you select the right fortran compiler?  If yes, try reinstalling fortran.\nFor gfortran, log into www.lapk.org and access system-specific tips on the Pmetrics installation page (step 5).\n",sep=""))
    }
  }

# Create NPAGutils module
  serialCommand <- sub("<exec>",paste("s","npag_utils",".o -c",sep=""),compiler[1])
  serialCommand <- sub("<files>",NPAGutils,serialCommand); serialCommand
  serialFortstatus <- suppressWarnings(system(serialCommand,intern=T,ignore.stderr=F))
  if(parallel){
    parallelCommand <- sub("<exec>",paste("p","npag_utils",".o -c",sep=""),compiler[2])
    parallelCommand <- sub("<files>",NPAGutils,parallelCommand); parallelCommand
    parallelFortstatus <- suppressWarnings(system(parallelCommand,intern=T,ignore.stderr=F))
    if(!is.null(attr(parallelFortstatus,"status"))){
      unlink(switch(OS,"~/.config/Pmetrics",
                    paste(Sys.getenv("APPDATA"),"\\Pmetrics",sep=""),
                    "~/.config/Pmetrics"),recursive=T)
      stop(paste("\nThere was an error compiling ",NPAGutils,".\nDid you select the right fortran compiler?  If yes, try reinstalling fortran.\nFor gfortran, log into www.lapk.org and access system-specific tips on the Pmetrics installation page (step 5).\n",sep=""))
    }
  }
# Above strategy no good: npag<ver>.f USEs npag_utils.mod; there is no source code 
# switch available to choose <s,p>npag_utils.mod, depending on the compiler
# flag -fopenmp
# Instead, precompile npag_utils.mod for each file in PMfiles:
# Compile the Pmetrics programs ; note: npag_utils.mod must be pre-compiled
# NOT SO! *.mod contains the interface, but *.o contains the compiled module
# Use above block to make npag_utils.mod, snpag_utils.o, and pnpag_utils.o
# and copy all three files to destdir = ~./config/Pmetrics/compiledFortran/
# and remember to link to *.mod and s/p*.o in PMrun()
  for(i in 1:nrow(PMfiles)){
    cat(paste("\nCompiling ",i," of ",nrow(PMfiles),": ",PMfiles$filename[i],"...",sep=""))
    flush.console()
    if(PMfiles$filename[i] %in% c("DOprep","mb2csv")){ #list of compiled and linked files
      serialCommand <- sub("<exec>",paste(PMfiles$filename[i],".exe",sep=""),compiler[1])
      serialCommand <- sub("<files>",PMfiles$path[i],serialCommand)
    } else if(PMfiles$filename[i] %in% c("NPeng")) {
      # npag_utils.mod/o
      # compUtilsCommand <- sub("<exec>",paste("npag_utils",".mod -c",sep=""),compiler[1])
      # compUtilsCommand <- sub("-o <exec>",c(" -c"),compiler[1])
      # compUtilsCommand <- sub("<files>",NPAGutils,compUtilsCommand)
      # sNPeng_<NNN>
      # serialCommand <- paste(compUtilsCommand,";",sub("<exec>",paste("s",PMfiles$filename[i],".o -c",sep=""),compiler[1]),sep=" ")
      serialCommand <- sub("<exec>",paste("s",PMfiles$filename[i],".o -c",sep=""),compiler[1])
      serialCommand <- sub("<files>",PMfiles$path[i],serialCommand) 
      # serialCommand <- sub("<files>",paste("snpag_utils.o",PMfiles$path[i],sep=" "),serialCommand) 
      # cat(serialCommand)
      # getwd()
      # Save the npag_util.mod/o files
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
    if(i==2 & parallel){ # parallel compilation for NPAG only *** BUG :: Assumed order of compiled files !!!
      # npag_utils.mod
      # compUtilsCommand <- sub("-o <exec>",c("-c"),compiler[2])
      # compUtilsCommand <- sub("<files>",NPAGutils,compUtilsCommand)
      # pNPeng_<NNN>
      # parallelCommand <- paste(compUtilsCommand,";",sub("<exec>",paste("p",PMfiles$filename[i],".o -c",sep=""),compiler[2]),sep=" ")
      # parallelCommand <- sub("<files>",PMfiles$path[i],parallelCommand)
      parallelCommand <- sub("<exec>",paste("p",PMfiles$filename[i],".o -c",sep=""),compiler[2])
      parallelCommand <- sub("<files>",PMfiles$path[i],parallelCommand)
      # cat(parallelCommand)
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
  invisible(file.copy(from=Sys.glob(c("*.o","*.exe","*.mod")),to=destdir))
  invisible(file.remove(Sys.glob(c("*.o","*.exe"))))
  fort <- paste(system.file("config",package="Pmetrics"),"newFort.txt",sep="/")
  writeLines("0",fort) #reset to zero
  setwd(currwd)
}

