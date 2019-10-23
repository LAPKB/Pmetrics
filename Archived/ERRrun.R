#' Runs Assay Error Module
#'
#' \code{ERRrun} will execute an Assay Error run to estimate error polynomial coefficients.
#'
#' If all function arguments are default, the simplest execution of this command is 
#' \code{ERRrun()}.  This will result in generation of a batch file.  On Unix (Mac) systems
#' will be launched automatically in a terminal window.  On Windows systems, the user
#' must execute the batch file from the current working directory, which will launch the estimation
#' program in a command prompt (DOS-like) window.  In either case, it will run independently of R
#' so that R can be used for other purposes if desired.
#'
#' @title Execute an Assay Error Estimation run.
#' @param model Name of a suitable model file template in the working directory or
#' an existing (previous) run number corresponding to a folder in the current working directory that used the same model file as will be used in the current run.
#' If this is supplied, then the model file will be copied into the current 
#' working directory for convenience.  If not supplied, 
#' the default is \dQuote{model.txt}.  This file will be converted to a fortran model file.
#' If it is detected to already be a fortran file, then the analysis will proceed without any further
#' file conversion.
#' @param data Name of a suitable data file (see \code{\link{PMwriteMatrix}}) or
#' an existing (previous) run number corresponding to a folder in the current working directory that used the same data file as will be used in the current run.
#' If this is supplied, then previously made  '.ZMQ' files will be copied into the current 
#' working directory, bypassing the need to re-convert the .csv file and speeding up the run..
#' @param run Specify the run number of the output folder.  Default if missing is the next available number.
#' @param include Vector of subject id values in the data file to include in the analysis.  The default (missing) is all.
#' @param exclude A vector of subject IDs to exclude in the plot, e.g. c(4,6:14,16:20)
#' @param ode Ordinary Differential Equation solver log tolerance or stiffness.  Default is -4, i.e. 0.0001.  Higher values will result in faster
#' runs, but parameter estimates may not be as accurate.
#' @param salt Vector of salt fractions for each ndrug, default is 1 for each drug.  This is not the same as bioavailability.
#' @param cycles Number of cycles to run. Default is 100.
#' @param search Default is "cursory", but can be "medium" or "extensive", which take progressively
#' longer times to converge, but are more accurate.
#' @param tol Tolerance for convergence, with default of 0.001.
#' @param xdev Multiple of standard deviations for parameters to be sent to NPAG as a range.  Default is 5.
#' @param auto If \code{auto} is \code{False} you can answer all questions about the run environment manually.  This might
#' be helpful for beginners.  Default is \code{True}.
#' @param intern MacOSX only: Run ERR in the R console without a batch script.  Default is false.  
#' This will be ignored on Windows systems.  On the latter, the behavior of cmd.exe (aka the \dQuote{DOS} window)
#' with R is poor - it does not update until the end of execution, so you cannot see any output that indicates that ERR is running.  
#' If \code{intern=T} the HTML summary page will not be automatically loaded at the end of the run, but all post-run processing will occur normally,
#' and you can find the HTML summary page in the /outputs folder: ERRreport.html.
#' @param silent Boolean operator controlling whether a model summary report is given.  Default is \code{True}.
#' @param overwrite Overwrite existing run result folders.  Default is FALSE.
#' @return A successful  run will result in creation of a new folder in the working
#' directory.  This folder will be named with a date-time stamp in the format "out-YYYYMMMDD-hhmm",
#' e.g. out-2011Apr10-1015.  Under this folder will be four subfolders: etc, inputs, outputs, and
#' wrkcopy, described below.
#' \itemize{
#'  \item \bold{etc}   Control files generally not needed by the user after a completed run.
#'  \item \bold{inputs}   This folder will contain the .csv data file and the model file.
#'  \item \bold{outputs}   This folder will contain the output from the run: a file that will be
#' prefixed by ASS with appended numbers, usually 0001. This file contains all the output of the
#' run, with the estimated assay error polynomical coefficients at the end.
#' \item \bold{wrkcopy}    The working copy format which is used by the program.  Invisibly to the user,
#' the .csv input file is converted to these text files, one file per subject.  
#' }
#' 
#' @author Michael Neely
#' @seealso \code{\link{ITrun}}, \code{\link{NPrun}}


ERRrun <- function(model="model.txt",data="data.csv",run,include,exclude,
                   ode=-4,salt,cycles=100,search="cursory",
                   tol=0.001,xdev=5,auto=T,intern=F,silent=F,
                   overwrite=F){
  remote <- F
  #check for files
  #copy model file if available
  if(is.numeric(model)){
    modelfile <- suppressWarnings(tryCatch(scan(paste(model,"etc/instr.inx",sep="/"),what="character",quiet=T,skip=4,n=1),error=function(e) NULL))
    modelfile <- Sys.glob(paste(model,"/inputs/",strsplit(modelfile,"\\.")[[1]][1],"*",sep=""))
    if(length(modelfile)>0){
      file.copy(from=modelfile,to=getwd())
      model <- basename(modelfile)
    }
  }
  
  #make sure model file name is <=8 characters
  if(!FileNameOK(model)) {endNicely(paste("Model file name must be 8 characters or fewer.\n"),model=-99,data)}
  
  while(!file.exists(model)) {
    model <- readline(paste("The model file",shQuote(paste(getwd(),model)),"does not exist.\nEnter another filename or 'end' to quit: \n")) 
    if (tolower(model)=="end") {endNicely(paste("No model file specified.\n"),model=-99,data); break}
  }
  
  #copy wrk files if available
  wrkFlag <- F
  if(is.numeric(data)){
    wrkfiles <- Sys.glob(paste(data,"wrkcopy/*.ZMQ",sep="/"))
    if(length(wrkfiles)>0){
      file.copy(from=wrkfiles,to=getwd())
      wrkFlag <- T
    }
    instrfile <- suppressWarnings(tryCatch(readLines(paste(data,"etc/instr.inx",sep="/")),error=function(e) NULL))
    if(length(instrfile)>0){
      datafileLine <- grep(" BLOCKPAT",instrfile)
      datafileName <- instrfile[datafileLine+1]
      file.copy(from=paste(data,"inputs",datafileName,sep="/"),to=getwd())
      data <- datafileName
    }
  }
  
  #make sure data file name is <=8 characters
  if(!FileNameOK(data)) {endNicely(paste("Data file name must be 8 characters or fewer.\n"),model,data=-99)}
  
  #ok look for a file
  while(!file.exists(data)) {
    data <- readline(paste("The data file",shQuote(paste(getwd(),data)),"does not exist.\nEnter another filename or 'end' to quit: \n")) 
    if (tolower(data)=="end") {endNicely(paste("No data file specified.\n"),model,data=-99); break}
  }
  #get information from datafile
  dataFile <- PMreadMatrix(data,quiet=T)
  dataoffset <- 2*as.numeric("addl" %in% names(dataFile))
  ncov <- ncol(dataFile)-(12+dataoffset)
  if(ncov>0) {covnames <- names(dataFile)[(13+dataoffset):ncol(dataFile)]} else {covnames <- NA}
  numeqt <- max(dataFile$outeq,na.rm=T)
  id <- unique(dataFile$id)
  nsubtot <- length(id)
  
  if(missing(include)) include <- id
  if(!missing(exclude)) include <- id[!id %in% exclude]
  
  nsub <- length(include)
  activesub <- c(which(id %in% include),0)
  ndrug <- max(dataFile$input,na.rm=T)
  if(missing(salt)) {
    salt <- rep(1,ndrug)
  } else {
    if(length(salt)!=length(ndrug)) endNicely("\nSalt fraction length must equal number of drugs.\n",model,data)
  }
  #set fraction of ab range for initial parameter SD
  xsig=0.5
  
  
  #attempt to translate model file into separate fortran model file and instruction files
  engine <- list(alg="ERR",nsubtot=nsubtot,nsub=nsub,activesub=activesub,ncov=ncov,covnames=covnames,ndrug=ndrug,
                 salt=salt,numeqt=numeqt,cycles=cycles,xsig=xsig,tol=tol,xdev=xdev,indpts=-99,
                 ode=ode,limits=NA)
  
  trans <- makeModel(model=model,data=data,engine=engine,write=T,silent=silent)
  
  if(trans$status==-1) endNicely(trans$msg,model,data) #error
  if(trans$status==0){
    useOldFortran <- T  #old fortran file model
    auto <- F #disable automatic running
  } 
  if(trans$status==1){ #new model and instruction files made
    useOldFortran <- F
    modelFor <- trans$modelFor
    ptype <- trans$ptype
    if(!wrkFlag) {ctype <- trans$ctype} else {ctype <- 0}
    instr <- "instr.inx"
  }
  
  OS <- getOS()
  #read or define the Fortran compiler
  fortSource <- switch(OS,"~/.config/Pmetrics/compiledFortran",
                       paste(Sys.getenv("APPDATA"),"\\Pmetrics\\compiledFortran",sep=""),
                       "~/.config/Pmetrics/compiledFortran")
  if(!file.exists(fortSource)){
    PMbuild()
  }
  compiler <- PMFortranConfig()
  if(is.null(compiler)) endNicely("\nExecute ERRrun after gfortran is installed.\n",model,data)
  
  #make new ouput directory
  if(missing(run)){
    olddir <- list.dirs(recursive=F)
    olddir <- olddir[grep("^\\./[[:digit:]]+",olddir)]
    olddir <- sub("^\\./","",olddir)
    if(length(olddir)>0){
      newdir <- as.character(max(as.numeric(olddir))+1)
    } else {newdir <- "1"}
  } else {
    if(!is.numeric(run)) {endNicely("'run' must be numeric.\n",model,data)} else {newdir <- as.character(run)}
  }
  if(file.exists(newdir)){
    if(overwrite) {unlink(newdir,recursive=T)} else {endNicely(paste("\n",newdir," exists already.  Set overwrite=T to overwrite.\n",model,data))}
  }  
  rep <- c("/","\\\\","/")[OS]
  
  ode <- c(0,10**ode)
  
  searchtype <- switch(search,cursory=0,medium=1,extensive=2,"0") #default is cursory
  
  prepfiles <- shQuote(normalizePath(list.files(fortSource,pattern="ITprep",full.names=T)))
  enginefiles <- shQuote(normalizePath(list.files(fortSource,pattern="ITerr",full.names=T)))
  
  prepcompile <- sub("<exec>","err_prep",compiler,fixed=T)
  prepcompile <- sub("<files>",prepfiles,prepcompile,fixed=T)
  prepcompile <- gsub("/",rep,prepcompile)
  
  
  enginecompile <- sub("<exec>","err_run",compiler,fixed=T)
  enginecompile <- sub("<files>",enginefiles,enginecompile,fixed=T)
  enginecompile <- gsub("/",rep,enginecompile)
  
  prepfiles <- gsub("/",rep,prepfiles)
  enginefiles <- gsub("/",rep,enginefiles)
  
  if (OS==1 | OS==3){
    system(paste("echo 'Assay Error run initiated at",format(Sys.time(),"%H:%M on %Y %b %d."),
                 "\nPREP FILES:",prepfiles,"\nENGINE FILES:",enginefiles,"\n\n' > errlog.txt"))
    system(prepcompile,ignore.stderr=F)
  } else {
    shell(paste("echo 'Assay Error run initiated at",format(Sys.time(),"%H:%M on %Y %b %d."),
                "\nPREP FILES:",prepfiles,"\nENGINE FILES:",enginefiles,"\n\n' > errlog.txt"))
    shell(prepcompile,ignore.stderr=F)        
  }
  
  if((OS==1 | OS==3) & (!intern | !auto) | OS==2){ #run as batch file
    
    ERRscript <- vector("character")
    if(!remote){workdir <- gsub("/",rep,getwd())} else {workdir <- gsub("/",rep,remote[2])}
    ERRscript[getNext(ERRscript)] <- paste("cd",shQuote(workdir))
    if (!auto){
      ERRscript[getNext(ERRscript)] <- paste(c("./err_prep ","err_prep ","./err_prep ")[OS],c("MacOSX","DOS","MacOSX")[OS],sep="")
    } else {
      
      ERRcontrol <- c("1",   #using instruction file
                      "instr.inx") #name of instruction file  
      
      f <- file("ERRcontrol","w")
      writeLines(ERRcontrol,f)
      close(f)
      ERRscript[getNext(ERRscript)] <- paste(c("./err_prep ","err_prep ","./err_prep ")[OS],c("MacOSX","DOS","MacOSX")[OS]," < ERRcontrol",c(" | tee -a errlog.txt",""," | tee -a errlog.txt")[OS],sep="")
    }
    ERRscript[getNext(ERRscript)] <- "echo 1 > extnum"
    ERRscript[getNext(ERRscript)] <- "echo go > go"
    ERRscript[getNext(ERRscript)] <- paste(enginecompile,"assdriv.f",sep=" ")
    ERRscript[getNext(ERRscript)] <- paste(c("./err_run ","err_run ","./err_run ")[OS],"< go",sep="")
    ERRscript[getNext(ERRscript)] <- c("echo;echo Cleaning up....;echo","echo. & echo Cleaning up.... & echo.","echo;echo Cleaning up....;echo")[OS]
    ERRscript[getNext(ERRscript)] <- c("stty -echo","echo off","stty -echo")[OS]
    ERRscript[getNext(ERRscript)] <- paste("mkdir ",newdir,sep="")
    ERRscript[getNext(ERRscript)] <- paste("mkdir ",newdir,c("/inputs","\\inputs","/inputs")[OS],sep="")
    ERRscript[getNext(ERRscript)] <- paste("mkdir ",newdir,c("/outputs","\\outputs","/outputs")[OS],sep="")
    ERRscript[getNext(ERRscript)] <- paste("mkdir ",newdir,c("/wrkcopy","\\wrkcopy","/wrkcopy")[OS],sep="")
    ERRscript[getNext(ERRscript)] <- paste("mkdir ",newdir,c("/etc","\\etc","/etc")[OS],sep="")
    
    ERRscript[getNext(ERRscript)] <- c("if [ ! -f ASS0001 ]; then error=true; else error=false; fi",
                                       "if not exist ASS0001 (set error=1) ELSE (set error=0)",
                                       "if [ ! -f ASS0001 ]; then error=true; else error=false; fi")[OS]
    
    outlist <- c("ASS*")
    for (i in 1:length(outlist)){
      ERRscript[getNext(ERRscript)] <- paste(c(paste("if [ -f ",outlist[i]," ]; then mv ",sep=""),
                                               paste("if exist ",outlist[i]," move ",sep=""),
                                               paste("if [ -f ",outlist[i]," ]; then mv ",sep=""))[OS],
                                             outlist[i]," ",newdir,c("/outputs; fi","\\outputs","/outputs; fi")[OS],sep="")
    }
    
    if (instr!=-99){
      ERRscript[getNext(ERRscript)] <- paste(c("mv ","move ","mv ")[OS],instr," ",newdir,c("/etc","\\etc","/etc")[OS],sep="")
      ERRscript[getNext(ERRscript)] <- paste(c("mv ","move ","mv ")[OS],"errlog.txt ",newdir,c("/outputs","\\outputs","/outputs")[OS],sep="")
      ERRscript[getNext(ERRscript)] <- paste(c("mv ","move ","mv ")[OS],"ERRcontrol ",newdir,c("/etc","\\etc","/etc")[OS],sep="")
      
    }
    
    if(!useOldFortran){  #we are using the new model template
      ERRscript[getNext(ERRscript)] <- paste(c("mv ","move ","mv ")[OS],modelFor," ",newdir,c("/etc/","\\etc\\","/etc/")[OS],modelFor,sep="")  #move fortran file to etc
      ERRscript[getNext(ERRscript)] <- paste(c("mv ","move ","mv ")[OS],model," ",newdir,c("/inputs/","\\inputs\\","/inputs/")[OS],model,sep="")  #move template file to inputs
    } else {ERRscript[getNext(ERRscript)] <- paste(c("mv ","move ","mv ")[OS],model," ",newdir,c("/inputs","\\inputs","/inputs")[OS],sep="")}  #using fortran file directly, so move to inputs
    
    
    ERRscript[getNext(ERRscript)] <- paste(c("mv ","move ","mv ")[OS],"XQZPJ*.ZMQ ",newdir,c("/wrkcopy","\\wrkcopy","/wrkcopy")[OS],sep="")
    ERRscript[getNext(ERRscript)] <- paste(c("mv ","move ","mv ")[OS],"extnum ",newdir,c("/etc","\\etc","/etc")[OS],sep="")
    
    ERRscript[getNext(ERRscript)] <- paste(c("mv ","move ","mv ")[OS],"it2b*.* ",newdir,c("/etc","\\etc","/etc")[OS],sep="")
    ERRscript[getNext(ERRscript)] <- paste(c("mv ","move ","mv ")[OS],"itas*.* ",newdir,c("/etc","\\etc","/etc")[OS],sep="")
    ERRscript[getNext(ERRscript)] <- paste(c("mv ","move ","mv ")[OS],"assdriv.f ",newdir,c("/etc","\\etc","/etc")[OS],sep="")
    ERRscript[getNext(ERRscript)] <- paste(c("rm ","erase ","rm ")[OS],"fort.*",sep="")
    ERRscript[getNext(ERRscript)] <- paste(c("rm ","erase ","rm ")[OS],"go",sep="")
    ERRscript[getNext(ERRscript)] <- paste(c("mv ","move ","mv ")[OS],"err_prep ",newdir,c("/etc","\\etc","/etc")[OS],sep="")
    ERRscript[getNext(ERRscript)] <- paste(c("mv ","move ","mv ")[OS],"err_run ",newdir,c("/etc","\\etc","/etc")[OS],sep="")
    ERRscript[getNext(ERRscript)] <- paste(c("mv ","move ","mv ")[OS],data," ",newdir,c("/inputs","\\inputs","/inputs")[OS],sep="")
    
    #make report
    reportscript <- paste(normalizePath(Sys.getenv("PmetricsPath"),winslash="/"),"/Pmetrics/report/ERRrepScript.R",sep="")
    outpath <- c(paste(workdir,"/",newdir,"/outputs",sep=""),
                 paste(workdir,"\\",newdir,"\\outputs",sep=""),
                 paste(workdir,"/",newdir,"/outputs",sep=""))[OS]
    
    ERRscript[getNext(ERRscript)] <- c("if ! $error ; then ", "if %error% == 0 (","if ! $error ; then ")[OS]
    ERRscript[getNext(ERRscript)] <- c(paste(normalizePath(R.home(),winslash="/"),"/bin/Rscript ",shQuote(reportscript)," ",shQuote(outpath)," ",sep=""),
                                       paste(shQuote(paste(gsub("/",rep,normalizePath(R.home(),winslash="/")),"\\bin\\Rscript",sep=""))," ",shQuote(reportscript)," ",shQuote(outpath)," ",sep=""),
                                       paste(normalizePath(R.home(),winslash="/"),"/bin/Rscript ",shQuote(reportscript)," ",shQuote(outpath)," ",sep=""))[OS]
    ERRscript[getNext(ERRscript)] <- c(paste("open ",shQuote(paste(gsub("/",rep,outpath),"/ERRreport.html",sep=""))," ; fi",sep=""),
                                       paste("start ",shQuote("ERR Report")," ",shQuote(paste(gsub("/",rep,outpath),"\\IT2Breport.html",sep="")),")",sep=""),
                                       paste("open ",shQuote(paste(gsub("/",rep,outpath),"/ERRreport.html",sep=""))," ; fi",sep=""))[OS]
    
    
    #final clean up
    if(OS==1 | OS==3){ #for Mac or Linux
      ERRscript[getNext(ERRscript)] <- paste("mv errscript ",newdir,"/etc",sep="")
    } 
    if(OS==2){  #for Windows
      ERRscript[getNext(ERRscript)] <- paste("copy errscript.bat ",newdir,"\\etc",sep="")
      ERRscript[getNext(ERRscript)] <- paste("echo. & echo Press any key to complete run and close this window... & echo.")
      ERRscript[getNext(ERRscript)] <- paste("pause > nul")
      ERRscript[getNext(ERRscript)] <- paste("erase errscript.bat ",sep="")
    }
    
    
    ERRscript <- ERRscript[!is.na(ERRscript)]
    
    
    f <- file(c("errscript","errscript.bat","errscript")[OS],"w")
    writeLines(ERRscript,f)
    close(f)
    outpath <- ifelse(remote,paste(getwd(),"/",newdir,"/remote",sep=""),paste(getwd(),"/",newdir,"/outputs",sep=""))
    
    if (OS==1){ #Mac
      system("chmod +x errscript")
      system(paste("open -a Terminal.app ",shQuote(paste(getwd(),"/errscript",sep="")),sep=""))
    } 
    if (OS==2){ #Windows
      cat(paste("Launch errscript.bat in your working directory to execute ERRrun.\n",sep=""))
      outpath <- gsub("\\\\","/",outpath)
    }
    if (OS==3){ #Linux
      system("chmod +x npscript")
      if(!remote) {system(paste(shQuote(paste(getwd(),"/errscript &",sep="")),sep=""))
      } else { cat("Upload all files in your working directory to the server and execute errscript.\n")}
    } 
    return(outpath)
    
  } else { #run ERRrun internally
    workdir <- getwd()
    if (!auto){  #allow users to see questions
      if(OS==1 | OS==3){system("./err_prep MacOSX")} 
    } else { #we do have instructions for prep
      
      ERRcontrol <- c("1",   #using instruction file
                      "instr.inx") #name of instruction file  
      
      f <- file("ERRcontrol","w")
      writeLines(ERRcontrol,f)
      close(f)
      
      system("./err_prep MacOSX < ERRcontrol")
      
      
    }  
    
    # RUN IT2B error engine
    
    system("echo 1 > extnum")
    system("echo go > go")
    system(paste(enginecompile,"assdriv.f",sep=" "))
    system("./err_run < go")
    
    
    #CLEAN UP
    dir.create(newdir)
    dir.create(paste(newdir,"/inputs",sep=""))
    dir.create(paste(newdir,"/outputs",sep=""))
    dir.create(paste(newdir,"/wrkcopy",sep=""))
    dir.create(paste(newdir,"/etc",sep=""))
    
    outlist <- "ASS*"
    if(length(Sys.glob(outlist))>0){
      file.copy(from=Sys.glob(outlist),to=paste(newdir,"/outputs",sep=""))    
      file.remove(Sys.glob(outlist))
    }
    if (instr!=-99){
      file.copy(from=instr,to=paste(newdir,"/etc",sep=""))
      file.copy(from="errlog.txt",to=paste(newdir,"/outputs",sep=""))
      file.copy(from="ERRcontrol",to=paste(newdir,"/etc",sep="")) 
      file.copy(from=data,to=paste(newdir,"/inputs",sep=""))
      file.remove(instr)
      file.remove("errlog.txt")
      file.remove("ERRcontrol")
      file.remove(data)
    }
    
    if(!modelfor){  #we are using the new model template
      file.copy(from=modelFor,to=paste(newdir,"/etc/",modelFor,sep=""))  #move fortran file to etc
      file.copy(from=model,to=paste(newdir,"/inputs/",model,sep="")) #move template file to inputs
    } else {
      file.copy(from=model,to=paste(newdir,"/inputs",sep="")) #using fortran file directly, so move to inputs
      file.remove(model)   
    }  
    file.remove(Sys.glob("fort.*"))
    file.remove("go")
    file.copy(from=Sys.glob("XQZPJ*.ZMQ"),to=paste(newdir,"/wrkcopy",sep=""))
    file.copy(from="extnum",to=paste(newdir,"/etc",sep=""))
    file.copy(from=Sys.glob("it2b*"),to=paste(newdir,"/etc",sep=""))
    file.copy(from=Sys.glob("itas*"),to=paste(newdir,"/etc",sep=""))
    file.copy(from="assdriv.f",to=paste(newdir,"/etc",sep=""))   
    file.copy(from=Sys.glob("err_prep*"),to=paste(newdir,"/etc",sep=""))
    file.copy(from=Sys.glob("err_run*"),to=paste(newdir,"/etc",sep=""))
    file.remove(Sys.glob("XQZPJ*.ZMQ"))
    file.remove("extnum")
    file.remove(Sys.glob("it2b*"))
    file.remove(Sys.glob("itas*"))
    file.remove("assdriv.f")
    file.remove(Sys.glob("err_prep*"))
    file.remove(Sys.glob("err_run*"))
    
    #make report
    ERRreport(paste(workdir,newdir,"outputs",sep="/"))
    
    #final clean up
    setwd(workdir)
    file.copy(from=Sys.glob("*.*"),to=paste(newdir,"/inputs",sep=""))
    file.remove(Sys.glob("*.*"))
    outpath <- paste(workdir,newdir,"outputs",sep="/")
    
    return(outpath)    
    
  }
  
}
