#' Runs IT2B
#'
#' \code{ITrun} will execute an IT2B run.
#'
#' If all function arguments are default, the simplest execution of this command is 
#' \code{ITrun()}.  This will result in generation of a batch file.  On Unix (Mac) systems
#' will be launched automatically in a terminal window.  On Windows systems, the user
#' must execute the batch file from the current working directory, which will launch IT2B
#' in a command prompt (DOS-like) window.  In either case, IT2B will run independently of R
#' so that R can be used for other purposes if desired.
#'
#' @title Execute an IT2B run.
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
#' @param tol Tolerance for convergence, with default of 0.001.
#' @param xdev Multiple of standard deviations for parameters to be sent to NPAG as a range.  Default is 5.
#' @param icen Summary of parameter distributions to be used to calculate predictions.  Default is "median", but could be "mean".
#' @param auto If \code{auto} is \code{False} you can answer all questions about the run environment manually.  This might
#' be helpful for beginners.  Default is \code{True}.
#' @param intern MacOSX only: Run IT2B in the R console without a batch script.  Default is false.  
#' This will be ignored on Windows systems.  On the latter, the behavior of cmd.exe (aka the \dQuote{DOS} window)
#' with R is poor - it does not update until the end of execution, so you cannot see any output that indicates that IT2B is running.  
#' If \code{intern=T} the HTML summary page will not be automatically loaded at the end of the run, but all post-run processing will occur normally,
#' and you can find the HTML summary page in the /outputs folder: IT2Breport.html.
#' @param silent Boolean operator controlling whether a model summary report is given.  Default is \code{True}.
#' @return A successful IT2B run will result in creation of a new folder in the working
#' directory.  This folder will be named with a date-time stamp in the format "out-YYYYMMMDD-hhmm",
#' e.g. out-2011Apr10-1015.  Under this folder will be four subfolders: etc, inputs, outputs, and
#' wrkcopy, described below.
#' @param overwrite Overwrite existing run result folders.  Default is FALSE.
#' \itemize{
#'  \item \bold{etc}   Control files for IT2B generally not needed by the user after a completed run.
#'  \item \bold{inputs}   This folder will contain the .csv data file and the model file.
#'  \item \bold{outputs}   This folder will contain the output from the IT2B run.  These files will be
#' prefixed by DENF, ILOG, OUTF, OUFF, LAST, FROM and RFILE, with appended numbers, usually 0001.
#' DEN is the density file which contains the joint posterior density which can be passed to IT2B.
#' OUTF and OUFF are full and truncated textfiles containing all output of IT2B.  OUFF is missing
#' density file.  LAST contains last cycle Bayesian posterior parameters and predictions for
#' each subject.  FROM contains estimated parameter ranges which can be passed to IT2B.
#' RFILE contains IT2B output formatted for easy import into R, and is the file read by
#' the \code{\link{ITparse}} command.  Finally, there will also be an itlog.txt file
#' containing additional run information.
#' \item \bold{wrkcopy}    The working copy format which is used by IT2B.  Invisibly to the user,
#' the .csv input file is converted to these text files, one file per subject.  
#' }
#' 
#' @author Michael Neely
#' @seealso \code{\link{ITparse}}, \code{\link{NPrun}}


ITrun <- function(model="model.txt",data="data.csv",run,
                  include,exclude,ode=-4,salt,cycles=100,
                  tol=0.001,xdev=5,icen="median",
                  auto=T,intern=F,silent=F,overwrite=F){
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
  
  modeltxt <- model
  #attempt to translate model file into separate fortran model file and instruction files
  engine <- list(alg="IT",nsubtot=nsubtot,nsub=nsub,activesub=activesub,ncov=ncov,covnames=covnames,ndrug=ndrug,
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
  if(is.null(compiler)) endNicely("\nExecute ITrun after gfortran is installed.\n",model,data)
  
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
  
  
  prepfiles <- shQuote(normalizePath(list.files(fortSource,pattern="ITprep",full.names=T)))
  enginefiles <- shQuote(normalizePath(list.files(fortSource,pattern="ITeng",full.names=T)))
  
  prepcompile <- sub("<exec>","it_prep",compiler,fixed=T)
  prepcompile <- sub("<files>",prepfiles,prepcompile,fixed=T)
  prepcompile <- gsub("/",rep,prepcompile)
  
  
  enginecompile <- sub("<exec>","it_run",compiler,fixed=T)
  enginecompile <- sub("<files>",enginefiles,enginecompile,fixed=T)
  enginecompile <- gsub("/",rep,enginecompile)
  
  prepfiles <- gsub("/",rep,prepfiles)
  enginefiles <- gsub("/",rep,enginefiles)
  
  if (OS==1 | OS==3){
    system(paste("echo 'IT2B run initiated at",format(Sys.time(),"%H:%M on %Y %b %d."),
                 "\nPREP FILES:",prepfiles,"\nENGINE FILES:",enginefiles,"\n\n' > itlog.txt"))
    system(prepcompile,ignore.stderr=F)
  } else {
    shell(paste("echo 'IT2B run initiated at",format(Sys.time(),"%H:%M on %Y %b %d."),
                "\nPREP FILES:",prepfiles,"\nENGINE FILES:",enginefiles,"\n\n' > itlog.txt"))
    shell(prepcompile,ignore.stderr=F)        
  }
  
  if((OS==1 | OS==3) & (!intern | !auto) | OS==2){ #run as batch file
    
    ITscript <- vector("character")
    if(!remote){workdir <- gsub("/",rep,getwd())} else {workdir <- gsub("/",rep,remote[2])}
    ITscript[getNext(ITscript)] <- paste("cd",shQuote(workdir))
    if (!auto){
      ITscript[getNext(ITscript)] <- paste(c("./it_prep ","it_prep ","./it_prep ")[OS],c("MacOSX","DOS","MacOSX")[OS],sep="")
    } else {
      
      ITcontrol <- c("1",   #using instruction file
                     "instr.inx") #name of instruction file  
      
      f <- file("ITcontrol","w")
      writeLines(ITcontrol,f)
      close(f)
      ITscript[getNext(ITscript)] <- paste(c("./it_prep ","it_prep ","./it_prep ")[OS],c("MacOSX","DOS","MacOSX")[OS]," < ITcontrol",c(" | tee -a itlog.txt",""," | tee -a itlog.txt")[OS],sep="")
    }  
    ITscript[getNext(ITscript)] <- "echo 1 > extnum"
    ITscript[getNext(ITscript)] <- "echo go > go"
    ITscript[getNext(ITscript)] <- paste(enginecompile,"it2bdriv.f",sep=" ")
    ITscript[getNext(ITscript)] <- paste(c("./it_run ","it_run ","./it_run ")[OS],"< go",sep="")
    ITscript[getNext(ITscript)] <- c("echo;echo Cleaning up....;echo","echo. & echo Cleaning up.... & echo.","echo;echo Cleaning up....;echo")[OS]
    ITscript[getNext(ITscript)] <- c("stty -echo","echo off","stty -echo")[OS]
    ITscript[getNext(ITscript)] <- paste("mkdir ",newdir,sep="")
    ITscript[getNext(ITscript)] <- paste("mkdir ",newdir,c("/inputs","\\inputs","/inputs")[OS],sep="")
    ITscript[getNext(ITscript)] <- paste("mkdir ",newdir,c("/outputs","\\outputs","/outputs")[OS],sep="")
    ITscript[getNext(ITscript)] <- paste("mkdir ",newdir,c("/wrkcopy","\\wrkcopy","/wrkcopy")[OS],sep="")
    ITscript[getNext(ITscript)] <- paste("mkdir ",newdir,c("/etc","\\etc","/etc")[OS],sep="")
    
    outlist <- c("DENF*","FROM*","LAST*","OUFF*","OUTF0*","IT_RF*")
    ITscript[getNext(ITscript)] <- paste("echo",data,">> IT_RF0001.TXT*")
    
    
    ITscript[getNext(ITscript)] <- c("if [ ! -f IT_RF0001.TXT ]; then error=true; else error=false; fi","if not exist IT_RF0001.TXT (set error=1) ELSE (set error=0)")[OS]
    for (i in 1:6){
      ITscript[getNext(ITscript)] <- paste(c(paste("if [ -f ",outlist[i]," ]; then mv ",sep=""),
                                             paste("if exist ",outlist[i]," move ",sep=""),
                                             paste("if [ -f ",outlist[i]," ]; then mv ",sep=""))[OS],
                                           outlist[i]," ",newdir,c("/outputs; fi","\\outputs")[OS],sep="")
    }
    
    if (instr!=-99){
      ITscript[getNext(ITscript)] <- paste(c("mv ","move ","mv ")[OS],instr," ",newdir,c("/etc","\\etc","/etc")[OS],sep="")
      ITscript[getNext(ITscript)] <- paste(c("mv ","move ","mv ")[OS],"itlog.txt ",newdir,c("/outputs","\\outputs","/outputs")[OS],sep="")
      ITscript[getNext(ITscript)] <- paste(c("mv ","move ","mv ")[OS],"ITcontrol ",newdir,c("/etc","\\etc","/etc")[OS],sep="")
      
    }
    
    if(!useOldFortran){  #we are using the new model template
      ITscript[getNext(ITscript)] <- paste(c("mv ","move ","mv ")[OS],modelFor," ",newdir,c("/etc/","\\etc\\","/etc/")[OS],modelFor,sep="")  #move fortran file to etc
      ITscript[getNext(ITscript)] <- paste(c("mv ","move ","mv ")[OS],model," ",newdir,c("/inputs/","\\inputs\\","/inputs/")[OS],model,sep="")  #move template file to inputs
    } else {ITscript[getNext(ITscript)] <- paste(c("mv ","move ","mv ")[OS],model," ",newdir,c("/inputs","\\inputs","/inputs")[OS],sep="")}  #using fortran file directly, so move to inputs
    
    
    ITscript[getNext(ITscript)] <- paste(c("mv ","move ","mv ")[OS],"XQZPJ*.ZMQ ",newdir,c("/wrkcopy","\\wrkcopy","/wrkcopy")[OS],sep="")
    ITscript[getNext(ITscript)] <- paste(c("mv ","move ","mv ")[OS],"extnum ",newdir,c("/etc","\\etc","/etc")[OS],sep="")
    
    ITscript[getNext(ITscript)] <- paste(c("mv ","move ","mv ")[OS],"it2b*.* ",newdir,c("/etc","\\etc","/etc")[OS],sep="")
    ITscript[getNext(ITscript)] <- paste(c("mv ","move ","mv ")[OS],"itas*.* ",newdir,c("/etc","\\etc","/etc")[OS],sep="")
    ITscript[getNext(ITscript)] <- paste(c("rm ","erase ","rm ")[OS],"fort.*",sep="")
    ITscript[getNext(ITscript)] <- paste(c("rm ","erase ","rm ")[OS],"go",sep="")
    ITscript[getNext(ITscript)] <- paste(c("mv ","move ","mv ")[OS],"it_prep* ",newdir,c("/etc","\\etc","/etc")[OS],sep="")
    ITscript[getNext(ITscript)] <- paste(c("mv ","move ","mv ")[OS],"it_run* ",newdir,c("/etc","\\etc","/etc")[OS],sep="")
    ITscript[getNext(ITscript)] <- paste(c("mv ","move ","mv ")[OS],data," ",newdir,c("/inputs","\\inputs","/inputs")[OS],sep="")
    
    #make report
    reportscript <- paste(normalizePath(Sys.getenv("PmetricsPath"),winslash="/"),"/Pmetrics/report/ITrepScript.R",sep="")
    outpath <- c(paste(workdir,"/",newdir,"/outputs",sep=""),
                 paste(workdir,"\\",newdir,"\\outputs",sep=""),
                 paste(workdir,"/",newdir,"/outputs",sep=""))[OS]
    
    
    ITscript[getNext(ITscript)] <- c("if ! $error ; then ", "if %error% == 0 (","if ! $error ; then ")[OS]
    ITscript[getNext(ITscript)] <- c(paste(normalizePath(R.home(),winslash="/"),"/bin/Rscript ",shQuote(reportscript)," ",shQuote(outpath)," ",icen,sep=""),
                                     paste(shQuote(paste(gsub("/",rep,normalizePath(R.home(),winslash="/")),"\\bin\\Rscript",sep=""))," ",shQuote(reportscript)," ",shQuote(outpath)," ",icen,sep=""),
                                     paste(normalizePath(R.home(),winslash="/"),"/bin/Rscript ",shQuote(reportscript)," ",shQuote(outpath)," ",icen,sep=""))[OS]
    ITscript[getNext(ITscript)] <- c(paste("open ",shQuote(paste(gsub("/",rep,outpath),"/IT2Breport.html",sep=""))," ; fi",sep=""),
                                     paste("start ",shQuote("IT2B Report")," ",shQuote(paste(gsub("/",rep,outpath),"\\IT2Breport.html",sep="")),")",sep=""),
                                     paste("open ",shQuote(paste(gsub("/",rep,outpath),"/IT2Breport.html",sep=""))," ; fi",sep=""))[OS]
    
    #final clean up
    if(OS==1 | OS==3){ #for Mac or Linux
      ITscript[getNext(ITscript)] <- paste("mv itscript ",newdir,"/etc",sep="")
    } 
    if(OS==2){  #for Windows
      ITscript[getNext(ITscript)] <- paste("copy itscript.bat ",newdir,"\\etc",sep="")
      ITscript[getNext(ITscript)] <- paste("echo. & echo Press any key to complete run and close this window... & echo.")
      ITscript[getNext(ITscript)] <- paste("pause > nul")
      ITscript[getNext(ITscript)] <- paste("erase itscript.bat ",sep="")
    }
    
    ITscript <- ITscript[!is.na(ITscript)]
    
    
    f <- file(c("itscript","itscript.bat","itscript")[OS],"w")
    writeLines(ITscript,f)
    close(f)
    
    if (OS==1){ #Mac
      system("chmod +x itscript")
      if(!remote) {system(paste("open -a Terminal.app ",shQuote(paste(getwd(),"/itscript",sep="")),sep=""))
      } else { cat("Upload all files in your working directory to the server and execute itscript.\n")}
      
    } 
    if (OS==2){ #Windows
      if(!remote) {cat(paste("Launch itscript.bat in your working directory to execute the IT2B run.\n",sep=""))
      } else {cat("Upload all files in your working directory to the server and execute itscript.bat.\n")}
      outpath <- gsub("\\\\","/",outpath)
    }
    if (OS==3){ #Linux
      system("chmod +x npscript")
      if(!remote) {system(paste(shQuote(paste(getwd(),"/itscript &",sep="")),sep=""))
      } else { cat("Upload all files in your working directory to the server and execute itscript.\n")}
    } 
    
    return(outpath)
    
  } else { #run IT2B internally
    workdir <- getwd()
    if (!auto){  #allow users to see questions
      if(OS==1 | OS==3){system("./it_prep MacOSX")} 
    } else { #we do have instructions for prep
      
      ITcontrol <- c("1",   #using instruction file
                     "instr.inx") #name of instruction file  
      
      f <- file("ITcontrol","w")
      writeLines(ITcontrol,f)
      close(f)
      
      if(OS==1 | OS==3){system("./it_prep MacOSX < ITcontrol")} else {shell("it_prep DOS < ITcontrol")}
      
    }  
    
    # RUN IT2B engine
    
    system("echo 1 > extnum")
    system("echo go > go")
    system(paste(enginecompile,"it2bdriv.f",sep=" "))
    system("./it_run < go")
    
    #CLEAN UP
    dir.create(newdir)
    dir.create(paste(newdir,"/inputs",sep=""))
    dir.create(paste(newdir,"/outputs",sep=""))
    dir.create(paste(newdir,"/wrkcopy",sep=""))
    dir.create(paste(newdir,"/etc",sep=""))
    
    outlist <- c("DENF*","FROM*","LAST*","OUFF*","OUTF0*","IT_RF*")
    if(file.exists(Sys.glob("IT_RF*"))){
      itfile <- file(Sys.glob("IT_RF*"),open="a")
      writeLines(data,itfile)
      close(itfile)
      file.copy(from=Sys.glob(outlist),to=paste(newdir,"/outputs",sep=""))    
      file.remove(Sys.glob(outlist))
    }
    if (instr!=-99){
      file.copy(from=instr,to=paste(newdir,"/etc",sep=""))
      file.copy(from="itlog.txt",to=paste(newdir,"/outputs",sep=""))
      file.copy(from="ITcontrol",to=paste(newdir,"/etc",sep="")) 
      file.copy(from=data,to=paste(newdir,"/inputs",sep=""))
      file.remove(instr)
      file.remove("itlog.txt")
      file.remove("ITcontrol")
      file.remove(data)
    }
    
    if(!useOldFortran){  #we are using the new model template
      file.copy(from=modelFor,to=paste(newdir,"/etc/",modelFor,sep=""))  #move fortran file to etc
      file.copy(from=model,to=paste(newdir,"/inputs/",model,sep="")) #move template file to inputs
    } else {
      file.copy(from=model,to=paste(newdir,"/inputs",sep="")) #using fortran file directly, so move to inputs
      file.remove(model)   
    }  
    
    #file.remove(Sys.glob("JUNK*"))
    file.remove(Sys.glob("fort.*"))
    file.remove("go")
    file.copy(from=Sys.glob("XQZPJ*.ZMQ"),to=paste(newdir,"/wrkcopy",sep=""))
    file.copy(from="extnum",to=paste(newdir,"/etc",sep=""))
    file.copy(from=Sys.glob("it2b*"),to=paste(newdir,"/etc",sep=""))
    file.copy(from=Sys.glob("itas*"),to=paste(newdir,"/etc",sep=""))
    
    file.copy(from=Sys.glob("it_prep*"),to=paste(newdir,"/etc",sep=""))
    file.copy(from=Sys.glob("it_run*"),to=paste(newdir,"/etc",sep=""))
    file.remove(Sys.glob("XQZPJ*.ZMQ"))
    file.remove("extnum")
    file.remove(Sys.glob("it2b*"))
    file.remove(Sys.glob("itas*"))
    file.remove(Sys.glob("it_prep*"))
    file.remove(Sys.glob("it_run*"))
    
    
    #make report
    ITreport(paste(workdir,newdir,"outputs",sep="/"),icen=icen)
    
    #final clean up
    setwd(workdir)
    file.copy(from=Sys.glob("*.*"),to=paste(newdir,"/inputs",sep=""))
    file.remove(Sys.glob("*.*"))
    outpath <- paste(workdir,newdir,"outputs",sep="/")
    
    return(outpath)    
    
  }
  
}
