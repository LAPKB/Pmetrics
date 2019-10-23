#' Runs NPAG
#'
#' \code{NPrun} will execute an NPAG run.
#'
#' If all function arguments are default, the simplest execution of this command is 
#' \code{NPrun()}.  This will result in generation of a batch file.  On Unix (Mac) systems
#' will be launched automatically in a terminal window.  On Windows systems, the user
#' must execute the batch file from the current working directory, which will launch NPAG
#' in a command prompt (DOS-like) window.  In either case, NPAG will run independently of R
#' so that R can be used for other purposes if desired.
#'
#' @title Execute an NPAG run.
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
#' @param exclude A vector of subject IDs to exclude in the analysis, e.g. c(4,6:14,16:20)
#' @param ode Ordinary Differential Equation solver log tolerance or stiffness.  Default is -4, i.e. 0.0001.  Higher values will result in faster
#' runs, but parameter estimates may not be as accurate.
#' @param tol Tolerance for convergence of NPAG.  Smaller numbers make it harder to converge.
#' Default value is 0.01.
#' @param salt Vector of salt fractions for each drug in the data file, default is 1 for each drug.  This is not the same as bioavailability.
#' @param cycles Number of cycles to run. Default is 100.
#' @param indpts Index of starting grid point number.  Default is missing, which allows NPAG to choose depending on the number of random parameters: 
#' 1 or 2 = index of 1; 3 = 3; 4 = 4, 5 = 6,
#' 6 or more is 10+number of multiples for each parameter greater than 5, e.g. 6 = 101; 7 = 102, up to 108 for 13 or more parameters.
#' @param icen Summary of parameter distributions to be used to calculate predictions.  Default is "median", and other choices are "mean" or "mode".
#' @param aucint Interval for AUC calculations.  Default is 24 hours if the number of intervals is not greater than 48; otherwise it defaults
#' to the interval which allows for <= 48 intervals.
#' @param idelta Interval in minutes for predictions at times other than observations.  Default is 12.
#' @param prior Name of a suitable NPAG output object from a prior run loaded with \code{\link{NPload}},
#' i.e. the \emph{NPdata} object.  A \code{prior} may be specified if the user wishes to
#' start from a non-uniform prior distribution for the NPAG run. The default value is -99,
#' which translates in NPAG to a uniform prior distribution.  An alternative is to include a DEN0001 file from the prior
#' NPAG run in the working directory of the new run, and specify this as the value for \code{prior}, e.g.
#' \code{prior = 'DEN0001'}.
#' @param auto If \code{auto} is \code{False} you can answer all questions about the run environment manually.  This might
#' be helpful for beginners.  Default is \code{True}.
#' @param intern MacOSX only: Run NPAG in the R console without a batch script.  Default is false.  
#' This will be ignored if on Windows systems.  On the latter, the behavior of cmd.exe (aka the \dQuote{DOS} window)
#' with R is poor - it does not update until the end of execution, so you cannot see any output that indicates that NPAG is running.  
#' If \code{intern=T} the HTML summary page will not be automatically loaded at the end of the run, but all post-run processing will occur normally,
#' and you can find the HTML summary page in the /outputs folder: NPAGreport.html.
#' @param silent Boolean operator controlling whether a model summary report is given.  Default is \code{True}.
#' @param overwrite Overwrite existing run result folders.  Default is FALSE.
#' @return A successful NPAG run will result in creation of a new folder in the working
#' directory.  This folder will be named with a date-time stamp in the format "out-YYYYMMMDD-hhmm",
#' e.g. out-2011Apr10-1015.  Under this folder will be four subfolders: etc, inputs, outputs, and
#' wrkcopy, described below.
#' \itemize{
#'  \item \bold{etc}   Control files for NPAG generally not needed by the user after a completed run.
#'  \item \bold{inputs}   This folder will contain the .csv data file and the model file.
#'  \item \bold{outputs}   This folder will contain the output from the NPAG run.  These files will be
#' prefixed by DEN, ILOG, OUT, OUTT, PRTB and RFILE, with appended numbers, usually 0001.
#' DEN is the density file which can be used to specifiy a non-uniform prior parameter value
#' distribution for a subsequent NPAG run of the same model via the \code{prior} argument
#' above.  ILOG is a summary of cycle objective function values, gamma/lambda, and gridpoints.
#' OUT and OUTT are full and truncated textfiles containing all output of NPAG.  OUTT is missing
#' density file.  PRTB contains Bayesian posterior individual predictions for each subject and
#' output at timepoints specified in the NPAG instructions (e.g. every 2, 4, 8, 12 minutes) as well
#' as predictions at each observation time.  RFILE contains NPAG output formatted for easy import
#' into R, and is the file read by the \code{\link{NPparse}} command.  Finally, there will also
#' be an nplog.txt file containing additional run information.
#' \item \bold{wrkcopy}    The working copy format which is used by NPAG.  Invisibly to the user,
#' the .csv input file is converted to these text files, one file per subject.  
#' }
#' 
#' @author Michael Neely
#' @seealso \code{\link{NPparse}}, \code{\link{ITrun}}


NPrun <- function(model="model.txt",data="data.csv",run,
                  include,exclude,ode=-4,tol=0.01,salt,cycles=100,
                  indpts,icen="median",aucint,
                  idelta=12,prior,
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
  if(missing(aucint)){
    maxTime <- max(dataFile$time,na.rm=T)
    if(maxTime>24*48) {aucint <- ceiling(maxTime/48)} else {aucint <- 24}
  }
  if(missing(indpts)) indpts <- -99
  
  #check if prior and if so, get name for instruction file
  if(missing(prior)) {
    prior <- -99
    priorName <- 1
  } else {
    if(inherits(prior,"NPAG")){priorName <- c(0,"prior.txt")} else {priorName <- c(0,prior)}
  }
  
  xmic <- 1
  
  
  
  #attempt to translate model and data files into separate fortran model  and instruction files
  engine <- list(alg="NP",nsubtot=nsubtot,nsub=nsub,activesub=activesub,ncov=ncov,covnames=covnames,ndrug=ndrug,tol=tol,
                 salt=salt,numeqt=numeqt,cycles=cycles,icen=icen,indpts=indpts,aucint=aucint,idelta=idelta,xmic=xmic,
                 ode=ode,limits=NA,priorName=priorName)
  
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
  
  fortSource <- switch(OS,"~/.config/Pmetrics/compiledFortran",
                       paste(Sys.getenv("APPDATA"),"\\Pmetrics\\compiledFortran",sep=""),
                       "~/.config/Pmetrics/compiledFortran")
  if(!file.exists(fortSource)){
    PMbuild()
  }
  compiler <- PMFortranConfig()
  if(is.null(compiler)) endNicely("\nExecute NPrun after gfortran is installed.\n",model,data)
  
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
  
  prepfiles <- shQuote(normalizePath(list.files(fortSource,pattern="NPprep",full.names=T)))
  enginefiles <- shQuote(normalizePath(list.files(fortSource,pattern="NPeng",full.names=T)))
  
  prepcompile <- sub("<exec>","np_prep",compiler)
  prepcompile <- sub("<files>",prepfiles,prepcompile,fixed=T)
  
  enginecompile <- sub("<exec>","np_run",compiler)
  enginecompile <- sub("<files>",enginefiles,enginecompile,fixed=T)
  
  prepfiles <- gsub("/",rep,prepfiles)
  enginefiles <- gsub("/",rep,enginefiles)
  
  if (OS==1 | OS==3){
    system(paste("echo 'NPAG run initiated at",format(Sys.time(),"%H:%M on %Y %b %d."),
                 "\nPREP FILES:",prepfiles,"\nENGINE FILES:",enginefiles,"\n\n' > nplog.txt"))
    system(prepcompile,ignore.stderr=F)
  } else {
    shell(paste("echo 'NPAG run initiated at",format(Sys.time(),"%H:%M on %Y %b %d."),
                "\nPREP FILES:",prepfiles,"\nENGINE FILES:",enginefiles,"\n\n' > nplog.txt"))
    shell(prepcompile,ignore.stderr=F)        
  }
  
  #if(OS==1 & (!intern | !auto) | OS==2){ #run as batch file
  if(!intern | !auto){ #run as batch file
    
    
    NPscript <- vector("character")
    if(!(remote)){workdir <- gsub("/",rep,getwd())} else {workdir <- gsub("/",rep,remote[2])}
    NPscript[getNext(NPscript)] <- paste("cd",shQuote(workdir))
    if (!auto){
      NPscript[getNext(NPscript)] <- paste(c("./np_prep ","np_prep ","./np_prep")[OS],c("MacOSX","DOS","MacOSX")[OS],sep="")
    } else {      
      if (inherits(prior,"NPAG")){ 
        nvar <- trans$nvar
        prior$ab <- as.matrix(trans$ab)
        prior$popPoints <- makeFinal(prior)$popPoints
        for(i in 1:nvar){
          if(prior$ab[i,1] > min(prior$popPoints[,i])) endNicely(paste("You have changed ",prior$par[i]," so that the minimum range of ",prior$ab[i,1]," is greater than the minimum prior point value of ",min(prior$popPoints[,i]),"\nThis will cause NPAG to crash.\n",sep=""),model,data)
          if(prior$ab[i,2] < max(prior$popPoints[,i])) endNicely(paste("You have changed ",prior$par[i]," so that the maximum range of ",prior$ab[i,2],"is less than the maximum prior point value of ",max(prior$popPoints[,i]),"\nThis will cause NPAG to crash.\n",sep=""),model,data)
        }
        err <- makeDen(prior,F)
        if(err==-1) stop("\nYour NPdata prior object is older and does not contain the number of dimensions in your model.\nRe-run your NPAG analysis with Pmetrics 0.25 or later before bootstrapping.\n")
        prior <- c(0,"prior.txt")
      } else {
        if(prior != -99){
          prior <- c(0,prior) } else {prior <- 1}
      }
      
      NPcontrol <- c("1",   #using instruction file
                     "instr.inx") #name of instruction file    
      f <- file("NPcontrol","w")
      writeLines(NPcontrol,f)
      close(f)
      NPscript[getNext(NPscript)] <- paste(c("./np_prep ","np_prep ","./np_prep ")[OS],c("MacOSX","DOS","MacOSX")[OS]," < NPcontrol",c(" | tee -a nplog.txt",""," | tee -a nplog.txt")[OS],sep="")
      
    }  
    NPscript[getNext(NPscript)] <- "echo 1 > extnum"
    NPscript[getNext(NPscript)] <- "echo go > go"
    NPscript[getNext(NPscript)] <- paste(enginecompile,"npagdriv.f",sep=" ")
    NPscript[getNext(NPscript)] <- paste(c("./np_run ","np_run ","./np_run ")[OS],"< go",sep="")
    NPscript[getNext(NPscript)] <- c("echo;echo Cleaning up....;echo","echo. & echo Cleaning up.... & echo.","echo;echo Cleaning up....;echo")[OS]
    NPscript[getNext(NPscript)] <- c("stty -echo","echo off","stty -echo")[OS]
    NPscript[getNext(NPscript)] <- paste("mkdir ",newdir,sep="")
    NPscript[getNext(NPscript)] <- paste("mkdir ",newdir,c("/inputs","\\inputs","/inputs")[OS],sep="")
    NPscript[getNext(NPscript)] <- paste("mkdir ",newdir,c("/outputs","\\outputs","/outputs")[OS],sep="")
    NPscript[getNext(NPscript)] <- paste("mkdir ",newdir,c("/wrkcopy","\\wrkcopy","/wrkcopy")[OS],sep="")
    NPscript[getNext(NPscript)] <- paste("mkdir ",newdir,c("/etc","\\etc","/etc")[OS],sep="")
    
    outlist <- c("DEN*","OUT0*","OUTT*","PRTB*","ILOG*","NP_RF*")
    NPscript[getNext(NPscript)] <- paste("echo",data,">> NP_RF0001.TXT")
    
    NPscript[getNext(NPscript)] <- c("if [ ! -f NP_RF0001.TXT ]; then error=true; else error=false; fi","if not exist NP_RF0001.TXT (set error=1) ELSE (set error=0)","if [ ! -f NP_RF0001.TXT ]; then error=true; else error=false; fi")[OS]
    for (i in 1:6){
      NPscript[getNext(NPscript)] <- paste(c(paste("if [ -f ",outlist[i]," ]; then mv ",sep=""),paste("if exist ",outlist[i]," move ",sep=""),paste("if [ -f ",outlist[i]," ]; then mv ",sep=""))[OS],outlist[i]," ",newdir,c("/outputs; fi","\\outputs","/outputs; fi")[OS],sep="")
    }
    NPscript[getNext(NPscript)] <- c(paste("if [ -f ERROR0001 ]; then mv ERROR0001 ",newdir,"/outputs; fi",sep=""),
                                     paste("if exist ERROR0001 move ERROR0001 ",newdir,"\\outputs",sep=""),
                                     paste("if [ -f ERROR0001 ]; then mv ERROR0001 ",newdir,"/outputs; fi",sep=""))[OS] 
    
    if (instr!=-99){
      NPscript[getNext(NPscript)] <- paste(c("mv ","move ","mv ")[OS],instr," ",newdir,c("/etc","\\etc","/etc")[OS],sep="")
      NPscript[getNext(NPscript)] <- paste(c("mv ","move ","mv ")[OS],"nplog.txt ",newdir,c("/outputs","\\outputs","/outputs")[OS],sep="")
      NPscript[getNext(NPscript)] <- paste(c("mv ","move ","mv ")[OS],"NPcontrol ",newdir,c("/etc","\\etc","/etc")[OS],sep="")
      
    }
    if(!useOldFortran){  #we are using the new model template
      NPscript[getNext(NPscript)] <- paste(c("mv ","move ","mv ")[OS],modelFor," ",newdir,c("/etc/","\\etc\\","/etc/")[OS],modelFor,sep="")  #move fortran file to etc
      NPscript[getNext(NPscript)] <- paste(c("mv ","move ","mv ")[OS],model," ",newdir,c("/inputs/","\\inputs\\","/inputs/")[OS],model,sep="")  #move template file to inputs
    } else {NPscript[getNext(NPscript)] <- paste(c("mv ","move ","mv ")[OS],model," ",newdir,c("/inputs","\\inputs","/inputs")[OS],sep="")}  #using fortran file directly, so move to inputs
    
    NPscript[getNext(NPscript)] <- paste(c("mv ","move ","mv ")[OS],"XQZPJ*.ZMQ ",newdir,c("/wrkcopy","\\wrkcopy","/wrkcopy")[OS],sep="")
    NPscript[getNext(NPscript)] <- paste(c("mv ","move ","mv ")[OS],"extnum ",newdir,c("/etc","\\etc","/etc")[OS],sep="")
    
    NPscript[getNext(NPscript)] <- paste(c("mv ","move ","mv ")[OS],"npag*.* ",newdir,c("/etc","\\etc","/etc")[OS],sep="")
    NPscript[getNext(NPscript)] <- paste(c("rm ","erase ","rm ")[OS],"CHMAX*.*",sep="")
    NPscript[getNext(NPscript)] <- c(paste("if [ -f FROM0001 ]; then mv FROM0001 ",newdir,"/inputs; fi",sep=""),
                                     paste("if exist FROM0001 move FROM0001 ",newdir,"\\inputs",sep=""),
                                     paste("if [ -f FROM0001 ]; then mv FROM0001 ",newdir,"/inputs; fi",sep=""))[OS] 
    NPscript[getNext(NPscript)] <- paste(c("rm ","erase ","rm ")[OS],"fort.*",sep="")
    NPscript[getNext(NPscript)] <- paste(c("rm ","erase ","rm ")[OS],"go",sep="")
    NPscript[getNext(NPscript)] <- paste(c("mv ","move ","mv ")[OS],"np_prep* ",newdir,c("/etc","\\etc","/etc")[OS],sep="")
    NPscript[getNext(NPscript)] <- paste(c("mv ","move ","mv ")[OS],"np_run* ",newdir,c("/etc","\\etc","/etc")[OS],sep="")
    NPscript[getNext(NPscript)] <- paste(c("mv ","move ","mv ")[OS],data," ",newdir,c("/inputs","\\inputs","/inputs")[OS],sep="")
    if(prior[1]==0) NPscript[getNext(NPscript)] <- paste(c("mv ","move ","mv ")[OS],prior[2]," ",newdir,c("/inputs","\\inputs","/inputs")[OS],sep="")
    
    #make report
    reportscript <- paste(normalizePath(Sys.getenv("PmetricsPath"),winslash="/"),"/Pmetrics/report/NPrepScript.R",sep="")
    outpath <- c(paste(workdir,"/",newdir,"/outputs",sep=""),
                 paste(workdir,"\\",newdir,"\\outputs",sep=""),
                 paste(workdir,"/",newdir,"/outputs",sep=""))[OS]
    
    NPscript[getNext(NPscript)] <- c("if ! $error ; then ", "if %error% == 0 (","if ! $error ; then ")[OS]
    #these are going to have to be conditional now if not boostrapping
    NPscript[getNext(NPscript)] <- c(paste(normalizePath(R.home(),winslash="/"),"/bin/Rscript ",shQuote(reportscript)," ",shQuote(outpath)," ",icen,sep=""),
                                     paste(shQuote(paste(gsub("/",rep,normalizePath(R.home(),winslash="/")),"\\bin\\Rscript",sep=""))," ",shQuote(reportscript)," ",shQuote(outpath)," ",icen,sep=""),
                                     paste(normalizePath(R.home(),winslash="/"),"/bin/Rscript ",shQuote(reportscript)," ",shQuote(outpath)," ",icen,sep=""))[OS]
    NPscript[getNext(NPscript)] <- c(paste("open ",shQuote(paste(gsub("/",rep,outpath),"/NPAGreport.html",sep=""))," ; fi",sep=""),
                                     paste("start ",shQuote("NPAG Report")," ",shQuote(paste(gsub("/",rep,outpath),"\\NPAGreport.html",sep="")),")",sep=""),
                                     paste("xdg-open ",shQuote(paste(gsub("/",rep,outpath),"/NPAGreport.html",sep=""))," ; fi",sep=""))[OS]
    
    #final clean up
    if(OS==1 | OS==3){ #for Mac or Linux
      NPscript[getNext(NPscript)] <- paste("mv npscript ",newdir,"/etc",sep="")
      
    } else {  #for Windows
      #NPscript[getNext(NPscript)] <- paste("for %%i in (*.*) do if not %%i == npscript.bat move %%i ",newdir,"\\inputs",sep="")
      NPscript[getNext(NPscript)] <- paste("copy npscript.bat ",newdir,"\\etc",sep="")
      NPscript[getNext(NPscript)] <- paste("echo. & echo Press any key to complete run and close this window... & echo.")
      NPscript[getNext(NPscript)] <- paste("pause > nul")
      NPscript[getNext(NPscript)] <- paste("erase npscript.bat ",sep="")
    }
    
    NPscript <- NPscript[!is.na(NPscript)]
    
    
    f <- file(c("npscript","npscript.bat","npscript")[OS],"w")
    writeLines(NPscript,f)
    close(f)
    
    if (OS==1){ #Mac
      system("chmod +x npscript")
      if(!(remote)) {system(paste("open -a Terminal.app ",shQuote(paste(getwd(),"/npscript",sep="")),sep=""))
      } else { cat("Upload all files in your working directory to the server and execute npscript.\n")}
    } 
    if (OS==2){ #Windows
      if(!(remote)) {cat(paste("Launch npscript.bat in your working directory to execute the NPAG run.\n",sep=""))
      } else {cat("Upload all files in your working directory to the server and execute npscript.bat.\n")}
      outpath <- gsub("\\\\","/",outpath)
    }
    if (OS==3){ #Linux
      system("chmod +x npscript")
      if(!remote) {system(paste(shQuote(paste(getwd(),"/npscript",sep="")),sep=""))
      } else { cat("Upload all files in your working directory to the server and execute npscript.\n")}
    } 
    
    return(outpath)
    
    
  } else { #run NPAG internally
    workdir <- getwd()
    if (!auto){  #allow users to see questions
      if(OS==1 | OS==3){system("./np_prep MacOSX")} 
    } else { #we do have instructions for prep
      if (inherits(prior,"NPAG")){ 
        nvar <- trans$nvar
        prior$ab <- as.matrix(trans$ab.df)
        prior$popPoints <- makeFinal(prior)$popPoints
        for(i in 1:nvar){
          if(prior$ab[i,1] > min(prior$popPoints[,i])) endNicely(paste("You have changed ",prior$par[i]," so that the minimum range of ",prior$ab[i,1]," is greater than the minimum prior point value of ",min(prior$popPoints[,i]),"\nThis will cause NPAG to crash.\n",sep=""),model,data)
          if(prior$ab[i,2] < max(prior$popPoints[,i])) endNicely(paste("You have changed ",prior$par[i]," so that the maximum range of ",prior$ab[i,2],"is less than the maximum prior point value of ",max(prior$popPoints[,i]),"\nThis will cause NPAG to crash.\n",sep=""),model,data)
        }
        err <- makeDen(prior,F)
        if(err==-1) stop("\nYour NPdata prior object is older and does not contain the number of dimensions in your model.\nRe-run your NPAG analysis with Pmetrics 0.25 or later before bootstrapping.\n")
        prior <- c(0,"prior.txt")
      } else {
        if(prior != -99){
          prior <- c(0,prior) } else {prior <- 1}
      }
      
      NPcontrol <- c("1",   #we have an instruction file
                     "instr.inx")   #name of the instruction file
      
      f <- file("NPcontrol","w")
      writeLines(NPcontrol,f)
      close(f)
      
      if(OS==1 | OS==3){system("./np_prep MacOSX < NPcontrol")} 
      if(OS==2) {shell("np_prep DOS < NPcontrol")}
      
    }  
    
    # RUN NPAG engine
    if(OS==1 | OS==3){
      system("echo 1 > extnum")
      system("echo go > go")
      system(paste(enginecompile,"npagdriv.f",sep=" "))
      system("./np_run < go")
    } else {
      shell("echo 1 > extnum")
      shell("echo go > go")
      shell(paste(enginecompile,"npagdriv.f",sep=" "))
      shell("np_run < go")
    }
    
    #CLEAN UP
    dir.create(newdir)
    dir.create(paste(newdir,"/inputs",sep=""))
    dir.create(paste(newdir,"/outputs",sep=""))
    dir.create(paste(newdir,"/wrkcopy",sep=""))
    dir.create(paste(newdir,"/etc",sep=""))
    
    outlist <- c("DEN*","OUT0*","OUTT*","PRTB*","ILOG*","NP_RF*")
    if(length(Sys.glob("NP_RF*"))>0){
      npfile <- file(Sys.glob("NP_RF*"),open="a")
      writeLines(data,npfile)
      close(npfile)
      file.copy(from=Sys.glob(outlist),to=paste(newdir,"/outputs",sep=""))    
      file.remove(Sys.glob(outlist))
    }
    if(auto){
      file.copy(from=instr,to=paste(newdir,"/etc",sep=""))
      file.copy(from="nplog.txt",to=paste(newdir,"/outputs",sep=""))
      file.copy(from="NPcontrol",to=paste(newdir,"/etc",sep="")) 
      file.copy(from=data,to=paste(newdir,"/inputs",sep=""))
      file.remove(instr)
      file.remove("nplog.txt")
      file.remove("NPcontrol")
      file.remove(data)
    }
    if(prior[1]==0){
      file.copy(from=prior[2],to=paste(newdir,"/inputs",sep=""))
      file.remove(prior[2])
    }
    if(!useOldFortran){  #we are using the new model template
      file.copy(from=modelFor,to=paste(newdir,"/etc/",modelFor,sep=""))  #move fortran file to etc
      file.copy(from=model,to=paste(newdir,"/inputs/",model,sep="")) #move template file to inputs
    } else {
      file.copy(from=model,to=paste(newdir,"/inputs",sep="")) #using fortran file directly, so move to inputs
      file.remove(model)   
    }  
    
    file.remove(Sys.glob("CHMAX*.*"))
    file.remove(Sys.glob("fort.*"))
    file.remove("SAVEINST.TMP")
    file.remove("go")
    
    if(length(Sys.glob("FROM*"))>0) {
      file.copy(from=Sys.glob("FROM*"),to=paste(newdir,"/inputs",sep=""))
      file.remove(Sys.glob("FROM*"))
    }
    if(length(Sys.glob("ERROR*"))>0) {
      file.copy(from=Sys.glob("ERROR*"),to=paste(newdir,"/outputs",sep=""))
      file.remove(Sys.glob("ERROR*"))
    }
    file.copy(from=Sys.glob("XQZPJ*.ZMQ"),to=paste(newdir,"/wrkcopy",sep=""))
    file.copy(from="extnum",to=paste(newdir,"/etc",sep=""))
    file.copy(from=Sys.glob("npag*"),to=paste(newdir,"/etc",sep=""))
    file.copy(from=Sys.glob("np_prep*"),to=paste(newdir,"/etc",sep=""))
    file.copy(from=Sys.glob("np_run*"),to=paste(newdir,"/etc",sep=""))
    file.remove(Sys.glob("XQZPJ*.ZMQ"))
    file.remove("extnum")
    file.remove(Sys.glob("npag*"))
    file.remove(Sys.glob("np_prep*"))
    file.remove(Sys.glob("np_run*"))
    
    #make report
    NPreport(paste(workdir,newdir,"outputs",sep="/"),icen=icen)
    
    #final clean up
    setwd(workdir)
    file.copy(from=Sys.glob("*.*"),to=paste(newdir,"/inputs",sep=""))
    file.remove(Sys.glob("*.*"))
    outpath <- paste(workdir,newdir,"outputs",sep="/")
    
    
    return(outpath)    
    
  }
  
}

