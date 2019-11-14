
.PMrun <- function(type,model,data,run,
                   include,exclude,ode,tol,salt,cycles,
                   indpts,icen,aucint,
                   idelta,prior,xdev,search,
                   auto,intern,silent,overwrite,nocheck,parallel,batch,
                   remoterun, remoteuser, remoteproject, remotecontact){

# TODO
# wmy :: beta should be passed in as new argument w/default value F
# for now, beta code is only working version; to get old code to work have to
# edit, PMrun(), makeModel(), PMbuild(), makePmetrics(), and maybe more!
    beta = T

# wmy added the new parameters:
# remoterun, remoteuser, remoteproject, and remotecontact
  
  currwd <- gsub(" ","\\ ",getwd()) #set the current working directory to go back to it at the end
  
  #make new output directory
  if(is.null(run)){
    olddir <- list.dirs(recursive=F)
    olddir <- olddir[grep("^\\./[[:digit:]]+",olddir)]
    olddir <- sub("^\\./","",olddir)
# wmy wants to run the following line b/c he likes to rename his directories
#  things like 253.badrun or 172.mostexcellent the trailing chars
#  cause the newline assignment to be NA, which then breaks the code 
#    olddir <- gsub("[^0-9]", "", olddir) #  remove all non-numeric chars (including the '.')
    if(length(olddir)>0){
      newdir <- as.character(max(as.numeric(olddir))+1)
    } else {newdir <- "1"}
  } else {
    if(!is.numeric(run)) {endNicely("'run' must be numeric.\n")} else {newdir <- as.character(run)}
  }
  
  if(file.exists(newdir)){
    if(overwrite) {unlink(newdir,recursive=T)} else {endNicely(paste("\n",newdir," exists already.  Set overwrite=T to overwrite.\n"))}
  }
  dir.create(newdir)
  
  setwd(currwd)
  setwd(newdir) #move to the new directory and do each run there (compatible with batching)
  
  #check for files
  #copy model file if available
  if(is.numeric(model)){
    modelfile <- suppressWarnings(tryCatch(scan(paste("../",model,"etc/instr.inx",sep="/"),what="character",quiet=T,skip=4,n=1),error=function(e) NULL))
    modelfile <- Sys.glob(paste("../",model,"/inputs/",strsplit(modelfile,"\\.")[[1]][1],"*",sep=""))
    if(length(modelfile)>0){
      if(length(modelfile)>1){
        for(thisfile in modelfile){
          firstline <- scan(thisfile, nline=1, what="character",quiet=T)
          if(firstline[1]!="POPDATA"){
          modelfile <- thisfile
          break
          }
        }
      }
      file.copy(from=modelfile,to=getwd())
      model <- basename(modelfile)
    }
  } else { #model name was a text file
    #make sure model file name is <=8 characters
    if(!FileNameOK(model)) {endNicely(paste("Model file name must be 8 characters or fewer.\n"),model=-99,data)}
    
    while(!file.exists(paste("../",model,sep=""))) {
      model <- readline(paste("The model file",shQuote(paste(getwd(),model)),"does not exist.\nEnter another filename or 'end' to quit: \n")) 
      if (tolower(model)=="end") {endNicely(paste("No model file specified.\n"),model=-99,data); break}
    }
    file.copy(from=paste("../",model,sep=""),to=getwd()) #copy found file to new folder
    file.remove(paste("../",model,sep="")) #erase the found file in parent folder
    
  }

# wmy.2017.04.12 -- see emails w/Femke, MN, MvG re: error equation not read correctly; MN found
#        underlying issue is, if data=integer, wrk files are used, the error in the wrk-ing
#        files is not overwritten. So, if the model file has a different error function, the
#        function in the wrk-ing files will be used (not the new error function in the model file).
#
# Potential solution: Compare the new model file ("model") w/the model in the directory
#   ../data/inputs/model.txt; looking for a difference in the error block.
#   If the respective error blocks match, continue w/copying the wrk-ing files
#   to the new /run_no/wrk/, if not, then copy the /data/inputs/*.csv
#   to /Runs/, and reset data="data.csv", then generate new wrk copy.
#
# Rejected approach : Code to parse the model file in in PMutils.R ... cutting and pasting the part that
# reads the error block and running twice here, once on each of the two models doesn't seem easy.
#
# Rejected Approach : In unix, this is a relatively easy command line program ... but then we have to deal w/
# Windows compatibility issues.
#
# I wrote compareTwoModelERRs <- function(mod1,mod2) that returns T/F if mod1 has
# the same or different #ERR block as mod2.  See PMutils.R`
#
# BUG FIX wmy.2017.04.12 The Femke error polynomial quandry.
#
  if(is.numeric(data))
  {
# find oldmod name in ../data/etc/*.inx
    if (! dir.exists(paste("../",data,"/etc/",sep=""))) 
    {
      endNicely(paste("\nNo directory: ",data,"/etc/\n",sep=""),model,data=-99)
    }
    oldmod <- getNPinstr(data,"FORTRAN MODEL FILE") # assumes ../data/etc/instr.inx exists
    oldmod <- paste("../",data,"/inputs/",gsub("\\..*","", oldmod ),".txt",sep="")
# compare old and new model error block
#
#The following objects are masked _by_ ‘.GlobalEnv’:
#
#    compareTwoModelERRs, getNPinstr
#
    if (! compareTwoModelERRs(model,oldmod)) # Copy old data.csv to here and rename data 
    { # The desired error function; and old error function do not match
      data.check1 <- getNPinstr(data,"BLOCKPAT")
      data.check2 <- list.files(path=paste("../",data,"/inputs/",sep=""),pattern="csv")
      if (data.check1 == data.check2) # only 1 csv file in /data/inputs/ and matches instr.inx
      { # copy datafile and convert data from integer to string
        file.copy(from=paste("../",data,"/inputs/",data.check2,sep=""),to=getwd())
        data <- list.files(path=paste("../",data,"/inputs/",sep=""),pattern="csv")
      }
      # else {error reporting goes here}
    }
#   else { data is from a run w/the same requested error poynomial as the current run; so do nothing }
  }
# End BUG FIX Femke error polynomial quandry wmy.2017.04.12

  #copy wrk files if available
  wrkFlag <- F
  if(is.numeric(data)){
    wrkfiles <- Sys.glob(paste("..",data,"wrkcopy/*.ZMQ",sep="/"))
    if(length(wrkfiles)>0){
      file.copy(from=wrkfiles,to=getwd())
      wrkFlag <- T
    } else {endNicely(paste("\nNo working copy files found in ",data,"/wrkcopy folder.\n",sep=""),model,data=-99)}
    RFfile <- suppressWarnings(tryCatch(readLines(Sys.glob(paste("..",data,"outputs/??_RF0001.TXT",sep="/"))),error=function(e) NULL))
    if(length(RFfile)>0){
      datafileName <- tail(RFfile,1)
      file.copy(from=paste("..",data,"inputs",datafileName,sep="/"),to=getwd())
      data <- datafileName
    } else {endNicely(paste("\nNo RF file found in ",data,"/outputs folder to extract matrix data filename.\n",sep=""),model,data=-99)}
  } else { #data name was a file
    #make sure data file name is <=8 characters
    if(!FileNameOK(data)) {endNicely(paste("Data file name must be 8 characters or fewer.\n"),model,data=-99)}
    
    #ok look for a data file
    while(!file.exists(paste("..",data,sep="/"))) {
      data <- readline(paste("The data file",shQuote(paste(getwd(),data)),"does not exist.\nEnter another filename or 'end' to quit: \n")) 
      if (tolower(data)=="end") {endNicely(paste("No data file specified.\n"),model,data=-99); break}
    }
    file.copy(from=paste("../",data,sep=""),to=getwd()) #copy found file to new folder
    file.remove(paste("../",data,sep="")) #erase the found file in parent folder
  }
  
  
  
  #get information from datafile
  dataFile <- PMreadMatrix(data,quiet=T)
  
  #check for errors in data if nocheck=F
  if(!nocheck){
    err <- PMcheck(dataFile,quiet=T)
    if(attr(err,"error")==-1){
      endNicely("\nThere are errors in your data file.  See errors.xlsx file in working directory.\n",model,data)
    }
  }
  

  ncov <- getCov(dataFile)$ncov
  covnames <- getCov(dataFile)$covnames
  numeqt <- max(dataFile$outeq,na.rm=T)
  id <- unique(dataFile$id)
  nsubtot <- length(id)
  
  if(is.null(include)) include <- id
  if(!is.null(exclude)) include <- id[!id %in% exclude]
  
  nsub <- length(include)
  activesub <- c(which(id %in% include),0)
  ndrug <- max(dataFile$input,na.rm=T)
  if(is.null(salt)) {
    salt <- rep(1,ndrug)
  } else {
    if(length(salt)!=length(ndrug)) endNicely("\nSalt fraction length must equal number of drugs.\n",model,data)
  }
  
  #AUC interval, index of gridpts, prior, and MIC for NPAG
  if(type=="NPAG"){
    #AUC interval
    if(is.null(aucint)){
      maxTime <- max(dataFile$time,na.rm=T)
      if(maxTime>24*48) {aucint <- ceiling(maxTime/48)} else {aucint <- 24}
    }
    
    #gridpts
    if(is.null(indpts)) indpts <- -99
    
    #check if prior and if so, get name for instruction file
    if(is.null(prior)) {  #prior not specified
      prior <- -99
      priorString <- 1
    } else { #prior specified, so choose how
      if(inherits(prior,"NPAG")){priorString <- c(0,"prior.txt")} #prior is an NPdata object
      if(is.character(prior)) { #prior is the name of a file
        priorString <- c(0,prior)
# wmy :: bug fix. When prior = file, the file is not found.
        priorDEN <- Sys.glob(paste("..",prior,sep="/"))[1]
        file.copy(from=priorDEN,to=getwd())
# wmy end bug fix
      }
      if(is.numeric(prior)){  #prior is a run number
        priorDEN <- Sys.glob(paste("..",prior,"outputs/DEN*",sep="/"))[1]
        if(length(priorDEN)>0){
          file.copy(from=priorDEN,to=paste(getwd(),"/prior.txt",sep=""))
          prior <- "prior.txt"
          priorString <- c(0,prior)
          
        }
      }
    }
    #MIC
    xmic <- 1
  }
  
  #set fraction of ab range for initial parameter SD for IT2B and ERR
  if(type=="IT2B" | type=="ERR"){
    xsig=0.5
  }
  
  
  #attempt to translate model and data files into separate fortran model  and instruction files
  
  if(type=="NPAG"){
    engine <- list(alg="NP",nsubtot=nsubtot,nsub=nsub,activesub=activesub,ncov=ncov,covnames=covnames,ndrug=ndrug,tol=tol,
                   salt=salt,numeqt=numeqt,cycles=cycles,icen=icen,indpts=indpts,aucint=aucint,idelta=idelta,xmic=xmic,
                   ode=ode,limits=NA,priorString=priorString,wrkFlag=wrkFlag)
  }
  if(type=="IT2B"){
    engine <- list(alg="IT",nsubtot=nsubtot,nsub=nsub,activesub=activesub,ncov=ncov,covnames=covnames,ndrug=ndrug,
                   salt=salt,numeqt=numeqt,cycles=cycles,xsig=xsig,tol=tol,xdev=xdev,indpts=-99,
                   ode=ode,limits=NA,wrkFlag=wrkFlag)
    
  }
  if(type=="ERR"){
    engine <- list(alg="ERR",nsubtot=nsubtot,nsub=nsub,activesub=activesub,ncov=ncov,covnames=covnames,ndrug=ndrug,
                   salt=salt,numeqt=numeqt,cycles=cycles,xsig=xsig,tol=tol,xdev=xdev,indpts=-99,
                   ode=ode,limits=NA,wrkFlag=wrkFlag)
  }
  
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
  
  #if parallel not specified, choose serial for algebraic/exact or parallel for ODE
  if(is.na(parallel)){
    parallel <- c(T,F)[1+as.numeric(trans$N<=0)]
  }
  
  #if parallel is true and in 32-bit, choose serial and warn
  if(parallel & getBits()=="32"){
    parallel <- F
    cat("\nNote: Parallel processing is not available for 32-bit systems.\n")
  }
  
  OS <- getOS()
  
  fortSource <- switch(OS,"~/.config/Pmetrics/compiledFortran",
                       paste(Sys.getenv("APPDATA"),"\\Pmetrics\\compiledFortran",sep=""),
                       "~/.config/Pmetrics/compiledFortran")
  if(!file.exists(fortSource)){
    PMbuild()
  }
  compiler <- PMFortranConfig()
  #check if gfortran and choose serial if not
  if(length(compiler)==1){
    parallel <- F
  }
  if(is.null(compiler)) endNicely(paste("\nExecute ",type," run after a fortran compiler is installed.\n",sep=""),
                                  model,data)
  
  #substitution string for directory separator according to OS  
  rep <- c("/","\\\\","/")[OS]
  
  #change units of ODE tolerance to linear from log
  ode <- c(0,10**ode)
  
  #generate the names of the permanent modules
  if(parallel){prefix <- "p"} else {prefix <- "s"} #add the correct switch for NPAG engine, always serial for IT2B and ERR
  prepfiles <- shQuote(normalizePath(list.files(fortSource,
                                                pattern=switch(type,NPAG="sNPprep",IT2B="sITprep",ERR="sITprep"),full.names=T)))
enginefiles <- shQuote(normalizePath(list.files(fortSource,
                         pattern=switch(type,NPAG=paste(prefix,"NPeng",sep=""),IT2B="sITeng",ERR="sITerr"),full.names=T)))

# wmy2017Oct13 -- create the ODE "engine"
  ODEsolver <- shQuote(normalizePath(list.files(fortSource,
                         pattern=paste(prefix,"ODEsolver", sep=""),full.names=T)))
# wmy2017Oct13 -- create NPAG utilities module
  NPAGutils <- shQuote(normalizePath(list.files(fortSource,
                       pattern=paste(prefix,"npag_utils", sep=""),full.names=T)))
  ModDir <- paste("-I",fortSource,"/",sep="")
  
  #generate names of files that will be created
  prepFileName <- switch(type,NPAG="np_prep",IT2B="it_prep",ERR="err_prep")
  runFileName <- switch(type,NPAG="np_run",IT2B="it_run",ERR="err_run")
  drivFileName <- switch(type,NPAG="npagdriv.f",IT2B="it2bdriv.f",ERR="assdriv.f")
  scriptFileName <- switch(type,NPAG="npscript",IT2B="itscript",ERR="errscript")
  #list of output files
  if(type=="NPAG") {outlist <- c("DEN*","OUT0*","OUTT*","PRTB*","ILOG*","NP_RF*")}
  if(type=="IT2B") {outlist <- c("DENF*","FROM*","LAST*","OUFF*","OUTF0*","IT_RF*")}
  if(type=="ERR") {outlist <- "ASS*"}
  
  #generate the compile statements  
  prepcompile <- sub("<exec>",prepFileName,compiler[1])
  # wmy2018Oct25 -- NPAGutils included in prep compile statement
  prepcompile <- sub("<files>",paste(ModDir,NPAGutils,prepfiles,sep=" "),prepcompile,fixed=T)
  enginecompile <- sub("<exec>",runFileName,compiler[1+as.numeric(parallel)])

# wmy2017Oct13 -- ODEsolver ("engine") included in compile statement
# wmy2018Sep18 -- NPAGutils included in compile statement
  if (type=="NPAG") {
    if (beta==T) {
      enginecompile <- sub("<files>",paste(ModDir,NPAGutils,enginefiles,ODEsolver,sep=" "),enginecompile,fixed=T)
    } else {
      enginecompile <- sub("<files>",enginefiles,enginecompile,fixed=T)
    }
  } else {
    enginecompile <- sub("<files>",enginefiles,enginecompile,fixed=T)
  }
  #now substitute the file separator for inclusion in the batch file
  prepfiles <- gsub("/",rep,prepfiles)
  enginefiles <- gsub("/",rep,enginefiles)
  
  #initiation statement  
  if (OS==1 | OS==3){  #Mac and Linux
    system(paste("echo '",type," run initiated at",format(Sys.time(),"%H:%M on %Y %b %d."),
                 "\nPREP FILES:",prepfiles,"\nENGINE FILES:",enginefiles,"\n\n' > log.txt"))
    system(prepcompile,ignore.stderr=F)
  } 
  
  if (OS==2){  #Windows
    shell(paste("echo '",type," run initiated at",format(Sys.time(),"%H:%M on %Y %b %d."),
                "\nPREP FILES:",prepfiles,"\nENGINE FILES:",enginefiles,"\n\n'"))
    shell(prepcompile,ignore.stderr=F)        
  }
  
  if(!intern | !auto){ #run as batch file script
    #build the batch file script
    PMscript <- vector("character")
    #format working directory for batch file
    workdir <- gsub("/",rep,getwd())
    #change working directory
    PMscript[getNext(PMscript)] <- paste("cd",shQuote(workdir))
    #start timer
    PMscript[getNext(PMscript)] <- c("echo Unix>time.txt","echo Windows>time.txt","echo Linux>time.txt")[OS]
    PMscript[getNext(PMscript)] <- c("date +%s>>time.txt","echo %time%>>time.txt","date +%s>>time.txt")[OS]
    
    if (!auto){
      #manual run of prep program
      PMscript[getNext(PMscript)] <- c(paste("./",prepFileName," MacOSX",sep=""),
                                       paste(prepFileName," DOS",sep=""),
                                       paste("./",prepFileName," MacOSX",sep=""))[OS]
      
    } else {
      if(type=="NPAG"){
        #handle the prior for NPAG runs
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
      } #end prior handling block for NPAG runs
      
      #make the control file to execute run with instructions
      ControlFile <- c("1",   #using instruction file
                       "instr.inx", #name of instruction file 
                       "1") #extra in case of error    
      f <- file("PMcontrol","w")
      writeLines(ControlFile,f)
      close(f)
      
      #run prep program
      PMscript[getNext(PMscript)] <- c(paste("./",prepFileName," MacOSX < PMcontrol",sep=""),
                                       paste(prepFileName," DOS < PMcontrol",sep=""),
                                       paste("./",prepFileName," MacOSX < PMcontrol",sep=""))[OS]
    }  
    PMscript[getNext(PMscript)] <- "echo 1 > extnum"
    PMscript[getNext(PMscript)] <- "echo go > go"
    PMscript[getNext(PMscript)] <- paste(enginecompile,drivFileName,sep=" ")
    PMscript[getNext(PMscript)] <- c(paste("./",runFileName," < go",sep=""),
                                     paste(runFileName," <go",sep=""),
                                     paste("./",runFileName," < go",sep=""))[OS]
    PMscript[getNext(PMscript)] <- c("echo;echo Cleaning up....;echo","echo. & echo Cleaning up.... & echo.","echo;echo Cleaning up....;echo")[OS]
    PMscript[getNext(PMscript)] <- c("stty -echo","echo off","stty -echo")[OS]
#    PMscript[getNext(PMscript)] <- paste("mkdir ",newdir,sep="")
    PMscript[getNext(PMscript)] <- "mkdir inputs"
    PMscript[getNext(PMscript)] <- "mkdir outputs"
    PMscript[getNext(PMscript)] <- "mkdir wrkcopy"
    PMscript[getNext(PMscript)] <- "mkdir etc"
    
    
    
    #add the name of the data file to the end of the output for NPAG or IT2B
    if(type=="NPAG" | type=="IT2B"){
      PMscript[getNext(PMscript)] <- paste("echo ",data,
                                           " >> ",substr(type,1,2),"_RF0001.TXT",sep="")   
    }
    
    #check to make sure run completed
    if(type=="NPAG" | type=="IT2B"){
      PMscript[getNext(PMscript)] <- c(paste("if [ ! -f ",substr(type,1,2),"_RF0001.TXT ]; then error=true; else error=false; fi",sep=""),
                                       paste("if not exist ",substr(type,1,2),"_RF0001.TXT (set error=1) ELSE (set error=0)",sep=""),
                                       paste("if [ ! -f ",substr(type,1,2),"_RF0001.TXT ]; then error=true; else error=false; fi",sep=""))[OS]
    }
    if(type=="ERR"){
      PMscript[getNext(PMscript)] <- c("if [ ! -f ASS0001 ]; then error=true; else error=false; fi",
                                       "if not exist ASS0001 (set error=1) ELSE (set error=0)",
                                       "if [ ! -f ASS0001 ]; then error=true; else error=false; fi")[OS]
    }
    
    #move output files    
    for (i in 1:6){
      PMscript[getNext(PMscript)] <- paste(c(paste("if [ -f ",outlist[i]," ]; then mv ",sep=""),
                                             paste("if exist ",outlist[i]," move ",sep=""),
                                             paste("if [ -f ",outlist[i]," ]; then mv ",sep=""))[OS],
                                           outlist[i]," ",c("outputs; fi","outputs","outputs; fi")[OS],sep="")
    }
    #if error file exists
    PMscript[getNext(PMscript)] <- c("if [ -f ERROR* ]; then mv ERROR* outputs; fi",
                                     "if exist ERROR* move ERROR* outputs",
                                    "if [ -f ERROR* ]; then mv ERROR* outputs; fi")[OS] 
    
    if (instr!=-99){
      PMscript[getNext(PMscript)] <- paste(c("mv ","move ","mv ")[OS],instr," etc",sep="")
      if(OS!=2) {PMscript[getNext(PMscript)] <- paste(c("mv ","","mv ")[OS],"log.txt outputs",sep="")}
      PMscript[getNext(PMscript)] <- paste(c("mv ","move ","mv ")[OS],"PMcontrol etc",sep="")
      
    }
    if(!useOldFortran){  #we are using the new model template
      PMscript[getNext(PMscript)] <- paste(c("mv ","move ","mv ")[OS],modelFor," ",c("etc/","etc\\","etc/")[OS],modelFor,sep="")  #move fortran file to etc
      PMscript[getNext(PMscript)] <- paste(c("mv ","move ","mv ")[OS],model," ",c("inputs/","inputs\\","inputs/")[OS],model,sep="")  #move template file to inputs
    } else {PMscript[getNext(PMscript)] <- paste(c("mv ","move ","mv ")[OS],model," ",c("inputs","inputs","inputs")[OS],sep="")}  #using fortran file directly, so move to inputs
    
    PMscript[getNext(PMscript)] <- paste(c("mv ","move ","mv ")[OS],"XQZPJ*.ZMQ wrkcopy",sep="")
    PMscript[getNext(PMscript)] <- paste(c("mv ","move ","mv ")[OS],"extnum etc",sep="")
    if(type=="NPAG"){
      PMscript[getNext(PMscript)] <- paste(c("mv ","move ","mv ")[OS],"npag*.* etc",sep="")
      PMscript[getNext(PMscript)] <- paste(c("rm ","erase ","rm ")[OS],"CHMAX*.*",sep="")
      PMscript[getNext(PMscript)] <- c(paste("if [ -f FROM0001 ]; then mv FROM0001 ","inputs; fi",sep=""),
                                       paste("if exist FROM0001 move FROM0001 ","inputs",sep=""),
                                       paste("if [ -f FROM0001 ]; then mv FROM0001 ","inputs; fi",sep=""))[OS] 
    }
    if(type=="IT2B" | type=="ERR"){
      PMscript[getNext(PMscript)] <- paste(c("mv ","move ","mv ")[OS],"it2b*.* etc",sep="")
      PMscript[getNext(PMscript)] <- paste(c("mv ","move ","mv ")[OS],"itas*.* etc",sep="")
    }
    if(type=="ERR"){
      PMscript[getNext(PMscript)] <- paste(c("mv ","move ","mv ")[OS],"assdriv*.* etc",sep="")
    }
    PMscript[getNext(PMscript)] <- paste(c("rm ","erase ","rm ")[OS],"fort.*",sep="")
    PMscript[getNext(PMscript)] <- paste(c("rm ","erase ","rm ")[OS],"go",sep="")
    PMscript[getNext(PMscript)] <- paste(c("mv ","move ","mv ")[OS],prepFileName,"* etc",sep="")
    PMscript[getNext(PMscript)] <- paste(c("mv ","move ","mv ")[OS],runFileName,"* etc",sep="")
    PMscript[getNext(PMscript)] <- paste(c("mv ","move ","mv ")[OS],data," inputs",sep="")
    if(type=="NPAG" && prior[1]==0) PMscript[getNext(PMscript)] <- paste(c("mv ","move ","mv ")[OS],basename(prior[2])," inputs",sep="")
    
    #make report
    reportscript <- paste(normalizePath(getPMpath(),winslash="/"),"/Pmetrics/report/",
                          switch(type,NPAG="NP",IT2B="IT",ERR="ERR"),"repScript.R",sep="")
    outpath <- c(paste(workdir,"/outputs",sep=""),
                 paste(workdir,"\\outputs",sep=""),
                 paste(workdir,"/outputs",sep=""))[OS]
    
    #end timer and move file to outputs
    PMscript[getNext(PMscript)] <- c("date +%s >> time.txt","echo %time% >> time.txt","date +%s >> time.txt")[OS]
    PMscript[getNext(PMscript)] <- paste(c("mv ","move ","mv ")[OS],"time.txt outputs",sep="")
    
    
    #close the error loop    
    PMscript[getNext(PMscript)] <- c("if ! $error ; then ", "if %error% == 0 (","if ! $error ; then ")[OS]   
    
    
    #call report script and then open HTML file
    PMscript[getNext(PMscript)] <- c(paste(normalizePath(R.home("bin"),winslash="/"),"/Rscript ",shQuote(reportscript)," ",shQuote(outpath)," ",icen," ",parallel,sep=""),
                                     paste(shQuote(paste(gsub("/",rep,normalizePath(R.home("bin"),winslash="/")),"\\Rscript",sep=""))," ",shQuote(reportscript)," ",shQuote(outpath)," ",icen," ",parallel,sep=""),
                                     paste(normalizePath(R.home("bin"),winslash="/"),"/Rscript ",shQuote(reportscript)," ",shQuote(outpath)," ",icen," ",parallel,sep=""))[OS]
    PMscript[getNext(PMscript)] <- c(paste("open ",shQuote(paste(gsub("/",rep,outpath),"/",type,"report.html",sep=""))," ; fi",sep=""),
                                     paste("start ",shQuote(paste(type,"Report"))," ",shQuote(paste(gsub("/",rep,outpath),"\\",type,"report.html",sep="")),")",sep=""),
                                     paste("xdg-open ",shQuote(paste(gsub("/",rep,outpath),"/",type,"report.html",sep=""))," ; fi",sep=""))[OS]
    
    #final clean up
    if(OS==1 | OS==3){ #for Mac or Linux
      PMscript[getNext(PMscript)] <- paste("mv ",scriptFileName," etc",sep="")
      
    } else {  #for Windows
      PMscript[getNext(PMscript)] <- paste("copy ",scriptFileName,".bat etc",sep="")
      PMscript[getNext(PMscript)] <- paste("echo. & echo Press any key to complete run and close this window... & echo.")
      PMscript[getNext(PMscript)] <- paste("pause > nul")
      PMscript[getNext(PMscript)] <- paste("erase ",scriptFileName,".bat ",sep="")
    }
    
    PMscript <- PMscript[!is.na(PMscript)]
    
    
    f <- file(c(scriptFileName,paste(scriptFileName,".bat",sep=""),scriptFileName)[OS],"w")
    writeLines(PMscript,f)
    close(f)
    
    if (OS==1){ #Mac
      system(paste("chmod +x ",scriptFileName))
      if(!batch) system(paste("open -a Terminal.app ",shQuote(paste(getwd(),"/",scriptFileName,sep="")),sep=""))
    } 
    if (OS==2 & !batch){ #Windows
      cat(paste("Launch ",scriptFileName,".bat in your working directory to execute the NPAG run.\n",sep=""))
      outpath <- gsub("\\\\","/",outpath)
    }
    if (OS==3){ #Linux
      system(paste("chmod +x ",scriptFileName))
      if(!batch) {
        # SuSE
        # system(paste("openvt ",shQuote(paste(getwd(),"./",scriptFileName,sep="")),sep=""))
        # Ubuntu16.04 uses gnome-terminal not openvt
        system(paste("gnome-terminal -x ",shQuote(paste(getwd(),"/",scriptFileName,sep="")),sep=""))
        # Ubuntu16.04 does not launch the *.html file
        if (file.exists("NPAGreport.html")) {
          system("xdg-open NPAGreport.html")
        }
      }
    } 
    setwd(currwd)
    return(outpath)
    
    
  } else { #run internally, also for servers
    workdir <- getwd()
    if (!auto){  #allow users to see questions
      if(OS==1 | OS==3){system(paste("./",prepFileName," MacOSX",sep=""))} 
    } else { #we do have instructions for prep
      if(type=="NPAG"){  #handle prior for NPAG
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
      } #end prior handling block
      
      ControlFile <- c("1",   #we have an instruction file
                       "instr.inx")   #name of the instruction file
      
      f <- file("PMcontrol","w")
      writeLines(ControlFile,f)
      close(f)
      
      if(OS==1 | OS==3){system(paste("./",prepFileName," MacOSX < PMcontrol",sep=""))} 
      if(OS==2) {shell(paste(prepFileName," DOS < PMcontrol",sep=""))}
      
    }  
    
    # RUN  engine
  
    if(OS==1 | OS==3){
      timeFile <- file("time.txt",open="a")
      writeLines(c("Unix","","Linux")[OS],timeFile)
      writeLines(as.character(proc.time()[3]),timeFile)
      system("echo 1 > extnum")
      system("echo go > go")
      system("echo checking remoterun")
      if (remoterun==T)
      {
         system(paste("echo", "RemoteRun is", remoterun, remoteuser, remoteproject, remotecontact,sep=" "))
      }
      # wmy :: Run on server (server set up w/Pmetrics)
      #
      # Minimal command
      # NPrun(model="model.txt", data="data.csv",
      #   remoterun=T, remoteuser=user@server, remoteproject=name.of.project, remotecontact=email.address)
      if (remoterun==T)
      {
        # Copy remoteNPrun.sh to getwd()
          pmsrc <- system.file("code",package="Pmetrics")
          getshscript <- paste("cp ", pmsrc, "/remoteNPrun.sh ", " .  ", sep = "", collapse = NULL)
          system(getshscript)
          system("sed 's/\r$//g' remoteNPrun.sh > tmp2.txt") # may need sed -i in linux ; this is for OSX
          system("mv tmp2.txt remoteNPrun.sh")

        # Run ./remoteNPrun.sh
          # sample command line:
          # ./remoteNPrun.sh 997 wyamada@lapkb.usc.edu Ashley model17.txt hcv_no2.csv walter.mas.yamada@gmail.com
          # remoterun, remoteuser, remoteproject, remotecontact, are passed into .PMrun()
          remoteruncomm <- paste("./remoteNPrun.sh", newdir, remoteuser, remoteproject, sep = " ", collapse = NULL)
          # remoteruncomm <- paste(newdir, remoteuser, remoteproject, sep = " ", collapse = NULL)
          remoteruncomm <- paste(remoteruncomm, model, data, remotecontact, sep = " ", collapse = NULL)

          system(paste("echo ", currwd, "> remoterunargs.txt", sep= " "))
          system(paste("echo ", newdir, ">> remoterunargs.txt", sep= " "))
          system(paste("echo ", remoteuser, ">> remoterunargs.txt", sep= " "))
          system(paste("echo ", remoteproject, ">> remoterunargs.txt", sep= " "))
          system(paste("echo ", model, ">> remoterunargs.txt", sep= " "))
          system(paste("echo ", data, ">> remoterunargs.txt", sep= " "))
          system(paste("echo ", parallel, ">> remoterunargs.txt", sep= " "))
          system(paste("echo ", overwrite, ">> remoterunargs.txt", sep= " "))
          system(paste("echo ", remotecontact, ">> remoterunargs.txt", sep= " "))

          fileConn<-file(paste(path.expand("~"),"/ClientInfo.txt",sep=""))
          writeLines(c(currwd,newdir,remoteuser,remoteproject,model,data,parallel,overwrite,remotecontact), fileConn)
          close(fileConn)

          if (OS==1 | OS==3){ #Mac #linux
            system(paste("chmod +x "," remoteNPrun.sh"))
            # system(paste("echo ", remoteruncomm, " >> remoterunargs", sep = " "))
            # if(!batch)

            # system(paste("open -a Terminal.app ",shQuote(paste(getwd(),"/","remoteNPrun.sh",sep="")), remoteruncomm,sep=" "))
            # system(paste(paste("open -a Terminal.app ",shQuote("./remoteNPrun.sh"), sep=" "), remoteruncomm, sep=" "))
            # system(paste("open -a Terminal.app ",shQuote("./remoteNPrun.sh"), remoteruncomm, sep=" "))
            # The files(arglist) do not exist

            # system(paste(paste("open -a Terminal.app ",shQuote(./remoteNPrun.sh),sep=" "), remoteruncomm, sep=" "))
            # Error in shQuote(./remoteNPrun.sh) : object '.' not found

            # system(paste("open -a Terminal.app ",shQuote(remoteruncomm), sep=" "))
            # The file /Users/mas/lapk/pmetrics/greco_ashley/Runs/183/remoteNPrun.sh 183 \\
            # wyamada@lapkb.usc.edu Ashley model22.txt hcv_12.csv walter.mas.yamada@gmail.com does not exist.

            # system(paste("open -a Terminal.app ",remoteruncomm, sep=" "))
            # system(paste("open -a Terminal.app ./remoteNPrun.sh ", remoteruncomm, sep=" "))
            # The files /Users/mas/lapk/pmetrics/greco_ashley/Runs/184/184,
            # /Users/mas/lapk/pmetrics/greco_ashley/Runs/184/wyamada@lapkb.usc.edu,
            # /Users/mas/lapk/pmetrics/greco_ashley/Runs/184/Ashley, and
            # /Users/mas/lapk/pmetrics/greco_ashley/Runs/184/walter.mas.yamada@gmail.com do not exist.

# This is stupid!!! Give up on this approach ... instead ... write a file w/remote params and have ./remoteNPrun read that file 
# to get it's arguments. ... MT thinks better to use this approach ... so after searching found osascript 

            # system(paste("open -a Terminal.app", "./remoteNPrun.sh", sep=" "))

              # example:
              # system(paste("osascript -e 'tell application \"Terminal\" to do script \"", "echo remote run here","\"'",sep=" "))
#              dirbeforelaunch<-getwd()
#              system(paste("osascript -e 'tell application \"Terminal\" to do script \" cd", dirbeforelaunch, ";", remoteruncomm,"\"'",sep=" "))

            # The osascript approach also has problems ... so going back to system call w/reading args from file ClientInfo.txt

# wmy2017 ubuntu vs. osx
            operating.system <- getOSname()
            if (operating.system == "osx") {
              system(paste("open -a Terminal.app", "./remoteNPrun.sh", sep=" "))
            }
            if (operating.system == "linux") { # Assume ubuntu, therefore gnome
              system("gnome-terminal -e ./remoteNPrun.sh")
            }
          }

# TODO -- make this work in Windows and Linux, too; start by editing this code fragment lifted from the batch run above 
#          if (OS==2 & !batch){ #Windows
#            cat(paste("Launch ",scriptFileName,".bat in your working directory to execute the NPAG run.\n",sep=""))
#            outpath <- gsub("\\\\","/",outpath)
#          }
#          if (OS==3){ #Linux
#            system(paste("chmod +x ",scriptFileName))
#            if(!batch) system(paste("openvt ",shQuote(paste(getwd(),"./",scriptFileName,sep="")),sep=""))
#          }

        # Return control to Rconsole
          # stop() # stop() generates an error; so use:
          return() # which returns the last calculated value, i.e. "ans"
      } else {
      # wmy (continue w/old code)

        system(paste(enginecompile,drivFileName,sep=" "))
        system(paste("./",runFileName," < go",sep=""))
      }
      # wmy end of remote run edits
    } else {
      shell("echo 1 > extnum")
      shell("echo go > go")
      shell(paste(enginecompile,drivFileName,sep=" "))
      shell(paste(runFileName," < go",sep=""))
    }
    
    #CLEAN UP
    dir.create(newdir)
    dir.create(paste(newdir,"/inputs",sep=""))
    dir.create(paste(newdir,"/outputs",sep=""))
    dir.create(paste(newdir,"/wrkcopy",sep=""))
    dir.create(paste(newdir,"/etc",sep=""))
    
    #write data file name to end of NPAG/IT2B output
    if(type=="NPAG" | type=="IT2B"){
      if(length(Sys.glob("??_RF*"))>0){
        pmfile <- file(Sys.glob("??_RF*"),open="a")
        writeLines(data,pmfile)
        close(pmfile)
      }
    }
    
    #move output files
    file.copy(from=Sys.glob(outlist),to=paste(newdir,"/outputs",sep=""))    
    file.remove(Sys.glob(outlist))
    
    if(auto){
      file.copy(from=instr,to=paste(newdir,"/etc",sep=""))
      file.copy(from="log.txt",to=paste(newdir,"/outputs",sep=""))
      file.copy(from="PMcontrol",to=paste(newdir,"/etc",sep="")) 
      file.copy(from=data,to=paste(newdir,"/inputs",sep=""))
      file.remove(instr)
      file.remove("log.txt")
      file.remove("PMcontrol")
      file.remove(data)
    }
    if(type=="NPAG" && prior[1]==0){
      file.copy(from=prior[2],to=paste(newdir,"/inputs",sep=""))
      file.remove(prior[2])
    }
    
    if(!useOldFortran){  #we are using the new model template
      file.copy(from=modelFor,to=paste(newdir,"/etc/",modelFor,sep=""))  #move fortran file to etc
      file.copy(from=model,to=paste(newdir,"/inputs/",model,sep="")) #move template file to inputs
      file.remove(modelFor)         
    } else {
      file.copy(from=model,to=paste(newdir,"/inputs",sep="")) #using fortran file directly, so move to inputs
      file.remove(model)   
    }  
    
    if(length(Sys.glob("CHMAX*.*"))>0){
      file.remove(Sys.glob("CHMAX*.*"))
    }
    
    if(length(Sys.glob("FROM*"))>0) {
      file.copy(from=Sys.glob("FROM*"),to=paste(newdir,"/inputs",sep=""))
      file.remove(Sys.glob("FROM*"))
    }
    if(length(Sys.glob("ERROR*"))>0) {
      file.copy(from=Sys.glob("ERROR*"),to=paste(newdir,"/outputs",sep=""))
      file.remove(Sys.glob("ERROR*"))
    }
    
    file.remove(Sys.glob("fort.*"))
    file.remove("go")
    
    if(type=="NPAG"){
      file.copy(from=Sys.glob("npag*"),to=paste(newdir,"/etc",sep=""))
      file.remove(Sys.glob("npag*"))
    }
    
    if(type=="IT2B" | type=="ERR"){
      file.copy(from=Sys.glob("it2b*"),to=paste(newdir,"/etc",sep=""))
      file.copy(from=Sys.glob("itas*"),to=paste(newdir,"/etc",sep=""))
      file.remove(Sys.glob("it2b*"))
      file.remove(Sys.glob("itas*"))
    }
    
    if(type=="ERR"){
      file.copy(from="assdriv.f",to=paste(newdir,"/etc",sep=""))   
      file.remove("assdriv.f")
    }
    
    
    file.copy(from=Sys.glob("XQZPJ*.ZMQ"),to=paste(newdir,"/wrkcopy",sep=""))
    file.copy(from="extnum",to=paste(newdir,"/etc",sep=""))
    file.copy(from=Sys.glob("*_prep*"),to=paste(newdir,"/etc",sep=""))
    file.copy(from=Sys.glob("*_run*"),to=paste(newdir,"/etc",sep=""))
    file.remove(Sys.glob("XQZPJ*.ZMQ"))
    file.remove("extnum")
    file.remove(Sys.glob("*_prep*"))
    file.remove(Sys.glob("*_run*"))
    
    #end time
    writeLines(as.character(proc.time()[3]),timeFile)
    close(timeFile)
    file.copy(from="time.txt",to=paste(newdir,"/outputs",sep=""))
    file.remove("time.txt")
    
    #make report
    if(type=="NPAG" | type=="IT2B") {PMreport(paste(workdir,newdir,"outputs",sep="/"),icen=icen,type=type,parallel=parallel)}
    if(type=="ERR") {ERRreport(paste(workdir,newdir,"outputs",sep="/"),icen=icen,type=type)}
    
    #final clean up
    setwd(workdir)
    file.copy(from=Sys.glob("*.*"),to=paste(newdir,"/inputs",sep=""))
    file.remove(Sys.glob("*.*"))
    outpath <- paste(workdir,newdir,"outputs",sep="/")
    
    
    return(outpath)    
    
  }
  
}
