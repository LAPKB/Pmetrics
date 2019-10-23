#' Runs the Pmetrics simulator
#'
#' The Monte Carlo simulator in Pmetrics is a powerful tool for parametric or semi-parametric
#' sampling.  NPAG or IT2B final objects can easily be used as the prior distributions for 
#' sampling, or prior distributions may be manually specified.  Prior distributions may be
#' unimodal-multivariate (parametric sampling), or multimodal-multivariate (semi-parametric sampling).
#' For priors from NPAG, this can easily be accomplished with the \code{split} argument. 
#' Noise can be applied to the observations. The first set of C0, C1, C2, and C3
#' in the template data file that are not missing will be used.  If all are missing,
#' the coefficients in the #error block of the model file will be used.  Noise may also be applied to
#' the observation times, to the dose times, or to the dose amounts.  These latter three are 
#' specified as arguments to the \code{SIMrun} function. Limits on the simulated parameter sets
#' can also be specified using the limits on primary parameters in the model file or by specifying
#' them manually as an argument.  It is permissible to fix a parameter for simulation that was a random
#' parameter in the model prior by changing the range in the model file to a single value for that parameter.
#' The same model and data file strutures are used for the simulator as for any other
#' Pmetrics functions.  In this case, the data file will serve as the template for the information 
#' regarding dosing, covariate values, and observations.  Template data files may have more than one
#' subject in them, in which case the simulator will use each subject specified by the \code{include}
#' argument (default is all subjects) to generate \code{nsim} parameter sets and corresponding
#' observations.  Output is directed to text files, one for each template subject, which can be 
#' read back into R by \code{link{SIMparse}}.  Output may also be directed to a new Pmetrics
#' .csv data file using the \code{makecsv} argument.
#' 
#' @title Run the Pmetrics Simulator
#' 
#' @param poppar Either an object of class \emph{PMfinal} (see \code{\link{makeFinal}})
#' or a list containing three items in this order, but of any name: vector of weights, vector of mean parameter values, and a covariance matrix.
#' If only one distribution is to be specified the \code{weights} vector should be of length 1 and contain a 1.
#' If multiple distributions are to be sampled, the \code{weights} vector should be of length equal to the number
#' of distributions and its values should sum to 1, e.g. \code{c(0.25,0.05,0.7)}.  The means matrix may be a vector for a single
#' distribution, or a matrix with \code{length(weights)} rows and number of columns equal to the number of parameters, \emph{npar}.
#' The covariance matrix will be divided by \code{length(weights)} and applied to each distribution. 
#' @param limits If limits are specified, each simulated parameter set that contains a value outside of the limits 
#' will be ignored and another set will be generated.  Four options exist for limits.  1) The default \code{NULL} 
#' indicates that no limits are to be applied to simulated parameters.   
#' 2) The second option is to set \code{limits} to \code{NA}. This will use the parameter limits on the 
#' primary parameters that are specified in the model file.  
#' 3) The third option is a numeric vector of length 1 or 2, e.g. 3 or c(0.5,4), which specifies what to multiply the
#' columns of the limits in the model file.  If length 1, then the lower limits will be the same as in the model file, and
#' the upper limits will be multiplied by value specified.  If length 2, then the lower and upper limits will be multiplied
#' by the specified values.  If this option is used, \code{popppar} must be a \code{PMfinal} object.
#' 4) The fourth option for limits is a fully customized matrix of limits for simulated values for each parameter which will
#' overwrite any limits in the model file.  If specified, it should be a data.frame or matrix with number of rows equal to the number
#' of random paramters and 2 columns, corresponding to the minimum and maximum values.  For example, a final$ab object, or a directly coded
#' matrix, e.g. matrix(c(0,5,0,5,0.01,100),nrow=3,ncol=2,byrow=T) for 3 parameters
#' with limits of [0,5], [0,5] and [0.01,100], respectively.  It is possible to convert a parameter to fixed by omitting
#' the second limit.   Means and covariances of the total number of simulated sets
#' will be returned to verify the simulation, but only those sets within the specified limits will be used to generate output(s) and the means and covariances of the
#' retained sets may (and likely will be) different than those specified by \code{poppar}.
#' @param model Name of a suitable model file template in the working directory.
#' The default is \dQuote{model.txt}.  This file will be converted to a fortran model file.
#' If it is detected to already be a fortran file, then the simulation will proceed without any further
#' file conversion.
#' @param data Either a PMmatrix object previously loaded with (\code{\link{PMreadMatrix}}) or character vector with the filename of a Pmetrics matrix file
#' that contains template regimens and observation times.  The value for outputs can be coded as
#' any number(s) other than -99.  The number(s) will be replaced in the simulator output with the simulated values.
#' @param split Boolean operator controlling whether to split an NPAG \emph{PMfinal} object into one distribution
#' per support point, with means equal to the vector of parameter values for that point, and covariance equal to
#' the population covariance divided by the number of support points
#' @param include A vector of subject IDs in the \code{matrixfile} to iterate through, with each subject
#' serving as the source of an independent simulation.  If missing, all subjects in the datafile will be used.
#' @param exclude A vector of subject IDs to exclude in the simulation, e.g. c(4,6:14,16:20)
#' If a \emph{makecsv} filename is supplied, ID numbers will be of the form nsub.nsim, e.g. 1.001 through 1.1 for the
#' first subject, 2.001 through 2.1 for the second subject, etc. if 1000 simulations are made from each subject.
#' @param nsim The number of simulated profiles to create, per subject.  Default is 1000.  Entering 0 will result in one profile being simulated from each
#' point in the non-parametric prior (for NPAG final objects only).
#' @param predInt The interval in fractional hours for simulated predicted outputs at times other than those specified in the template \code{data}.  
#' The default is 0, which means there will be simulated outputs only at times specified in the data file (see below).  Values of predInt > 0 result in simulated
#' outputs at the specified value of predInt, e.g. every 15 minutes for predInt = 0.25 from time 0 up 
#' to the maximal time in the template file, per subject if nsub > 1.  You may also specify \code{predInt}
#' as a vector of 3 values, e.g. \code{c(1,4,1)}, similar to the R command \code{\link{seq}}, where the
#' first value is the start time, the second is the stop time, and the third is the
#' step value.  Outputs for times specified in the template file will also be simulated.
#' To simulate outputs \emph{only} at the output times in the template data (i.e. EVID=0 events), use \code{predInt=0}, which is the default.
#' Note that the maximum number of predictions total is 594, so the interval must be sufficiently large to accommodate this for a given
#' number of output equations and total time to simulate over.  If \code{predInt} is set so that this cap is exceeded, predictions will be truncated.
#' @param seed The seed for the random number generator.  For \code{nsub} > 1, should be a vector of length equal to \code{nsub}.
#' Shorter vectors will be recycled as necessary.  Default is -17.
#' @param ode Ordinary Differential Equation solver log tolerance or stiffness.  Default is -4, i.e. 0.0001.  Higher values will result in faster
#' runs, but simulated concentrations may not be as accurate.
#' @param obsNoise The noise added to each simulated concentration for each output equation, where the noise
#' is randomly drawn from a normal distribution with mean 0 and SD = C0 + C1*conc + C2*conc^2 + C3*conc^3.
#' Default values are 0 for all coefficients (i.e.) no noise. If present will override any other values in the data file or model file.
#' Specify as a vector of length 4 times the number of output equations, e.g.
#' c(0.1,0.1,0,0) for one output and c(0.1,0.1,0,0,0.01,0.2,-0.001,0) for two output equations.
#' If specified as \code{NA}, values in the data file will be used (similar to \code{limits}, above).  If they are missing, values in the model file
#' will be used.
#' @param doseTimeNoise A vector of length four to specify dose time error polynomial coefficients.  The default is 0 for all coefficients.
#' @param doseNoise A vector of length four to specify dose amount error polynomial coefficients.  The default is 0 for all coefficients.
#' @param obsTimeNoise A vector of length four to specify observation timing error polynomial coefficients.  The default is 0 for all coefficients.
#' @param makecsv A character vector for the name of the single .csv file to be made for all simulated
#' \dQuote{subjects}.  If missing, no files will be made.  
#' @param outname The name for the output file(s) without an extension.  Numbers 1 to \code{nsub} will be appended to the files.
#' If missing, will default to \dQuote{simout}.
#' @param clean Boolean parameter to specify whether temporary files made in the course of the simulation run should
#' be deleted. Defaults to \code{True}.  This is primarily used for debugging.
#' @param silent Boolean operator controlling whether a model summary report is given.  Default is \code{FALSE}.
#' @param nocheck Suppress the automatic checking of the data file with \code{\link{PMcheck}}.  Default is \code{FALSE}.
#' @return No value is returned, but simulated file(s) will be in the working directory.
#' @author Michael Neely
#' @seealso \code{\link{SIMparse}}
#' @examples
#' \dontrun{
#' wd <- getwd()
#' #make 1 lognormal distribution for each parameter
#' weights <- 1
#' mean <- log(c(0.7,0.05,100))
#' cov <- matrix(rep(0,length(mean)**2),ncol=length(mean))
#' diag(cov) <- (c(0.15,0.15,0.15)*mean)**2
#' #make the prior for the simulation
#' poppar <- list(weights,mean,cov)
#' setwd(paste(normalizePath(Sys.getenv("PmetricsPath"),winslash="/"),"/Pmetrics/example/Sim",sep=""))
#' #run simulation
#' SIMrun(poppar,"temp1.csv",nsim=15,model="model1.for",obsNoise=c(0.02,0.1,0,0),makecsv="PMex1.csv",outname="example",clean=T)
#' #extract results of simulation
#' simout <- SIMparse("example1.txt")
#' file.remove("example1.txt")
#' #plot simulated profiles (use help(plot.PMsim) for more information)
#' plot(simout,ci=0,probs=NA,x.qlab=0.75,log=T,col="red",lwd=2,pch=NA,join=T)
#' setwd(wd)
#' }

SIMrun <- function(poppar,limits=NULL,model="model.txt",data="data.csv",split=F,include,exclude,nsim=1000,predInt=0,seed=-17,ode=-4,
                   obsNoise,doseTimeNoise=rep(0,4),doseNoise=rep(0,4),obsTimeNoise=rep(0,4),
                   makecsv,outname,clean=T,silent=F,nocheck=F){
  
  #make sure model file name is <=8 characters
  if(!FileNameOK(model)) {endNicely(paste("Model file name must be 8 characters or fewer.\n"),model=-99,data)}
  
  #make sure data file name is <=8 characters
  if(!FileNameOK(data)) {endNicely(paste("Data file name must be 8 characters or fewer.\n"),model,data=-99)}
  
  #check for files
  while(!file.exists(model)) {
    model <- readline(paste("The model file",shQuote(paste(getwd(),model)),"does not exist.\nEnter another filename or 'end' to quit: \n")) 
    if (tolower(model)=="end") {endNicely(paste("No model file specified.\n"),model=-99,data); break}
  }
  
  if(!inherits(data,"PMmatrix")){
    while(!file.exists(data)) {
      data <- readline(paste("The data file",shQuote(paste(getwd(),data)),"does not exist.\nEnter another filename or 'end' to quit: \n")) 
      if (tolower(data)=="end") {endNicely(paste("No data file specified.\n"),model,data=-99); break}
    }
    data <- PMreadMatrix(data,quiet=T)
  }
  
  #check for errors in data if nocheck=T
  if(nocheck){
    err <- PMcheck(data,quiet=T)
    if(attr(err,"error")==-1){
      endNicely("\nThere are errors in your template data file.  Run PMcheck.\n",model,data)
    }
  }
  
  #get information from datafile
  dataoffset <- 2*as.numeric("addl" %in% names(data))
  ncov <- ncol(data)-(12+dataoffset)
  if(ncov>0) {covnames <- names(data)[(13+dataoffset):ncol(data)]} else {covnames <- NA}
  numeqt <- max(data$outeq,na.rm=T)
  if (missing(include)){
    include <- unique(data$id)
  }
  if(!missing(exclude)){
    include <- unique(data$id)[!unique(data$id) %in% exclude]
  }
  nsub <- length(include)
  if(missing(obsNoise)){ #obsNoise not specified, set to 0 for all outeq
    obsNoise <- rep(0,4*numeqt)
  }
  if(all(is.na(obsNoise))){ #obsNoise set to NA, so get coefficients from data file; if missing, set to 0
    obsNoiseNotMiss <- lapply(1:numeqt,function(x) {
      which(!is.na(data$c0) & data$outeq==x)[1]}) #get non-missing coefficients for each output
    
    checkObsNoise <- function(x,outeq){
      if(is.na(x)) {
        if(!silent){
          cat(paste("Missing error coefficients in data file; model file defaults used for output ",outeq,".\n",sep=""))
          flush.console()
        }
        return(rep(NA,4))
      } else {return(c(data$c0[x],data$c1[x],data$c2[x],data$c3[x]))}
    }
    obsNoise <- unlist(lapply(1:numeqt,function(x) checkObsNoise(obsNoiseNotMiss[[x]],x))) 
  }
  
  
  #attempt to translate model file into  fortran model file   
  modeltxt <- model
  if(all(is.null(limits))){ #limits are omitted altogether
    limits <- NA
    omitLimits <-T
  } else {
    if(!is.na(limits[1]) & is.vector(limits)){ #limits not NA and specified as vector of length 1 or 2
      #so first check to make sure poppar is a PMfinal object
      if(!inherits(poppar,"PMfinal")) endNicely("\npoppar must be a PMfinal object when multiplicative limits specified.\n",modeltxt,data)
      orig.lim <- poppar$ab
      if(length(limits)==1) limits <- c(1,limits)
      final.lim <- t(apply(poppar$ab,1,function(x) x*limits))
      limits <- final.lim
    } else {limits <- limits} #limits specified as NA or a matrix
    omitLimits <- F
  }
  
  engine <- list(alg="SIM",ncov=ncov,covnames=covnames,numeqt=numeqt,limits=limits,indpts=-99)
  trans <- makeModel(model=model,data=data,engine=engine,write=T,silent=silent)
  if(trans$status==-1) {
    endNicely(trans$msg,modeltxt,data)
  } else {
    model <- trans$model
    nvar <- trans$nvar  #number of random parameters
    nofix <- trans$nofix #number of fixed parameters
    valfix <- trans$valfix
    asserr <- trans$asserr
    
    if(omitLimits) {  #no limits at all
      limits <- data.frame(a=rep(NA,nvar),b=rep(NA,nvar)) #no limits
    } else {
      limits <- trans$ab #limits are either from model file (which was overwritten if limits!=NA at start)
    } 
    
    ptype <- ifelse(trans$ptype==1,"r","f")
    ctype <- trans$ctype
  }
  if(identical(modeltxt,model)) {
    modelfor <- T
  } else {
    modelfor <- F
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
  if(is.null(compiler)) {cat("\nExecute SIMrun after gfortran is installed.\n");return(invisible(NULL))}
  
  enginefiles <- shQuote(normalizePath(list.files(fortSource,pattern="SIMeng",full.names=T)))
  enginecompile <- sub("<exec>","montbig.exe",compiler)
  enginecompile <- sub("<files>",enginefiles,enginecompile,fixed=T)
  
  if(missing(makecsv)){
    makecsv <- 0
  } else {
    if(file.exists(makecsv)) file.remove(makecsv)
    orig.makecsv <- makecsv
    makecsv <- c("1","abcde.csv")
  }
  
  #get prior density
  if(inherits(poppar,"PMfinal")){
    if(split & inherits(poppar,"NPAG")){
      popPoints <- poppar$popPoints
      ndist <- nrow(popPoints)
      if(ndist>30) {ndist <- 30} #take the 30 most probable points as there are max 30 distributions in simulator          
      popPointsOrdered <- popPoints[order(popPoints$prob),]
      pop.weight <- popPointsOrdered$prob[1:ndist]
      pop.mean <- popPointsOrdered[1:ndist,1:(ncol(popPointsOrdered)-1)]
      pop.cov <- poppar$popCov
      
    } else {
      pop.weight <- 1
      pop.mean <- data.frame(t(poppar$popMean))
      pop.cov <- poppar$popCov
      ndist <- 1 
    }
    #if there are fixed variables in simulation, check to see which should be fixed in prior and remove if necessary
    if(nofix>0){
      whichfix <- trans$blocks$primVar[ptype=="f"]
      whichrand <- trans$blocks$primVar[ptype=="r"]
      modelpar <- names(pop.mean)
      if(!all(modelpar %in% c(whichfix,whichrand))) stop("Primary parameters in simulation model file do not match parameters\nin the PMfinal object used as a simulation prior.\n")
      tofix <- which(modelpar %in% whichfix)
      if(length(tofix)>0){
        pop.mean <- pop.mean[,-tofix]
        pop.cov <- pop.cov[-tofix,-tofix]
      }
    }
  } else {
    pop.weight <- poppar[[1]]
    ndist <- length(pop.weight)
    if(inherits(poppar[[2]],"numeric")) {pop.mean <- data.frame(t(poppar[[2]]))} else {pop.mean <- data.frame(poppar[[2]])}
    pop.mean <- pop.mean[order(pop.weight),] #sort means by order of probability
    if(ndist>30) {ndist <- 30} #take the 30 most probable points as there are max 30 distributions in simulator          
    pop.weight <- sort(pop.weight)
    pop.weight <- pop.weight[1:ndist]
    pop.mean <- pop.mean[1:ndist,]
    pop.cov <- data.frame(poppar[[3]])
  }
  
  pop.cov[upper.tri(pop.cov)] <- NA
  pop.cov <- as.vector(t(pop.cov))
  pop.cov <- pop.cov[!is.na(pop.cov)]
  pop.cov <- pop.cov/ndist
  
  #if nsim=0 then we will use each population point to simulate a single
  #output based on the template; otherwise, we will use the specified prior
  
  if(nsim==0 & inherits(poppar,"NPAG")){
    popPoints <- poppar$popPoints
    #put it all together in the following order
    #2:                             enter values from "results of BIG NPAG run"
    #2:                             use each grid point once
    #1:                             enter values manually
    #ndist:                         number of grid points
    #gridpts:                       values of gridpoints
    gridpts <- c(t(popPoints[,1:nvar]))
    priorSource <- c(2,2,1,nrow(popPoints),gridpts)
    nsim <- 1 #change to avoid error, but it will be ignored
    
    #make some confirmation answers
    #rep(1,2):                            #confirm one point per sim
    #confirm gridpoints 
    
    confirm <- rep(1,2)
    
  } #end of block to make distribution when nsim=0
  
  else {
    #put it all together in the following order
    #1:                             enter values from "keyboard"
    #ndist:                         number of distributions
    #0:                             covariances
    #dist:                          weight, mean, 0 for covariance matrix, and cov matrix for each dist
    #1:                             gaussian distributions
    #make distribution string
    dist <- list()
    for (i in 1:ndist){
      dist[[i]] <- unlist(c(pop.weight[i],pop.mean[i,],0,pop.cov))
    }
    dist <- unlist(dist)
    priorSource <- c(1,ndist,0,dist,1)
    
    #make some confirmation answers
    #0:                            #covariance matrix
    #rep("go",ndist):                #view distributions
    #rep("1",2):                     #distribution info is correct
    #restrictions on parameters are correct
    confirm <- c("0",rep("go",ndist),rep("1",2))
    
    #double check nsim isn't 0
    if(nsim==0) nsim <- 1
    
  } #end of block to make distribution when nsim>0
  
  #other simulation arguments
  if (missing(outname)){outname <- "simout"}
  if(identical(length(obsNoise),length(asserr))){
    obsNoise[is.na(obsNoise)] <- asserr[is.na(obsNoise)] #try to set any missing obsNoise to asserr from model file
  } else {obsNoise[is.na(obsNoise)] <- 0} #but if can't, set any missing obsNoise to 0
  ode <- c(0,10**ode)
  
  #apply limits as necessary
  if(sum(ptype=="r")>ncol(pop.mean)) stop("You have specified variables to be random in your model file\nthat were not random in poppar.\n")
  varDF <- data.frame(ptype=ptype,limit=ifelse(ptype=="r" & all(is.na(limits)),1,0))
  varDF$limit[varDF$ptype=="f"] <- NA
  varDF$a[varDF$ptype=="r"] <- limits[,1]
  varDF$b[varDF$ptype=="r"] <- limits[,2]
  varVec <- c(apply(varDF,1,c))
  varVec <- varVec[!is.na(varVec)]
  varVec <- gsub("[[:space:]]","",varVec)
  
  
  
  #compile simulator
  if(OS==1 | OS==3) {
    system(paste(enginecompile,model))
  } else {
    shell(paste(enginecompile,model))
  }
  #create seed
  if(missing(seed)) seed <- rep(-17,nsub)
  if(length(seed)<nsub) seed <- rep(seed,nsub) 
  seed <- floor(seed) #ensure that seed is a vector of integers
  
  if(!clean){
    instructions <- c("1","sim.inx")
  } else (instructions <- "0" )
  
  
  
  #cycle through the subjects and simulate
  if(!silent) cat(paste("\nThe following subject(s) in the data will serve as the template(s) for simulation: ",paste(include,collapse=" "),"\n\n"))
  for (i in 1:nsub){
    if(!silent){
      cat(paste("Simulating from subject",include[i],"...\n"))
      flush.console()
    }
    
    temp <- subset(data,data$id==include[i])
    if(nrow(temp)==0){
      if(!silent){
        cat(paste("\nThere is no subject with an ID of ",include[i],". Skipping...\n",sep=""))
        flush.console()
      }
      next
    }
    #add prediction times if necessary
    if(predInt[1]>0){
      if(length(predInt)==1){
        predTimes <- rep(seq(0,ceiling(max(temp$time,na.rm=T)),predInt)[-1],each=numeqt)
      }else{
        if(length(predInt)==3){
          predTimes <- rep(seq(predInt[1],predInt[2],predInt[3]),each=numeqt)
          
        } else {stop("\npredInt is misspecified.  See help for SIMrun.\n")}
      }
      predTimes <- predTimes[!predTimes %in% data$time[data$evid==0]] #remove prediction times at times that are specified in template
      numPred <- length(predTimes)
      maxsim <- 594-length(temp$evid[temp$evid==0])
      if(numPred>maxsim){  #too many predictions
        numPred.total <- numPred + length(temp$evid[temp$evid==0])
        predTimes <- predTimes[1:(maxsim-maxsim%%numeqt)]
        numPred <- length(predTimes)
        cat(paste("The maximum number of simulated observations is 594.  Your prediction interval, specific prediction times, and time horizon results in ",numPred.total," predictions.\nInterval predictions will be truncated at time ",predTimes[numPred],", plus predictions at specific times in the template (if any).\n",sep=""))
      }
      newPred <- data.frame(matrix(NA,nrow=numPred,ncol=ncol(temp)))
      newPred[,1] <- temp$id[1] #id
      newPred[,2] <- 0 #evid
      newPred[,3] <- predTimes #time
      newPred[,9] <- 1 #out
      newPred[,10] <- rep(1:numeqt,numPred/numeqt) #outeq
      names(newPred) <- names(temp)
      temp <- rbind(temp,newPred)
      temp <- temp[order(temp$time,temp$outeq),]    
    }
    
    PMwriteMatrix(temp,"ZMQtemp.csv",override=T,version="DEC_11")
    if(length(makecsv)==2) makecsv[2] <- paste("abcde",i,".csv",sep="")
    outfile <- paste(outname,i,".txt",sep="")
    if(file.exists(outfile)) {file.remove(outfile)}
    if(file.exists("sim.inx")) {file.remove("sim.inx")}
    
    #build the control stream   
    simControl <- unlist(c("1",                            #files in current directory
                           "0",                            #input from "keyboard"
                           model,                          #name of model file
                           varVec,                        #random parameters and limits if they exist
                           "1",                            #input from .csv file
                           "ZMQtemp.csv",                     #name of .csv file
                           ctype,                  #piecewise covariates
                           "go",
                           nsim,                           #number of simulations/subject
                           valfix,                         #value of any fixed parameters
                           ode,                            #ode tolerance
                           obsNoise,                       #observation noise
                           "1",                            #skip explanation of noisy values
                           doseTimeNoise,                  #dose time noise
                           doseNoise,                      #dose noise
                           obsTimeNoise,                   #observation time noise
                           priorSource,                    #prior 
                           outfile,                        #output file name (without extension)
                           makecsv,                        #make .csv file?
                           "0",                            #read file seeto.mon for seed
                           rep("1",2),                     #data file info is correct
                           #nsim is correct                               
                           "go",
                           rep("go",numeqt),
                           rep("1",3),                     #observation error information is correct
                           #skip explanation of noisy values
                           #other error information is correct
                           confirm,                        #confirm answers
                           rep("1",4),                     #common confimations  
                           #output file is correct
                           #confirm .wrk file generation
                           #confirm starting seed
                           #all instructions are now correct
                           instructions))                           #instruction file
    simControl <- simControl[!is.na(simControl)]
    f <- file("simControl.txt","w")
    writeLines(simControl,f,sep="\r\n")
    close(f)
    
    #make seed file and run
    if(OS==1 | OS==3) {
      system(paste("echo",seed[i],"> seedto.mon"))
      system("./montbig.exe MacOSX < simControl.txt",ignore.stdout=T)
    } else {
      shell(paste("echo",seed[i],"> seedto.mon"))
      shell("montbig.exe DOS < simControl.txt",invisible=T)
    }
  }
  #clean up csv files if made
  if(length(makecsv)==2){
    trunc <- ceiling(log10(nsim+1))
    temp <- PMreadMatrix("abcde1.csv",quiet=T)
    simnum <- unlist(lapply(temp$id,function(x) substr(gsub("[[:space:]]","",x),9-trunc,8)))
    temp$id <- paste(include[1],simnum,sep="_")
    if(any(unlist(lapply(as.character(unique(temp$id)),function(x) nchar(x)>11)))) stop("Shorten all template id values to 6 characters or fewer.\n")
    if(nsub>1){
      for(j in 2:nsub){
        curTemp <- PMreadMatrix(paste("abcde",j,".csv",sep=""),quiet=T)   
        simnum <- unlist(lapply(curTemp$id,function(x) substr(gsub("[[:space:]]","",x),9-trunc,8)))
        curTemp$id <- paste(include[j],simnum,sep="_")
        if(any(unlist(lapply(as.character(unique(curTemp$id)),function(x) nchar(x)>11)))) stop("Shorten all template id values to 6 characters or fewer.\n")
        temp <- rbind(temp,curTemp)
      }
    }
    zero <- which(temp$evid==1 & temp$dur==0 & temp$dose==0)
    if(length(zero)>0) temp <- temp[-zero,]
    
    ### update the version once simulator updated        
    PMwriteMatrix(temp,orig.makecsv,override=T,version="DEC_11")
  }
  exampleName <- paste(outname,"1.txt",sep="")
  if(!silent){
    if(length(Sys.glob(paste(outname,"*",sep="")))>0) cat("\nUse, for example, SIMparse(",dQuote(exampleName),") to extract simulator output for analysis or plotting.\n",sep="")
    flush.console()
  }
  if(clean) {
    invisible(file.remove(Sys.glob(c("fort.*","*.Z3Q","*.ZMQ","montbig.exe","ZMQtemp.csv","simControl.txt","seedto.mon","abcde*.csv"))))
    if(!modelfor){
      invisible(file.remove(model))
    }
  }
}

