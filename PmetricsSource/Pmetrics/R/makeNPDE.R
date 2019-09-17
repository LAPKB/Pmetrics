#' Use simulations to run model diagnostic tests.
#'
#' This function is a Pmetrics wrapper to the \code{autonpde} function in the \code{npde}
#' package of Comets et al that will generate normalized prediction distribution errors.
#' Output from a loaded NPAG or IT2B run will be used as the population model supplied to the
#' simulator. The function will iterate through the .csv file, using each subject as a template to simulate \code{nsim} new individuals. 
#' It is HIGHLY recommended to use the default value of 1000 for \code{nsim} for the most valid  
#' calculation of npde.  More than this could take a long time to execute.
#' The mean population values will be used for each parameter and the covariance matrix.  
#' Errors may arise if extreme or negative concentrations are simulated
#' from excessively large covariance matrices.  
#'
#' @title Simulation-based model diagnostics
#'
#' @param run When the current working directory is the Runs folder, the folder name of a previous run that you wish to use for the npde,
#' which will typically be a number, e.g. 1.
#' @param data An optional data file in the Runs folder to use as an external dataset for NPDE.  The population parameters
#' from \code{run} will be used with the subjects in \code{data}.  If missing, the original population
#' will be used.
#' @param outeq The number of the output equation to simulate/test.  Default is missing, which will test all output equations.
#' @param nsim The number of simulations per subject in the data file.  We recommend 1000 (the default)
#' to return valid npde results.  More may result in excessive simulation times.
#' @param \dots Other parameters to be passed to \code{\link{SIMrun}}. 
#' @return The output of \code{makeNPDE} is a list of class \code{PMnpde} with objects
#' of \code{NpdeObject} class.  Additionally, two objects with run numbers appended will be saved to the output directory of the run for subsequent
#' loading with \code{\link{PMload}}: npde and sim.  \emph{npde} is the PMnpde object, and \emph{sim} is a 
#' PMsim object of all simulations combined which can be used for visual predictive checks
#' (see \code{\link{plot.PMsim}}).
#' @author Michael Neely
#' @seealso \code{\link{SIMrun}}, \code{\link{autonpde}}, \code{\link{plot.PMnpde}}
#' @references 
#' Brendel K, Comets E, Laffont CM, Laveille C, Mentre F (2006) Metrics for external model evaluation with an application to the population pharmacokinetics of gliclazide. \emph{Pharmaceutical Research}, 23:2036-49
#' 
#' Mentre F, Escolano S (2006) Prediction discrepancies for the evaluation of nonlinear mixed-effects models. \emph{J Pharmacokinet Pharmacodyn}, 33:345-67

makeNPDE <- function(run,data,outeq,nsim=1000,...){
  
  if(length(grep("npde",installed.packages()[,1]))==0){
    install.packages("npde",repos="http://cran.cnr.Berkeley.edu",dependencies=T)
  }
  npde.installed <- require(npde,quietly=T,warn.conflicts=F)
  if(!npde.installed) stop("Package npde not installed.\n")
  
  getName <- function(x){
    return(get(paste(x,run,sep=".")))
  }
  
  #copy model file if available
  if(!missing(run)){
    if(!file.exists(as.character(run))) {stop(paste(run," not found in the current working directory.\n",sep=""))}
    if(!file.exists(paste(run,"/npde",sep=""))) dir.create(paste(run,"/npde",sep=""))
    
    #get model and data files
    instrfile <- suppressWarnings(tryCatch(readLines(paste(run,"etc/instr.inx",sep="/")),error=function(e) NULL))
    if(length(grep("IVERIFY",instrfile))==0){ #not updated instruction file
      modelfile <- readline("Your run used an old instruction file. Enter model name. ")
    } else { #ok we are using updated instruction file
      if(length(instrfile)>0){  #ok we got one
        #model.for file name
        modelfile <- instrfile[5]
        #convert to original name    
        modelfile <- basename(Sys.glob(paste(run,"/inputs/",strsplit(modelfile,"\\.")[[1]][1],"*",sep="")))
        if(length(modelfile)>1){modelfile <- modelfile[grep(".txt",modelfile)]}
        
      } else {stop("Model file not found.\n")}
    }
    
    #copy this modelfile to new /npde folder
    invisible(file.copy(from=paste(run,"/inputs/",modelfile,sep=""),to=paste(run,"/npde",sep="")))
    
    
    #now get the data file  
    if(missing(data)){
      RFfile <- suppressWarnings(tryCatch(readLines(Sys.glob(paste(run,"outputs/??_RF0001.TXT",sep="/"))),error=function(e) NULL))
      if(length(RFfile)>0){
        datafileName <- tail(RFfile,1)
        file.copy(from=paste(run,"inputs",datafileName,sep="/"),to=paste(run,"/npde",sep=""))
        datafile <- datafileName
      } else {stop("Data file not found\n")}
    } else {
      datafileName <- data
      file.copy(from=datafileName,to=paste(run,"/npde",sep=""))
      file.remove(datafileName)
    }
  } else {stop("Please supply a run number.\n")}
  
  #parse dots
  arglist <- list(...)
  namesSIM <- names(formals(SIMrun))
  #namesNPDE <- names(formals(autonpde))
  argsSIM <- arglist[which(names(arglist) %in% namesSIM)]
  #argsNPDE <- arglist[which(names(arglist) %in% namesNPDE)]
  
  #get and format data appropriately
  PMload(run)
  currwd <- getwd()
  setwd(paste(run,"/npde",sep=""))
  data <- PMreadMatrix(datafile,quiet=T)
  if("include" %in% names(argsSIM)){
    data <- data[data$id %in% argsSIM$include,]
  }
  if("exclude" %in% names(argsSIM)){
    data <- data[!data$id %in% argsSIM$exclude,]
  }
  
  nsub <- length(unique(data$id))
  nout <- max(data$outeq,na.rm=T)
  
  
  #remove old files
  invisible(file.remove(Sys.glob("simout*.txt")))
  
  #do the simulation
  if("seed" %in% names(argsSIM)){
    seed.start <- argsSIM$seed
    argsSIM[[which(names(argsSIM)=="seed")]] <- NULL
  } else {seed.start <- -17}
  set.seed(seed.start)
  argsSIM <- c(list(poppar=get(paste("final",run,sep=".")),data=datafile,model=modelfile,nsim=nsim,seed=runif(nsub,-100,100),obsNoise=rep(0,4*nout)),argsSIM)
  do.call("SIMrun",argsSIM)
  #read and format the results of the simulation
  simdata <- SIMparse("simout*")
  
  
  #prepare data for npde
  obs <- subset(data,(data$evid==0))
  obs <- obs[,c("id","time","out","outeq")]
  #remove missing obs
  obs <- obs[obs$out!=-99,]

  
  if(inherits(simdata,"PMsim")){ #only one simulated subject
    simobs <- simdata$obs
  } else {simobs <- lapply(simdata,function(x) x$obs)} #multiple simulated subjects
  
  simobs <- lapply(1:nsub,function(x) {
    simobs[[x]]$id <- unique(data$id)[x];
    simobs[[x]]$nsub <- x;
    simobs[[x]]$nsim <- 1:nsim;
    simobs[[x]]})
  
  sim <- do.call("rbind",simobs)
  sim <- sim[order(sim$nsim,sim$nsub,sim$time),]
  sim <- subset(sim,!is.na(sim$out))
  #remove missing simulations
  sim <- sim[sim$out!=-99,]
  
  #cycle through all outeq if outeq is missing
  allouteq <- 1:max(obs$outeq,na.rm=T)
  
  if(!missing(outeq)) allouteq[-outeq] <- NA
  
  npdeRes <- list(length(allouteq))
  
  for(nout in 1:length(allouteq)){
    if(is.na(allouteq[nout])) {npdeRes[[nout]] <- NULL;next}
    thisobs <- obs[obs$outeq==nout,]
    thissim <- sim[sim$outeq==nout,]
    assign("thisobs",thisobs,pos=1)
    assign("thissim",thissim,pos=1)
    thisNpdeRes <- tryCatch(autonpde(namobs=thisobs,namsim=thissim,iid="id",ix="time",iy="out",imdv="mdv",verbose=T),error=function(e) e)
    npdeRes[[nout]] <- thisNpdeRes
  }
  
  #combine simdata for saving 
  #make unique id values of "id.simnumber" for each set
  simdata <- lapply(seq_along(simdata),function(x) {simdata[[x]]$obs$id <- paste(simdata[[x]]$obs$id,sprintf("%02d",x),sep=".");simdata[[x]]})
  obs <- do.call(rbind,lapply(simdata,function(x) x$obs))
  amt <- do.call(rbind,lapply(simdata,function(x) x$amt))
  parValues <- do.call(rbind,lapply(simdata,function(x) x$parValues))
  totalSets <- sum(sapply(simdata,function(x) x$totalSets))
  totalMeans <- do.call(rbind,lapply(simdata,function(x) x$totalMeans*x$totalSets))
  totalMeans <- apply(totalMeans,2,function(x) sum(x)/totalSets)
  totalCov <- Reduce("+",lapply(simdata,function(x) x$totalCov*x$totalSets))/totalSets
  simdata <- list(obs=obs,amt=amt,parValues=parValues,totalSets=totalSets,totalMeans=totalMeans,totalCov=totalCov)
  
  class(simdata) <- c("PMsim","list")
  class(npdeRes) <- c("PMnpde","list")
  
  NPAGout <- list(NPdata=getName("NPdata"),
                  pop=getName("pop"),
                  post=getName("post"),
                  final=getName("final"),
                  cycle=getName("cycle"),
                  op=getName("op"),
                  cov=getName("cov"),
                  mdata=getName("mdata"),
                  npde=npdeRes,
                  sim=simdata)
  save(NPAGout,file="../outputs/NPAGout.Rdata")
  
  setwd(currwd)
  #put sim in global environment
  assign(paste("sim",as.character(run),sep="."),simdata,pos=1)
  
  
  return(npdeRes)  
}
