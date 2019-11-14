#' Creates a Pmetrics validation object
#'
#' \code{makeValid} will create an object suitable for plotting visual predictive checks (VPCs), prediction-corrected visual
#' predictive checks (pcVPCs) and normalized prediction distribution errors (NPDEs). The function will guide the user
#' through appropriate clustering of doses, covariates and sample times for prediction correction using the methods of
#' 
#'
#' @title Create a Pmetrics validation object
#' @param simdata Can be one of multiple inputs.  Typically it is a vector of simulator output filenames, e.g. c(\dQuote{simout1.txt},\dQuote{simout2.txt}),
#' with wildcard support, e.g. \dQuote{simout*} or \dQuote{simout?}, or
#' a list of PMsim objects made by \code{\link{SIMparse}} with suitable simulated regimens and observations.  The number and times of simulated
#' observations does not have to be the same in all objects.  It can also be a \emph{PMpta} object previously made with
#' \code{makePTA} can be passed for recalculation with a new success value or simlabels.  Finally, \emph{PMpost} and \emph{PMmatrix} objects are 
#' also allowed.
#' @param simlabels Optional character vector of labels for each simulation.  Default is \code{c(\dQuote{Regimen 1}, \dQuote{Regimen 2},...)}.
#' @param targets A vector of pharmacodynamic targets, such as Minimum Inhibitory Concentrations (MICs), e.g. c(0.25, 0.5,1,2,4,8,16,32).
#' This can also be a sampled distribution using  \code{\link{makePTAtarget}}.
#' @param target.type A numeric or character vector, length 1.  If numeric, must correspond to an observation time common to all PMsim objects in
#' \code{simdata}, rounded to the nearest hour.  In this case, the target statistic will be the ratio of observation at time \code{target.type} to target.  This enables 
#' testing of a specific timed concentration (e.g. one hour after a dose or C1) which may be called a peak, but is not actually the maximum drug
#' concentration.  Be sure that the time in the simulated data is used, e.g. 122 after a dose given at 120.  Character values may be one of 
#' \dQuote{time}, \dQuote{auc}, \dQuote{peak}, or \dQuote{min}, for, respectively, percent time above target within the time range
#' specified by \code{start} and \code{end}, ratio of area under the curve within the time range to target, ratio of peak concentration within the time range 
#' to target, or ratio of minimum concentration within the time range to target.  
#' @param success A single value specifying the success statistic, e.g. 0.4 for proportion time (end-start) above target, or 100 for peak:target.
#' @param outeq An integer specifying the number of the simulated output equation to use. Default is 1.
#' @param free.fraction Proportion of free, active drug.  Default is 1, i.e. 100\% free drug or 0\% protein binding.
#' @param start Specify the time to begin PTA calculations. Default is a vector with the first observation time for subjects
#' in each element of \code{simdata}, e.g. dose regimen. If specified as a vector, values will be recycled as necessary.
#' @param end Specify the time to end PTA calculations so that PTA is calculated
#' from \code{start} to \code{end}.  Default for end is the maximum observation
#' time for subjects in each element of \code{simdata}, e.g. dose regimen.  If specified as a vector, values will be recycled
#' as necessary. Subjects with insufficient data (fewer than 5 simulated observations) for a specified interval will trigger a warning.
#' Ideally then, the simulated datset should contain sufficient observations within the interval specified by \code{start} and \code{end}.
#' @return The output of \code{makeValid} is a list of class \emph{PMvalid},
#' which has 2 objects:
#' \item{results }{A data frame with the following columns: simnum, id, target, pdi.  
#' \emph{simnum} is the number of the simulation; \emph{id} is the simulated profile number
#' within each simulation; \emph{target} is the specified target; and \emph{pdi} is
#' the target pharmacodynamic index, e.g. time > target, auc:target, etc.}
#' \item{outcome }{A data frame summarizing the results with the following columns: simnum, target, prop.success, pdi.mean, and pdi.sd.
#' If \code{targets} was specified via \code{\link{makePTAtarget}} to be a sampled distribution, then
#' the target column will be missing from the outcome table.
#' \emph{simnum} and \emph{target} are as for \code{results}.  The \emph{prop.success} column has the proportion with a pdi > \code{success},
#' as specified in the function call.  The \emph{pdi.mean} and \emph{pdi.sd} columns have the 
#' mean and standard deviation of the target pharmacodynamic index (e.g. proportion end-start above target, ratio of Cmax to target) for each simulation and target.}  
#' @author Michael Neely and Jan Strojil
#' @seealso \code{\link{plot.PMpta}}, \code{\link{SIMparse}}

makeValid <- function(run,outeq=1,input=1,icen="median",tad=F,binCov,doseC,timeC,...){
  
  if(length(grep("mclust",installed.packages()[,1]))==0){
    install.packages("mclust",repos="http://cran.cnr.Berkeley.edu",dependencies=T)
  }
  mclust.installed <- require(mclust,quietly=T,warn.conflicts=F)
  if(!mclust.installed) stop("Package mclust not installed.")
  
  if(length(grep("npde",installed.packages()[,1]))==0){
    install.packages("mclust",repos="http://cran.cnr.Berkeley.edu",dependencies=T)
  }
  npde.installed <- require(npde,quietly=T,warn.conflicts=F)
  if(!npde.installed) cat("Package npde not installed, so npde cannot be made.\n")
  #save current wd
  currwd <- getwd()
  
  #get the run
  if(missing(run)) run <- readline("Enter the run number: ")
  PMload(run)
  
  getName <- function(x){
    return(get(paste(x,run,sep=".")))
  }
  
  #parse dots
  arglist <- list(...)
  namesSIM <- names(formals(SIMrun))
  #namesNPDE <- names(formals(autonpde))
  argsSIM <- arglist[which(names(arglist) %in% namesSIM)]
  
  # Cluster raw data --------------------------------------------------------
  
  #grab raw data file
  mdata <- getName("mdata")
  maxInput <- max(mdata$input,na.rm=T)
  maxOuteq <- max(mdata$outeq,na.rm=T)
  if(outeq > maxOuteq){
    stop("You entered an output equation number greater than the number of output equations.")
  }
  if(input > maxInput){
    stop("You entered a drug input number greater than the number of drug inputs.")
  }
  #filter mdata to appropriate input and outeq
  mdata <- mdata[(mdata$evid>0 & mdata$input==input) | (mdata$evid==0 & mdata$outeq==outeq),]
  
  #filter to include/exclude subjects
  if("include" %in% names(argsSIM)){
    includeID <- argsSIM$include
    mdata <- mdata[mdata$id %in% includeID,]
    argsSIM[[which(names(argsSIM)=="include")]] <- NULL
  } else {includeID <- NA}
  if("exclude" %in% names(argsSIM)){
    excludeID <- argsSIM$exclude
    mdata <- mdata[!mdata$id %in% excludeID,]
    argsSIM[[which(names(argsSIM)=="exclude")]] <- NULL
  } else {excludeID <- NA}
  
  #if tad=T calculate time after dose 
  #warn that dosing should be homogenous
  if(tad==T){
    cat("WARNING: You have chosen to use time after dose.\nThis is only valid if dosing does not change within a subject and observations are at steady state.\n")
    
    for(i in 1:nrow(mdata)){
      if(mdata$evid[i]!=0){
        doseTime <- mdata$time[i]
        prevDose <- mdata$dose[i]
      }
      mdata$tad[i] <- mdata$time[i] - doseTime
    }
    mdata$timeOrig <- mdata$time
    mdata$time <- mdata$tad
  }
  
  nsub <- length(unique(mdata$id))
  
  if(missing(binCov)){
    cat(paste("Covariates in your data file: ",paste(names(mdata)[-c(1:get("nfixed",envir=PMenv))],collapse=", ")))
    binCov <- readline("Enter any covariates to be binned, separated by commas (<Return> for none): ")
    binCov <- unlist(strsplit(binCov,","))
    #remove leading/trailing spaces
    binCov <- gsub("^[[:space:]]|[[:space:]]$","",binCov)
  }
  
  if(!all(binCov %in% names(mdata))){
    stop("You have entered covariates which are not valid covariates in your data.")
  }
  #set up data for clustering
  #fill in gaps for cluster analysis only for binning variables (always dose and time)
  mdata2 <- mdata[,c("id","evid","time","dose",binCov)]
  #set zero doses (covariate changes) as missing
  mdata2$dose[mdata2$dose==0] <- NA
  for(i in 1:nrow(mdata2)){
    missingVal <- which(is.na(mdata2[i,]))
    if(4 %in% missingVal){ #dose is missing
      if(i==1 | (mdata2$id[i-1]!=mdata2$id[i])){ #first record for patient has zero dose
        j <- 0
        while(is.na(mdata2$dose[i+j])){ #increment until non-zero dose is found
          j <- j+1
        }
        mdata2$dose[i] <- mdata2$dose[i+j] #set dose equal to first non-zero dose
        missingVal <- missingVal[-which(missingVal==3)] #take out missing flag for dose as it has been dealt with
      }
    }
    mdata2[i,missingVal] <- mdata2[i-1,missingVal]
  }
  
  #restrict to doses for dose/covariate clustering (since covariates applied on doses)
  mdata2a <- mdata2[mdata2$evid>0,-c(1:3)]
  #restrict to observations for time clustering
  mdata2b <- mdata2$time[mdata2$evid==0]
  
  #ELBOW PLOT for clustering if used
  elbow <- function(x){
    set.seed(123)
    # Compute and plot wss for k = 2 to k = 15.
    # set k.max
    if(is.null(dim(x))){
      k.max <- min(length(unique(x)),15)
    }else{
      k.max <- min(nrow(unique(x)),15)
    }
    
    wss <- sapply(2:k.max, 
                  function(k){val <- kmeans(x, k, nstart=50,iter.max = 15 );
                  val$tot.withinss})
    wss
    plot(2:k.max, wss,
         type="b", pch = 19, frame = FALSE, 
         xlab="Number of clusters",
         ylab="Total within-clusters sum of squares (WSS)") 
  }
  
  
  if(missing(doseC)){
    #DOSE/COVARIATES
    cat("Now optimizing clusters for dose/covariates.\n")
    cat("First step is a Gaussian mixture model analysis, followed by an elbow plot.\n")
    readline(paste("Press <Return> to start cluster analysis for ",
                   paste(c("dose",binCov),collapse=", ",sep=""),": ",sep=""))
    cat("Now performing Gaussian mixture model analysis.")
    mod1 <- Mclust(mdata2a)
    cat(paste("Most likely number of clusters is ",mod1$G,".",sep=""))
    readline("Press <Return> to see classification plot: ")
    plot(mod1,"classification")   
    readline("Press <Return> to see elbow plot: ")
    elbow(mdata2a)
    doseC <- as.numeric(readline(paste("Specify your dose/covariate cluster number, <Return> for ",mod1$G,": ",sep="")))
    if(is.na(doseC)) doseC <- mod1$G
  } #end if missing doseC
  
  if(missing(timeC)){
    #TIME
    readline("Press <Return> to start cluster analysis for sample times: ")
    mod2 <- Mclust(mdata2b)
    cat(paste("Most likely number of clusters is ",mod2$G,".\n",sep=""))
    readline("Press <Return> to see classification plot: ")
    plot(mod2,"classification")  
    readline("Press <Return> to see cluster plot: ")
    plot(out~time,mdata[mdata$out>-99,],xlab="Time",ylab="Observation",xlim=c(min(mdata$time[mdata$evid==0]),
                                                                              max(mdata$time[mdata$evid==0])))
    abline(v=mod2$parameters$mean,col="red")
    readline("Press <Return> to see elbow plot: ")
    elbow(mdata2b)
    ans <- readline(cat(paste("Enter:\n<1> for ",mod2$G," clusters\n<2> for a different number of automatically placed clusters\n<3> to manually specify cluster centers ", sep="")))
    if(ans==1) {timeC <- mod2$G}
    if(ans==2) {timeC <- readline("Specify your sample time cluster number\n")}
    if(ans==3) {
      confirm <- 2
      while(confirm!=1){
        plot(out~time,mdata[mdata$out>-99,],xlab="Time",ylab="Observation",xlim=c(min(mdata$time[mdata$evid==0]),
                                                                                  max(mdata$time[mdata$evid==0])))
        timeVec <- readline("Specify a comma-separated list of times, e.g. 1,2,8,10: ")
        timeVec <- as.numeric(strsplit(timeVec,",")[[1]])
        abline(v=timeVec,col="red")
        confirm <- readline(cat("Enter:\n<1> to confirm times\n<2> to revise times "))
      }
      timeC <- timeVec
    }    
    if(is.na(timeC)) timeC <- mod2$G
  } #end if missing timeC
  
  #now set the cluster bins
  mod3 <- kmeans(mdata2a,centers=doseC,nstart=50)
  mdata2$mbin[mdata2$evid>0] <- mod3$cluster  #m=dose,covariate bins
  
  mod4 <- kmeans(mdata2b,centers=timeC,nstart=50)
  mdata2$nbin[mdata2$evid==0] <- sapply(mod4$cluster,function(x) which(order(mod4$centers)==x))  #n=ordered time bins
  
  
  # Simulations -------------------------------------------------------------
  
  #create /vpc
  if(!file.exists(paste(run,"/vpc",sep=""))) dir.create(paste(run,"/vpc",sep=""))
  
  #get model file
  instrfile <- suppressWarnings(tryCatch(readLines(paste(run,"etc/instr.inx",sep="/")),error=function(e) NULL))
  if(length(grep("IVERIFY",instrfile))==0){ #not updated instruction file
    modelfile <- readline("Your run used an old instruction file. Enter model name: ")
  } else { #ok we are using updated instruction file
    if(length(instrfile)>0){  #ok we got one
      #model.for file name
      modelfile <- instrfile[5]
      #convert to original name    
      modelfile <- basename(Sys.glob(paste(run,"/inputs/",strsplit(modelfile,"\\.")[[1]][1],"*",sep="")))
      if(length(modelfile)>1){modelfile <- modelfile[grep(".txt",modelfile)]}
      
    } else {stop("Model file not found.\n")}
  }
  
  #copy this modelfile to new /vpc folder
  invisible(file.copy(from=paste(run,"/inputs/",modelfile,sep=""),to=paste(run,"/vpc",sep="")))
  
  #now get the data file  
  RFfile <- suppressWarnings(tryCatch(readLines(Sys.glob(paste(run,"outputs/??_RF0001.TXT",sep="/"))),error=function(e) NULL))
  if(length(RFfile)>0){
    datafileName <- tail(RFfile,1)
    file.copy(from=paste(run,"inputs",datafileName,sep="/"),to=paste(run,"/vpc",sep=""))
    datafile <- datafileName
  } else {stop("Data file not found\n")}
  
  #change wd to new /vpc folder which now contains data and model files
  setwd(paste(run,"/vpc",sep=""))
  
  #simulate PRED_bin from pop icen parameter values and median of each bin for each subject
  #first, calculate median of each bin
  med1 <- aggregate(mdata2[,4:(ncol(mdata2)-2)],by=list(mdata2$mbin),FUN=median)
  names(med1) <- c("bin","dose",binCov)
  med2 <- aggregate(mdata2$time,by=list(mdata2$nbin),FUN=median)
  names(med2) <- c("bin","time")
  
  #create  datafile based on mdata, but with covariates and doses replaced by medians
  #and sample times by bin times
  mdataMedian <- mdata
  mdataMedian$mbin <- mdata2$mbin
  mdataMedian$nbin <- mdata2$nbin
  mdataMedian$dose <- med1$dose[match(mdataMedian$mbin,med1$bin)]
  mdataMedian$time[mdataMedian$evid==0] <- med2$time[match(mdataMedian$nbin[mdataMedian$evid==0],med2$bin)]
  covCols <- which(names(mdataMedian) %in% binCov)
  if(length(covCols)>0) mdataMedian[,covCols] <- med1[match(mdataMedian$mbin,med1$bin),-c(1:2)]
  
  #write median file
  MedianDataFileName <- paste(substr(paste("m_",datafileName,sep=""),0,8),sep="")
  PMwriteMatrix(mdataMedian[,1:(ncol(mdataMedian)-2)],MedianDataFileName,override=T)
  
  #remove old files
  invisible(file.remove(Sys.glob("sim*.txt")))
  
  #get poppar and make one with zero covariance
  poppar=getName("final")
  popparZero <- poppar
  popparZero$popCov[popparZero$popCov!=0] <- 0
  #do the simulation for each subject using the median dose, median covariates and pop parameters
  if("seed" %in% names(argsSIM)){
    seed.start <- argsSIM$seed
    argsSIM[[which(names(argsSIM)=="seed")]] <- NULL
  } else {seed.start <- -17}
  set.seed(seed.start)
  if("nsim" %in% names(argsSIM)){
    nsim <- argsSIM$nsim
    argsSIM[[which(names(argsSIM)=="nsim")]] <- NULL
  } else {nsim <- 1000}
  argsSIM1 <- c(list(poppar=popparZero,data=MedianDataFileName,model=modelfile,nsim=1,
                     seed=runif(nsub,-100,100),obsNoise=rep(0,4),outname="simMed"),argsSIM)
  cat("Simulating outputs for each subject using population means...\n")
  flush.console()
  do.call("SIMrun",argsSIM1)
  
  #read and format the results of the simulation
  PRED_bin <- SIMparse("simMed*",combine=T,silent=T)
  
  #make tempDF subset of PMop for subject, time, obs, outeq, pop, icen predictions (PREDij)
  tempDF <- getName("op")
  tempDF <- tempDF[tempDF$pred.type=="pop" & tempDF$icen==icen,]
  if(!is.na(includeID[1])){
    tempDF <- tempDF[tempDF$id %in% includeID,]
  }
  if(!is.na(excludeID[1])){
    mdata <- mdata[!mdata$id %in% excludeID,]
  }
  #remove missing obs from tempDF
  #tempDF <- tempDF[!is.na(tempDF$obs),]
  if(tad){
    tempDF$time <- mdata2b #replace time with tad
  }
  
  #add PRED_bin to tempDF
  tempDF$PRED_bin <- PRED_bin$obs$out
  
  #add pcYij column to tempDF as obs * PREDbin/PREDij
  tempDF$pcObs <- tempDF$obs * tempDF$PRED_bin/tempDF$pred
  
  #take out observations at time 0 (from evid=4 events)
  #tempDF <- tempDF[tempDF$time>0,]
  #bin pcYij by time and add to tempDF
  tempDF$nbin <- mdata2$nbin[mdata2$evid==0] 
  tempDF$bintime <- med2$time[match(tempDF$nbin,med2$bin)]
  
  
 
  #Now, simulate using full pop model
  set.seed(seed.start)
  argsSIM2 <- c(list(poppar=poppar,data=datafileName,model=modelfile,nsim=nsim,
                     seed=runif(nsub,-100,100),obsNoise=rep(0,4),outname="full"),argsSIM)
  if(!is.na(includeID[1])){
    argsSIM2$include <- includeID
  }
  if(!is.na(excludeID[1])){
    argsSIM2$exclude <- excludeID
  }
  do.call("SIMrun",argsSIM2)
  #read and format the results of the simulation
  simFull <- SIMparse("full*",combine=T,silent=T)
  #filter outeq
  simFull$obs <- simFull$obs[simFull$obs$outeq==outeq,]
  #take out observations at time 0
  simFull$obs <- simFull$obs[simFull$obs$time>0,]
  #pull in time bins from tempDF
  simFull$obs$nbin <- unlist(tapply(tempDF$nbin,tempDF$id,function(x) rep(x,nsim)))
  
  
  #make simulation number 1:nsim
  simFull$obs$simnum <- as.numeric(sapply(strsplit(simFull$obs$id,"\\."), function(x) x[1]))
  
  
  # NPDE --------------------------------------------------------------------
  
  #prepare data for npde
  obs <- tempDF[,c("id","time","obs")]
  #remove missing obs
  obs <- obs[obs$obs!=-99,]
  names(obs)[3] <- "out"
  
  simobs <- simFull$obs
  #remove missing simulations
  simobs <- simobs[simobs$out!=-99,]
  simobs$id <- rep(obs$id,each=nsim)
  
  #get NPDE
  assign("thisobs",obs,pos=1)
  assign("thissim",simobs,pos=1)
  npdeRes <- tryCatch(autonpde(namobs=thisobs,namsim=thissim,1,2,3,verbose=T),error=function(e) e)
  
  class(simFull) <- c("PMsim","list")
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
                  sim=simFull)
  save(NPAGout,file="../outputs/NPAGout.Rdata")
  
  #put sim in global environment
  assign(paste("sim",as.character(run),sep="."),simFull,pos=1)
  
  
  # Clean Up ----------------------------------------------------------------
  
  setwd(currwd)
  
  valRes <- list(simdata=simFull,bintimes=med2,opDF=tempDF,npde=npdeRes)
  class(valRes) <- c("PMvalid","list")
  
  return(valRes)
  
  
  
} #end function




