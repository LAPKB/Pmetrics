#' Computes 1 to 4 MM-optimal sampling times.
#'
#' Based on the mulitple-model optimization algorithm.
#' Bayard, David S., and Michael Neely. "Experiment Design for Nonparametric 
#' Models Based on Minimizing Bayes Risk: Application to Voriconazole." 
#' Journal of Pharmacokinetics and Pharmacodynamics 44, no. 2 (April 2017): 
#' 95–111. https://doi.org/10.1007/s10928-016-9498-5.
#' 
#' @title Compute MM-optimal Sample Times
#' 
#' @param poppar An object of class *PMfinal* (see [makeFinal])
#' @param model Name of a suitable model file template in the working directory.
#' The default is "model.txt".  This file will be converted to a fortran model file.
#' If it is detected to already be a fortran file, then the simulation will proceed without any further file conversion.
#' @param data Either a [PM_data] object or character vector with the filename 
#' of a Pmetrics data file
#' that contains template regimens and observation times.  The value for outputs 
#' can be coded as any number(s) other than -99.  The number(s) will be replaced in the simulator output with the simulated values.
#' @param nsamp The number of MM-optimal sample times to compute; default is 1, but can be up to 4.  Values >4 will be capped at 4.
#' @param weight List whose names indicate the type of weighting, and values indicate the relative weight. Values should sum to 1.  Names can be any of the following:
#' \itemize{
#' \item `none` The default. MMopt times will be chosen to maximally discriminate all responses at all times.
#' \item `AUC` MMopt times will be chosen to maximally discriminate AUC, regardless of the shape of the response profile. 
#' \item `max` MMopt times will be chosen to maximally discriminate maximum, regardless of the shape of the response profile.
#' \item `min` MMopt times will be chosen to maximally discriminate minimum, regardless of the shape of the response profile. 
#' }
#' Any combination of AUC, max, and min can be chosen.  If "none" is specified, other
#' weight types will be ignored and the relative value will be set to 1.
#' For example,\code{list(auc=0.5,max=0.5)} or \code{auc=0.2,min=0.8}.
#' @param predInt The interval in fractional hours for simulated predicted outputs at times other than those specified in the template `data`.  
#' The default is 0.5, which means there will be simulated outputs every 30 minutes from time 0 up 
#' to the maximal time in the template file.  You may also specify `predInt`
#' as a vector of 3 values, e.g. \code{c(1,4,1)}, similar to the R command [seq], where the
#' first value is the start time, the second is the stop time, and the third is the
#' step value.  Outputs for times specified in the template file will also be simulated.
#' To simulate outputs *only* at the output times in the template data (i.e. EVID=0 events), use \code{predInt=0}.
#' Note that the maximum number of predictions total is 594, so the interval must be sufficiently large to accommodate this for a given
#' number of output equations and total time to simulate over.  If `predInt` is set so that this cap is exceeded, predictions will be truncated.
#' @param mmInt Specify the time intervals from which MMopt times can be selected.
#' These should only include simulated times specified by `predInt`.
#' @param outeq Output equation to optimize
#' @param \dots Other parameters to pass to [SIMrun], which are not usually necessary.
#' @return A object of class *MMopt* with 3 items.
#' \item{sampleTime }{The MM-optimal sample times}
#' \item{bayesRisk }{The Bayesian risk of mis-classifying a subject based on the sample times.  This
#' is more useful for comparisons between sampling strategies, with minimization the goal.}
#' \item{simdata }{A *PMsim* object with the simulated profiles}
#' \item{mmInt }{A list with the `mmInt` values, `NULL` if `mmInt` is missing.}
#' @author Michael Neely
#' @seealso [SIMrun], [plot.MMopt], [print.MMopt]
#' @export


MM_opt <- function(poppar, model, data, nsamp = 1, weight = list(none = 1), 
                   predInt = 0.5, mmInt, outeq = 1, ...){
  
  if(inherits(poppar,"PM_result")){
    if(!inherits(poppar$final,"NPAG")) {stop("Prior must be NPAG result.")}
    popPoints <- poppar$final$popPoints
  } else {
    if(!inherits(poppar,"NPAG")) {stop("Prior must be NPAG result.")}
    popPoints <- poppar$popPoints
    if(missing(model)){model <- "model.txt"}
    if(missing(data)){data <- "data.txt"}
    
  }
  #remove prior simulations if they exist
  old <- Sys.glob("MMsim*.txt")
  invisible(file.remove(old))
  #if(nsamp>4) nsamp <- 4
  #simulate each point
  SIMrun(poppar=poppar,model=model,data=data,nsim=0,
         predInt=predInt,obsNoise=NA,outname="MMsim",quiet=T,...)
  #parse the simulated output
  simdata <- SIMparse("MMsim*.txt",combine=T,quiet=T)
  simdata$obs <- simdata$obs[simdata$obs$outeq==outeq,]
  #transform into format for MMopt
  #nsubs is the number of subjects
  nsubs <- length(unique(simdata$obs$id))
  #parse mmInt
  if(!missing(mmInt)){
    simdata_full <- simdata
    if(!inherits(mmInt,"list")){ 
      mmInt <- list(mmInt)
    } #mmInt was a single vector; make a list of 1
    simdata$obs <- purrr::map_df(mmInt, function(x){
      filter(simdata$obs, time >= x[1] & time <= x[2])
    }) %>% arrange(id, time)
  } else {
    mmInt <- NULL
    simdata_full <- simdata
    }
  
  #time is the simulated times
  time <- unique(simdata$obs$time) 
  #nout is the number of simulated times (outputs)
  nout <- length(time)
  #Mu is a matrix of nout rows x nsubs columns containing the outputs at each time
  Mu <- t(matrix(simdata$obs$out,nrow=nsubs,byrow=T))
  
  #pH is the vector of probabilities of each population point
  pH <- popPoints[,ncol(popPoints)]
  #replicate pH and normalize based on number of simulation templates
  ntemp <- nsubs/nrow(popPoints)
  pH <- rep(pH,ntemp)
  pH <- pH/ntemp
  numeqt <- max(simdata$obs$outeq)
  #get the assay error from the simulated output
  simout <- readLines("MMsim1.txt")
  errLine <- grep(" EQUATIONS, IN ORDER, WERE:",simout)
  cassay <- scan("MMsim1.txt",n=4,skip=errLine+numeqt-1,quiet=T)
  
  #make the weighting Matrix
  wtnames <- names(weight)
  Cbar0 <- array(NA,dim=c(nsubs,nsubs,4),
                 dimnames=list(a=1:nsubs,b=1:nsubs,type=c("none","auc","cmax","cmin")))
  
  #default is no penalites (diag=0, off-diag=1)
  if("none" %in% wtnames){
    Cbar0[,,1] <- matrix(1,nrow=nsubs,ncol=nsubs)
    diag(Cbar0[,,1]) <- 0
  } else {
    if(sum(unlist(weight))!=1){stop("Relative weights do not sum to 1.\n")} else {
      if("auc" %in% wtnames){
        auc <- makeAUC(simdata)
        sqdiff <- matrix(sapply(1:nsubs,function(x) (auc$tau[x] - auc$tau)^2),nrow=nsubs)
        cbar <- cbar_make1(sqdiff)
        Cbar0[,,2] <- weight$auc*cbar/mean(cbar)
      }
      
      if("max" %in% wtnames){
        maxi <- unlist(tapply(simdata$obs$out,simdata$obs$id,max))
        sqdiff <- matrix(sapply(1:nsubs,function(x) (maxi[x] - maxi)^2),nrow=nsubs)
        cbar <- cbar_make1(sqdiff)
        Cbar0[,,3] <- weight$max*cbar/mean(cbar)
      }
      
      if("min" %in% wtnames){
        mini <- unlist(tapply(simdata$obs$out,simdata$obs$id,min))
        sqdiff <- matrix(sapply(1:nsubs,function(x) (mini[x] - mini)^2),nrow=nsubs)
        cbar <- cbar_make1(sqdiff)
        Cbar0[,,4] <- weight$min*cbar/mean(cbar)
      }
      notWt <- which(!wtnames %in% c("auc","min","max","none"))
      if(length(notWt)>0){cat(paste("The following parameters are not valid weighting factors and were ignored: ",paste(wtnames[notWt],collapse=", "),".\n",sep=""))}
    }
    
  }
  #find max value over all selected weights (condense to nsubs x nsubs matrix)
  Cbar <- apply(Cbar0,c(1,2),max,na.rm=T)
  
  # Call MMMOPT1 routine to compute optimal sampling times
  mmopt1 <- wmmopt1(Mu,time,pH,cassay,nsamp,nsubs,nout,Cbar);
  optsamp <- mmopt1$optsamp
  brisk <- mmopt1$brisk_cob
  optindex <- mmopt1$optindex
  
  
  
  # ---------------------------
  
  
  
  mmopt <- list(sampleTime = optsamp[1:nsamp,nsamp],
                bayesRisk = brisk[nsamp], Cbar = Cbar,
                simdata = simdata_full, mmInt = mmInt)
  class(mmopt) <- c("MMopt","list")
  return(mmopt)
}