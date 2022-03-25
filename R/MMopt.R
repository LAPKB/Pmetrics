#' Computes 1 to 4 MM-optimal sampling times.
#'
#' Based on the mulitple-model optimization algorithm developed by David Bayard and presented at the 2012 American
#' College of Clinical Pharmacology Meeting and the 2013 International Association of Therapeutic
#' Drug Monitoring and Clinical Toxicology meeting.  A manuscript is in preparation.
#' 
#' @title Compute MM-optimal Sample Times
#' 
#' @param poppar An object of class \emph{PMfinal} (see \code{\link{makeFinal}})
#' @param model Name of a suitable model file template in the working directory.
#' The default is \dQuote{model.txt}.  This file will be converted to a fortran model file.
#' If it is detected to already be a fortran file, then the simulation will proceed without any further
#' file conversion.
#' @param data Either a PMmatrix object previously loaded with (\code{\link{PMreadMatrix}}) or character vector with the filename of a Pmetrics matrix file
#' that contains template regimens and observation times.  The value for outputs can be coded as
#' any number(s) other than -99.  The number(s) will be replaced in the simulator output with the simulated values.
#' @param nsamp The number of MM-optimal sample times to compute; default is 1, but can be up to 4.  Values >4 will be capped at 4.
#' @param weight List whose names indicate the type of weighting, and values indicate the
#' relative weight. Values should sum to 1.  Names can be any of the following:
#' \itemize{
#' \item \code{none} The default. MMopt times will be chosen to maximally discriminate all responses at all times.
#' \item \code{AUC} MMopt times will be chosen to maximally discriminate AUC, regardless of the shape of the response profile. 
#' \item \code{max} MMopt times will be chosen to maximally discriminate maximum, regardless of the shape of the response profile.
#' \item \code{min} MMopt times will be chosen to maximally discriminate minimum, regardless of the shape of the response profile. 
#' }
#' Any combination of AUC, max, and min can be chosen.  If \dQuote{none} is specified, other
#' weight types will be ignored and the relative value will be set to 1.
#' For example,\code{list(auc=0.5,max=0.5)} or \code{auc=0.2,min=0.8}.
#' @param predInt The interval in fractional hours for simulated predicted outputs at times other than those specified in the template \code{data}.  
#' The default is 0.5, which means there will be simulated outputs every 30 minutes from time 0 up 
#' to the maximal time in the template file.  You may also specify \code{predInt}
#' as a vector of 3 values, e.g. \code{c(1,4,1)}, similar to the R command \code{\link{seq}}, where the
#' first value is the start time, the second is the stop time, and the third is the
#' step value.  Outputs for times specified in the template file will also be simulated.
#' To simulate outputs \emph{only} at the output times in the template data (i.e. EVID=0 events), use \code{predInt=0}.
#' Note that the maximum number of predictions total is 594, so the interval must be sufficiently large to accommodate this for a given
#' number of output equations and total time to simulate over.  If \code{predInt} is set so that this cap is exceeded, predictions will be truncated.
#' @param outeq Output equation to optimize
#' @param \dots Other parameters to pass to \code{\link{SIMrun}}, which are not usually necessary.
#' @return A object of class \emph{MMopt} with 3 items.
#' \item{sampleTime }{The MM-optimal sample times}
#' \item{bayesRisk }{The Bayesian risk of mis-classifying a subject based on the sample times.  This
#' is more useful for comparisons between sampling strategies, with minimization the goal.}
#' \item{simdata }{A \emph{PMsim} object with the simulated profiles}
#' @author Michael Neely
#' @seealso \code{\link{SIMrun}}, \code{\link{plot.MMopt}}, \code{\link{print.MMopt}}
#' @export


MMopt <- function(poppar,model="model.txt",data="data.csv",nsamp=1,weight=list(none=1),predInt=0.5,outeq=1,...){
  
  #remove prior simulations if they exist
  old <- Sys.glob("MMsim*.txt")
  invisible(file.remove(old))
  popPoints <- poppar$popPoints
  #if(nsamp>4) nsamp <- 4
  #simulate each point
  SIMrun(poppar=poppar,model=model,data=data,nsim=0,predInt=predInt,obsNoise=NA,outname="MMsim",silent=T,...)
  #parse the simulated output
  simdata <- SIMparse("MMsim*.txt",combine=T,silent=T)
  simdata$obs <- simdata$obs[simdata$obs$outeq==outeq,]
  #transform into format for MMopt
  #nsubs is the number of subjects
  nsubs <- length(unique(simdata$obs$id))
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
  
  
  
  mmopt <- list(sampleTime=optsamp[1:nsamp,nsamp],
                bayesRisk=brisk[nsamp],Cbar=Cbar,
                simdata=simdata)
  class(mmopt) <- c("MMopt","list")
  return(mmopt)
  
  #   # display Results
  #   nout<-dim(Mu)[1];
  #   nsubs<-dim(Mu)[2];
  #   print('---------------------------')
  #   print('Background')
  #   print('Assay Polynomial sigma= c0 + c1*y + c2*y^2 + c3*y^3')
  #   print('[c0,c1,c2,c3]')
  #   print(cassay)
  #   #print('Bayesian Prior: Uniform')
  #   print('---------------------------')
  #   print('Optimal Designs')
  #   print('One Sample Design (hr)')
  #   print(optsamp[1,1])
  #   print('Two Sample Design (hr)')
  #   print(optsamp[1:2,2])
  #   print('Three Sample Design (hr)')
  #   print(optsamp[1:3,3])
  #   print('Four Sample Design (hr)')
  #   print(optsamp[1:4,4])
  #   print('Bayes Risk Overbound')
  #   #print(brisk)
  #   print(paste('1-Sample:   ', as.character(brisk[1])))
  #   print(paste('2-Sample:   ', as.character(brisk[2])))
  #   print(paste('3-Sample:   ', as.character(brisk[3])))
  #   print(paste('4-Sample:   ', as.character(brisk[4])))
  #   # --------------------------------------
  #   # Plot All Responses
  #   timediv=time%*%matrix(1,1,nsubs);
  #   dev.set(1)
  #   matplot(timediv,Mu,type = 'l',main='All Model Responses',xlab='hr',ylab='Conc',lty=1)
  #   grid()
  #   # --------------------------------------
  #   # plot Bayesian Prior Weights
  #   dev.set(2)
  #   matplot(pH,pch=4,type="b",main='Bayesian Prior Weights',xlab= 'Model Index',ylab='Prob',lty=1)
  #   grid()
  #   # --------------------------------------
  #   # Plot Results with 3-sample MMopt design
  #   timediv=time%*%matrix(1,1,nsubs);
  #   dev.set(3)
  #   matplot(timediv,Mu,type = 'l',main='All Model Responses with 3-Sample Design=(triangle,square,o)',xlab='hr',ylab='Conc',lty=1)
  #   vtime1<-matrix(time[optindex[1,3]],1,nsubs)
  #   points(vtime1,Mu[optindex[1,3], ],pch=6)
  #   vtime2<-matrix(time[optindex[2,3]],1,nsubs)
  #   points(vtime2,Mu[optindex[2,3],],pch=5)
  #   vtime3<-matrix(time[optindex[3,3]],1,nsubs)
  #   points(vtime3,Mu[optindex[3,3],],pch=1)
  #   grid()
  
}
#poppar,model="model.txt",data="data.csv",nsamp=1,weight=list(none=1),predInt=0.5,outeq=1,...
MM_opt <- function(poppar,...){
  is_res <- F
  if (inherits(poppar, "PM_result")) {
    is_res <- T
    res <- poppar
    poppar <- poppar$final
  }
  dots <- list(...)
  if (exists("model", where = dots)) {
    model <- dots$model
  } else {
    if (is_res) {
      model <- "simmodel.txt"
      res$model$write_model_file(model)
    } else {
      model <- "model.txt"
    }
  }
  if (exists("data", where = dots)) {
    data <- "simdata.csv"
    data_obj <- PM_data$new(dots$data)
    data_obj$write(data)
  } else {
    if (is_res) {
      data <- "simdata.csv"
      res$data$write(data)
    } else {
      data <- "data.csv"
    }
  }
  if (exists("nsamp", where = dots)) {
    nsamp <- dots$nsamp
  } else {
    nsamp <- 1
  }
  if (exists("weight", where = dots)) {
    weight <- dots$weight
  } else {
    weight <- list(none=1)
  }
  if (exists("predInt", where = dots)) {
    predInt <- dots$predInt
  } else {
    predInt <- 0.5
  }
  if (exists("outeq", where = dots)) {
    outeq <- dots$outeq
  } else {
    outeq <- 1
  }
  #remove prior simulations if they exist
  old <- Sys.glob("MMsim*.txt")
  invisible(file.remove(old))
  popPoints <- poppar$popPoints
  #if(nsamp>4) nsamp <- 4
  #simulate each point
  SIMrun(poppar=poppar,model=model,data=data,nsim=0,predInt=predInt,obsNoise=NA,outname="MMsim",silent=T,...)
  #parse the simulated output
  simdata <- SIMparse("MMsim*.txt",combine=T,silent=T)
  simdata$obs <- simdata$obs[simdata$obs$outeq==outeq,]
  #transform into format for MMopt
  #nsubs is the number of subjects
  nsubs <- length(unique(simdata$obs$id))
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
  
  
  
  mmopt <- list(sampleTime=optsamp[1:nsamp,nsamp],
                bayesRisk=brisk[nsamp],Cbar=Cbar,
                simdata=simdata)
  class(mmopt) <- c("MMopt","list")
  return(mmopt)
  
  #   # display Results
  #   nout<-dim(Mu)[1];
  #   nsubs<-dim(Mu)[2];
  #   print('---------------------------')
  #   print('Background')
  #   print('Assay Polynomial sigma= c0 + c1*y + c2*y^2 + c3*y^3')
  #   print('[c0,c1,c2,c3]')
  #   print(cassay)
  #   #print('Bayesian Prior: Uniform')
  #   print('---------------------------')
  #   print('Optimal Designs')
  #   print('One Sample Design (hr)')
  #   print(optsamp[1,1])
  #   print('Two Sample Design (hr)')
  #   print(optsamp[1:2,2])
  #   print('Three Sample Design (hr)')
  #   print(optsamp[1:3,3])
  #   print('Four Sample Design (hr)')
  #   print(optsamp[1:4,4])
  #   print('Bayes Risk Overbound')
  #   #print(brisk)
  #   print(paste('1-Sample:   ', as.character(brisk[1])))
  #   print(paste('2-Sample:   ', as.character(brisk[2])))
  #   print(paste('3-Sample:   ', as.character(brisk[3])))
  #   print(paste('4-Sample:   ', as.character(brisk[4])))
  #   # --------------------------------------
  #   # Plot All Responses
  #   timediv=time%*%matrix(1,1,nsubs);
  #   dev.set(1)
  #   matplot(timediv,Mu,type = 'l',main='All Model Responses',xlab='hr',ylab='Conc',lty=1)
  #   grid()
  #   # --------------------------------------
  #   # plot Bayesian Prior Weights
  #   dev.set(2)
  #   matplot(pH,pch=4,type="b",main='Bayesian Prior Weights',xlab= 'Model Index',ylab='Prob',lty=1)
  #   grid()
  #   # --------------------------------------
  #   # Plot Results with 3-sample MMopt design
  #   timediv=time%*%matrix(1,1,nsubs);
  #   dev.set(3)
  #   matplot(timediv,Mu,type = 'l',main='All Model Responses with 3-Sample Design=(triangle,square,o)',xlab='hr',ylab='Conc',lty=1)
  #   vtime1<-matrix(time[optindex[1,3]],1,nsubs)
  #   points(vtime1,Mu[optindex[1,3], ],pch=6)
  #   vtime2<-matrix(time[optindex[2,3]],1,nsubs)
  #   points(vtime2,Mu[optindex[2,3],],pch=5)
  #   vtime3<-matrix(time[optindex[3,3]],1,nsubs)
  #   points(vtime3,Mu[optindex[3,3],],pch=1)
  #   grid()
  
}