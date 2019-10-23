#' Use simulations to run model diagnostic tests.
#'
#' This function will generate normalized predictive discrepancies  (pd) which have also been called 
#' standardized visual predictive checks (SVPC).
#' Output from NPAG or IT2B will be used as the population model supplied to the
#' simulator.  In the working directory must be the same data .csv file and Fortran model file used in the NPAG/IT2B run.
#' The function will iterate through the .csv file, using each subject as a template to simulate 1000 new individuals.  The mean
#' population values will be used for each parameter and the covariance matrix.  In the case of a prior from NPAG, each point
#' will serve as a distribution within the multi-modal, multi-variate normal distribution, weighted according to the probability
#' of the point.  The population covariance matrix will be divided by the number of support points.  For a prior from IT2B,
#' only a uni-modal, multi-variate distribution is possible.  Errors may arise if extreme or negative concentrations are simulated
#' from excessively large covariance matrices.
#'
#' @title Simulation-based model diagnostics
#'
#' @param data Either a PMmatrix object previously loaded with (\code{\link{PMreadMatrix}}) or character vector with the filename of the Pmetrics matrix file
#' that contains the data for the model that you wish to analyze
#' @param model The name of the template model file. Default is \dQuote{model.for}.
#' @param poppar An object of class \emph{PMfinal} (see \code{\link{makeFinal}}) which serves as the prior for the simulations.
#' @param limits A matrix of limits for simulated values for each parameter.  If specified, it should be a matrix with number of rows equal to the number
#' of random paramters and 2 columns, corresponding to the minimum and maximum values.  For example, matrix(c(0,5,0,5,0.01,100),nrow=3,ncol=2,byrow=T) for 3 parameters
#' with limits of [0,5], [0,5] and [0.01,100], respectively.  If no limits are desired, this argument can be missing.  If limits are specified, each simulated parameter
#' set that contains a value outside of the limits will be ignored and another set will be generated.  Means and covariances of the total number of simulated sets
#' will be returned to verify the simulation, but only those sets within the specified limits will be used to generate output(s) and the means and covariances of the
#' retained sets may (and likely will be) different than those specified by \code{poppar}.
#' @param outeq The number of the output equation to simulate
#' @param output The filename to save the diagnostic results. If missing, will default to \dQuote{simcheck.Rdata}.
#' @return The output of \code{PMdiag} is a list with the following objects and
#'  of the class \emph{PMdiag}.
#'  \item{obsdat }{A data frame equivalent to a Pmetrics data .csv format, but with only observation rows (evid=0) and with the
#' addition of a \code{PIJ} column to indicate the percentile of each observation relative to simulated observations}
#'  \item{ypred }{The empirical mean of the simulated predicted distribution for each observation}
#'  \item{xerr }{An integer valued 0 if no error occurred during the computation or a positive number (1 or 2) depending on the error}
#'  \item{pd }{The normalised prediction discrepancies}
#' @author Michael Neely
#' @seealso \code{\link{SIMrun}}
#' @references 
#' Brendel K, Comets E, Laffont CM, Laveille C, Mentre F (2006) Metrics for external model evaluation with an application to the population pharmacokinetics of gliclazide. \emph{Pharmaceutical Research}, 23:2036-49
#' 
#' Mentre F, Escolano S (2006) Prediction discrepancies for the evaluation of nonlinear mixed-effects models. \emph{J Pharmacokinet Pharmacodyn}, 33:345-67
#'
#' Wang DD, Zhang S. (2011) Standardized Visual Predictive Check Versus Visual Predictive Check for Model Evaluation. \emph{J Clin Pharmacol}, epub ahead of print


PMdiag <- function(data,model="model.txt",poppar,limits,outeq=1,output="simcheck.Rdata"){
  
  ########## define internal functions
  
  #functions for pd/npde
  
  computepd <- function (tabobs, tabsim, calc.npde) 
  {
    ypredall <- c()
    pd <- c()
    nobs <- dim(tabobs)[1]
    nrep <- dim(tabsim)[1]/nobs
    for (isuj in unique(tabobs$id)) {
      matsim <- matrix(tabsim$ysim[tabsim$idsim == isuj], ncol = nrep)
      tcomp <- apply(matsim, 2, "<", tabobs$out[tabobs$id == 
        isuj])
      if (!is.matrix(tcomp)) 
        tcomp <- t(as.matrix(tcomp))
      ycal <- apply(tcomp, 1, mean)
      ycal[ycal == 0] <- 1/nrep
      ycal[ycal == 1] <- 1 - 1/nrep
      pd <- c(pd, ycal)
      if (!calc.npde) {
        ypred <- apply(matsim, 1, mean)
        ypredall <- c(ypredall, ypred)
      }
    }
    if (calc.npde) 
      ypredall <- NA
    return(list(pd = pd, ypred = ypredall))
  }  
  computenpde <- function (tabobs, tabsim) 
  {
    nobs <- nrow(tabobs)
    nrep <- nrow(tabsim)/nobs
    ypred <- ydobs <- pde <- tabobs$out
    ydsim <- tabsim$ysim
    for (isuj in unique(tabobs$id)) {
      msuj <- tabobs[tabobs$id == isuj, ]
      matsim <- matrix(tabsim$ysim[tabsim$idsim == isuj], ncol = nrep)
      x <- calcnpde(isuj, msuj, matsim, nrep)
      xerr <- x[[1]]
      pde[tabobs$id == isuj] <- x[[2]]
      ydsim[tabsim$idsim == isuj] <- x[[3]]
      ydobs[tabobs$id == isuj] <- x[[4]]
      ypred[tabobs$id == isuj] <- x[[5]]
      if (xerr > 0) {
        cat("The computation of the pde has failed for subject", 
            isuj, "because \n")
        if (xerr == 1) 
          cat("the Cholesky decomposition of the covariance matrix of the simulated data could not be obtained.\n")
        if (xerr == 2) 
          cat("the covariance matrix of the simulated data could not be inverted.\n")
        cat("This usually means that the covariance matrix is not positive-definite.\n")
        cat("This can be caused by simulations widely different from observations (in \n")
        cat("other words, a poor model).\n")
        cat("We suggest to plot a prediction interval from the simulated data to check\n")
        cat("whether the simulations are reasonable, and to consider prediction\n")
        cat("discrepancies.\n")
        cat("Prediction discrepancies will now be computed.\n")
        break
      }
    }
    if(xerr == 0){npde <- qnorm(pde)}
    else {npde <- rep(NA, length(pde))}
    return(list(ydsim = ydsim, ydobs = ydobs, xerr = xerr, npde = npde, 
                ypred = ypred))
  }
  
  calcnpde <- function (isuj, msuj, matsim, nrep) 
  {
    ypred <- apply(matsim, 1, mean)
    varsim <- cov(t(matsim))
    moysim <- apply(matsim, 1, mean)
    xerr <- 0
    xmat <- try(chol(varsim))
    if (is.numeric(xmat)) {
      ymat <- try(solve(xmat))
      if (!is.numeric(ymat)) 
        xerr <- 2
    }
    else xerr <- 1
    if (xerr == 0) {
      decsim <- t(ymat) %*% (matsim - moysim)
      decobs <- t(ymat) %*% (msuj$out - moysim)
      ydsim <- c(decsim)
      ydobs <- decobs
      tcomp <- apply(decsim, 2, "<", decobs)
      if (!is.matrix(tcomp)) 
        tcomp <- t(as.matrix(tcomp))
      ycal <- apply(tcomp, 1, mean)
      ycal[ycal == 0] <- 1/nrep
      ycal[ycal == 1] <- 1 - 1/nrep
      pde <- ycal
    }
    return(list(xerr, pde, ydsim, ydobs, ypred))
  }
  
  
  #for SVPC
  
  SVPC <- function(ori, sim){
    
    n.of.sim <- length(sim$idsim)/length(ori$id)
    
    sim$out <- rep(ori$out, n.of.sim)
    sim$ind <- rep(0, length(sim$idsim))
    sim[sim$ysim <= sim$out, "ind"] <- 1
    
    rank <- tapply(sim$ind, list(sim$idsim, sim$xsim), sum)
    
    Pij <- c()
    for (i in 1:nrow(rank)){
      Pij <- c(Pij, rank[i,!is.na(rank[i,])])
    }
    
    return (Pij/n.of.sim)
  }    
  
  #################################
  #main body of function
  calc.npde <- F  #disable npde for now
  calc.pd <- T #only do a pd (SVPC)
  nsim=1000
  #get and format data appropriately
  if(!inherits(data,"PMmatrix")) data <- PMreadMatrix(data,quiet=T)
  nsub <- length(unique(data$id))
  
  #do the simulation
  set.seed(-17)
  seed <- runif(nsub,-100,100)
  if(missing(limits)){
    SIMrun(poppar=poppar,data=data,model=model,split=T,nsim=nsim,predInt=0,seed=seed,outname="simout")
  } else {
    SIMrun(poppar=poppar,limits=limits,data=data,model=model,split=T,nsim=nsim,predInt=0,seed=seed,outname="simout")
  }
  #read and format the results of the simulation
  simdata <- data.frame()
  cat("\nProcessing simulated data\n")
  flush.console()
  pb <- txtProgressBar(min = 0, max = nsub, style = 3)

  for(i in 1:nsub){
    setTxtProgressBar(pb, i)
    temp <- SIMparse(paste("simout",i,".txt",sep=""),silent=T)
    times <- as.numeric(dimnames(temp$obs)$time)
    temp <- matrix(temp$obs[outeq,,],nrow=nsim)
    temp2 <- data.frame(id=rep(i,nrow(temp)*ncol(temp)),time=rep(times,nsim),simobs=c(t(temp)),simnum=rep(1:nsim,each=dim(temp)[2]))
    simdata <- rbind(simdata,temp2)
  }
  close(pb)
  simdata <- simdata[order(simdata$simnum,simdata$id,simdata$time),]
  simdata$simobs[simdata$simobs<0] <- 0
  simdata <- simdata[simdata$time>0,]
  tabsim <- data.frame(idsim=simdata$id, irsim=simdata$simnum, xsim=simdata$time, ysim=simdata$simobs)
  
  tabobs <- data[data$evid==0,]
  a=table(tabobs$id)
  b=a[match(tabobs$id,names(a))]
  c=b[unique(names(b))]
  tabobs$id <- rep(1:length(unique(tabobs$id)),times=c)
  
  
  #do the standardized visual predictive check - dosabled in favor of pd
  #    tabobs$PIJ = SVPC(tabobs, tabsim)
  
  #set up objects
  xdum <- rep(NA, length(tabobs$id))
  #    xret <- list(obsdat = tabobs, ydobs = xdum, ydsim = xdum, 
  #    ypred = xdum, xerr = NA, npde = NA, pd = NA)
  xret <- list(obsdat = tabobs, ypred = xdum, xerr = NA, pd = NA)
  if(calc.npde){    #always false for now
    x <- computenpde(tabobs, tabsim)
    if (x$xerr == 0) {
      xret$ydobs <- x$ydobs
      xret$ydsim <- x$ydsim
      xret$xerr <- x$xerr
      xret$npde <- x$npde
      xret$ypred <- x$ypred  
    }
  }
  if(calc.pd){  #always true for now
    xpd <- computepd(tabobs, tabsim, calc.npde)
    if (calc.npde) 
      ypred <- x$ypred
    else ypred <- xpd$ypred
    xret$pd <- xpd$pd
    if (!calc.npde) 
      xret$ypred <- xpd$ypred    
  }
  class(xret) <- c("PMdiag","list")
  saveRDS(xret,file=output) 
  return(xret)
  
}


