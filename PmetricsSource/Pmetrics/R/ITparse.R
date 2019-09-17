#' \code{ITparse} processes the output from an IT2B run into a list.
#'
#' This function can take some time to process the RFILE, depending on the number of subjects,
#'  doses, observations, etc.  Typical wait times are a few seconds up to 5 minutes.
#'  When processing is complete a summary of the extracted data will be reported on the console.
#'
#' @title Parse Pmetrics IT2B Output
#' @param outfile This is the filename of the output from IT2B. Typically,
#' the file will be called IT_RF0001.txt, and this is the default.
#' @return The output of \code{ITparse} is a list with the following objects and
#'  of the class \emph{IT2B}.
#'  \item{nsub }{Number of subjects}
#'  \item{nvar }{Number of random variables or parameters in the model}
#'  \item{nofix }{Number of fixed variables or parameters in the model}
#'  \item{par }{Names of random parameters}
#'  \item{parfix }{Names of fixed parameters}
#'  \item{covnames }{Names of covariates}
#'  \item{ab }{Suggested boundaries for each random parameter to be passed to NPAG}
#'  \item{fixedpos }{Index of variables fixed to be positive}
#'  \item{valfix }{Values for fixed parameters}
#'  \item{icycmax }{Maximum number of cycles specified by the user}
#'  \item{icyctot }{Number of cycles run.  If less than \code{icycmax}, convergence occurred.}
#'  \item{stoptol }{Stopping tolerance for convergence, default 0.001}
#'  \item{converge }{Boolean value if convergence occurred.}
#'  \item{ODEtol }{Ordindary Differential Equation solver tolerance.}
#'  \item{numeqt }{Number of output equations}
#'  \item{ERRmod }{Vector of length equal to \code{numeqt} whose values are 0 if gamma was estimated for that
#'   output equation or 1 if gamma was fixed to 1 for that output equation}
#'  \item{ndrug }{Number of drug inputs}
#'  \item{salt }{Vector of values of the salt fraction for each \code{ndrug}}
#'  \item{ndose }{Vector of the number of doses for each subject in the population}
#'  \item{ncov }{Number of covariates in the model}
#'  \item{nobs }{Vector of the number of observations for each subject in the population}
#'  \item{nobsmax }{Maximum number of observation in any individual subject}
#'  \item{ypredpop }{Array of population model predictions for each subject at each observation time point.
#'  \emph{ypredpop[nsub,numeqt,time,type]} where \emph{type} is 1=mean, 2=median of the population prior used to calculate ypredpop}
#'  \item{ypredbay }{Array of Bayesian posterior model predictions for each subject at each observation time point.
#'  \emph{ypredbay[nsub,numeqt,time,type]} where \emph{type} is 1=mean, 2=median of the population prior used to calculate ypredbay}
#'  \item{parbay }{Array of Bayesian posterior parameter estimates for each subject,
#'  \emph{parbay[nsub,nvar,type]} where \emph{type} is 1=mean, 2=median of the population prior used to calculate parbay}
#'  \item{ic }{Data frame with one row and two columns for final cycle Akaike Information Criterion (AIC) and Bayesian Information Criterion (BIC)}
#'  \item{ilog }{Vector of cycle number and associated log-likelihood}
#'  \item{imean }{Matrix of cycle numbers and associated means for each random parameter}
#'  \item{imed }{Matrix of cycle numbers and associated medians for each random parameter}
#'  \item{isd }{Matrix of cycle numbers and associated standard deviations for each random parameter}
#'  \item{icv }{Matrix of cycle numbers and associated coefficients of variation for each random parameter}
#'  \item{igamlam }{Matrix of cycle number and associated gamma or lambda with each output equation in a column}
#'  \item{lpar }{Matrix of subjects in rows and MAP Bayesian parameter estimates in columns for each parameter,
#'   based on population means from the next to last cycle.}
#'  \item{lsd }{Matrix of subjects in rows and SD of Bayesian posterior parameter distributions in columns for each parameter,
#'   based on population means from the next to last cycle.}
#'  \item{lcv }{Matrix of subjects in rows and CV of Bayesian posterior parameter distributions in columns for each parameter,
#'   based on population means from the next to last cycle.}
#'  \item{sdata }{Subject data consisting of 5 columns: [id,  nsub,  age,  sex,  ht],
#'  \emph{id} is the original identification number in the .csv matrix file;
#'  \emph{nsub} is the sequential subject number in the IT2B run; \emph{age},
#'  \emph{sex} and \emph{ht} will be missing for .csv input and present if included in .wrk input files}
#'  \item{dosecov }{Data frame with all dosing information for each subject,  including times,  routes,  amounts,  and associated covariate values}
#'  \item{outputs }{Data frame with measured outputs for each subject and associated assay error polynomials.
#'   The order of the columns is nsub, time, numeqt, observation, c0, c1, c2, c3, where the last
#'   four columns are the coefficients of the assay error polynomial for that observation, such that
#'   SD[obs] = c0 + c1*[obs] + c2*[obs]**2 + c3*[obs]**3}
#'  \item{negflag }{A flag indicating that some negative predictions were changed to missing.
#'  This means that the model may be misspecified.}
#'  \item{mdata }{The filename of the data used in the run.}
#' @author Michael Neely

ITparse <- function(outfile="IT_RF0001.TXT"){
  #require(utils)
  cat("\n\n\nParsing IT2B results...\n\n")
  flush.console()
  
  #PARSE DENSITY FILE
  #get data
  if (!file.exists(outfile)){stop(paste(outfile,"not found.\n",sep=" "))}
  
  setwd(dirname(outfile))
  
  negflag <- F
  RFver <- readLines(outfile,n=1)
  vernum <- switch(RFver,
                   " VERSION 1.1 - JAN 2011 "=1,
                   " VERSION 1.2 - APR 2011 "=2,
                   " VERSION 1.3 - JUL 2011 "=3,
                   " VERSION 1.4 - JUL 2012 "=3,
                   " VERSION 1.5 - MAR 2016 "=4,4)
  dimlines <- switch(vernum,6,6,9,10)
  ITdims <- scan(outfile,quiet=T,skip=3,nlines=dimlines,comment.char="#")
  
  
  
  
  #if version 1.2 or less
  if(vernum<3){
    #number of subjects
    nsub <- ITdims[1]
    #number of random parameters
    nvar <- ITdims[2]
    #number of fixed parameters
    nofix <- ITdims[3]
    #final cycle number
    icyctot <- ITdims[4]
    #number of output equations
    numeqt <- ITdims[5]
    #number of drugs
    ndrug <- ITdims[6]
    #number of dose events for each subjects
    ndose <- scan(outfile,quiet=T,skip=9,n=nsub,comment.char="#")
    #number of additional covariates
    ncov <- scan(outfile,quiet=T,skip=9+nsub,n=1,comment.char="#")
    #number of observations for each subject
    nobs <- scan(outfile,quiet=T,skip=10+nsub,n=nsub,comment.char="#")
    nobsmax <- max(nobs)
    #get table of contents
    toc <- scan(outfile,quiet=T,skip=12+2*nsub,n=21,comment.char="#")
    #get random parameter names
    par <- scan(outfile,what="character",n=nvar,skip=toc[1],quiet=T)
    #get fixed parameter names
    parfix <- if(nofix>0) scan(outfile,what="character",skip=toc[2],n=nofix,quiet=T)
    #get initial ranges for random parameters
    ab <- matrix(scan(outfile,n=nvar*3,skip=toc[3],quiet=T,what="character"),nrow=nvar,ncol=3,byrow=T)
    fixedpos <- which(ab[,3]=="NO")
    if(length(fixedpos)==0) fixedpos <- NULL
    ab <- matrix(as.numeric(ab[,1:2]),ncol=2)
    #get values for fixed parameters
    valfix <- if(nofix>0) scan(outfile,skip=toc[4],n=nofix,quiet=T)
    #get the covariate names
    covNameOffset <- switch(vernum,2,0,0)
    if (ncov>0){
      covnames <- scan(outfile,what="character",skip=toc[5],n=ncov+covNameOffset,quiet=T)
    } else {covnames <- NA}
    #get aic-bic
    ic <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[6],n=2,quiet=T,what=""))),nrow=1,ncol=2,byrow=T)
    #get population predictions at observation times
    temp1 <-as.numeric(sub("D","E",scan(outfile,skip=toc[7],n=numeqt*sum(nobs)*2,quiet=T,what="")))
    
    #get posterior predictions at observation times
    temp2 <-as.numeric(sub("D","E",scan(outfile,skip=toc[8],n=numeqt*sum(nobs)*2,quiet=T,what="")))
    
    #flag negative pop or post preds
    if (any(temp1<0,na.rm=T) | any(temp2<0,na.rm=T)){
      #    temp1[temp1<0] <- 0
      #    temp2[temp2<0] <- 0
      negflag <- T
    }
    
    ypredpop <- array(dim=c(nsub,numeqt,nobsmax,2))
    ypredbay <- array(dim=c(nsub,numeqt,nobsmax,2))
    count <- 1
    cat("\nPopulation and posterior predictions at each subject's observation times.\n")
    flush.console()
    pb <- txtProgressBar(min = 1, max = nsub, style = 3)
    for (jsub in 1:nsub){
      setTxtProgressBar(pb,jsub)
      for (ieq in 1:numeqt){
        for (iobs in 1:nobs[jsub]){
          for (icen in 1:2){
            ypredpop[jsub,ieq,iobs,icen]<-temp1[count]
            ypredbay[jsub,ieq,iobs,icen]<-temp2[count]
            count <- count+1
          }
        }
      }
    }
    count <- 1
    #get MAP bayesian parameter estimate
    temp <-as.numeric(sub("D","E",scan(outfile,skip=toc[9],n=nsub*2*nvar,quiet=T,what="")))
    parbay <- array(dim=c(nsub,nvar,2))
    count <- 1
    cat("\nBayesian posterior parameters.\n")
    flush.console()
    pb <- txtProgressBar(min = 1, max = nsub, style = 3)
    for (jsub in 1:nsub){
      setTxtProgressBar(pb,jsub)
      for (ivar in 1:nvar){
        for (icen in 1:2){
          parbay[jsub,ivar,icen]<-temp[count]
          count <- count+1
          
        }
      }
    }
    count <- 1
    
    #CYCLE INFORMATION
    cat("\nCycle information.\n")
    flush.console()
    if (icyctot > 0){
      #get log-likelihoods
      ilog <- as.numeric(sub("D","E",scan(outfile,skip=toc[10],n=icyctot,quiet=T,what="")))
      #get means
      imean <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[11],n=icyctot*nvar,quiet=T,what=""))),nrow=icyctot,ncol=nvar,byrow=T)
      dimnames(imean) <- list(cycle=1:icyctot,par=par)
      #get medians
      imed <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[12],n=icyctot*nvar,quiet=T,what=""))),nrow=icyctot,ncol=nvar,byrow=T)
      dimnames(imed) <- list(cycle=1:icyctot,par=par)
      #get SDs
      if(nsub>1) {
        isd <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[13],n=icyctot*nvar,quiet=T,what=""))),nrow=icyctot,ncol=nvar,byrow=T)
      } else { isd <- matrix(NA,nrow=icyctot,ncol=nvar)}
      dimnames(isd) <- list(cycle=1:icyctot,par=par)
      #get CVs
      if(nsub>1) {
        icv <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[14],n=icyctot*nvar,quiet=T,what=""))),nrow=icyctot,ncol=nvar,byrow=T)
      } else { icv <- matrix(NA,nrow=icyctot,ncol=nvar)}
      dimnames(icv) <- list(cycle=1:icyctot,par=par)
      
      #get gamlam values
      igamlam <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[15],n=numeqt*icyctot,quiet=T,what=""))),ncol=numeqt,byrow=T)
      
      #calculate cycle aic and bic
      if(all(diff(as.numeric(igamlam))==0)){q <- 0} else {q <- 1}
      aic <- -2*ilog + (nvar^2 + 3*nvar)/2 + q
      bic <- -2*ilog + 0.5*((nvar^2 + 3*nvar)/2 + q) * log(sum(nobs))
      iic <- data.frame(cbind(aic,bic))
      names(iic) <- c("AIC","BIC")
      
    } else {
      ilog <- NA
      iic <- NA
      imean <- NA
      isd <- NA
      icv <- NA
      igamlam <- NA
    }
    
    #LAST CYCLE
    #get par values
    lpar <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[16],n=nsub*nvar,quiet=T,what=""))),nrow=nsub,ncol=nvar,byrow=T)
    dimnames(lpar) <- list(nsub=1:nsub,par=par)
    #get par SDs
    lsd <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[17],n=nsub*nvar,quiet=T,what=""))),nrow=nsub,ncol=nvar,byrow=T)
    dimnames(lsd) <- list(nsub=1:nsub,par=par)
    #get par CVs
    lcv <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[18],n=nsub*nvar,quiet=T,what=""))),nrow=nsub,ncol=nvar,byrow=T)
    dimnames(lcv) <- list(nsub=1:nsub,par=par)
    
    #SUBJECT DATA
    cat("\nSubject and covariate data.\n")
    flush.console()
    sdata <- matrix(scan(outfile,skip=toc[19],n=nsub*5,quiet=T,what="character"),nrow=nsub,ncol=5,byrow=T)
    sdata <- data.frame(id=checkID(sdata[,2]),nsub=1:nsub,age=as.numeric(sdata[,3]),
                        sex=sdata[,4],ht=as.numeric(sdata[,5]))
    #dose covariates
    doseCovOffset <- switch(vernum,3,1,1)
    dosecov <- cbind(rep(1:nsub,ndose),matrix(as.numeric(sub("D","E",scan(outfile,skip=toc[20],n=sum(ndose)*(2*ndrug+doseCovOffset+ncov),quiet=T,what=""))),nrow=sum(ndose),byrow=T))
    
    #outputs
    outputs <- switch(vernum,
                      cbind(rep(1:nsub,nobs),matrix(as.numeric(sub("D","E",scan(outfile,skip=toc[21],n=sum(nobs)*(numeqt+1),quiet=T,what=""))),nrow=sum(nobs),byrow=T)),
                      matrix(as.numeric(sub("D","E",scan(outfile,skip=toc[21],n=sum(nobs)*(numeqt)*8,quiet=T,what=""))),ncol=8,byrow=T),
                      matrix(as.numeric(sub("D","E",scan(outfile,skip=toc[21],n=sum(nobs)*(numeqt)*8,quiet=T,what=""))),ncol=8,byrow=T))
    outputs <- data.frame(outputs)
    names(outputs) <- c("id","time","outeq","out","c0","c1","c2","c3")
    
    #data filename
    mdata <- scan(outfile,skip=toc[21]+sum(nobs)*numeqt,quiet=T,what="character",n=1)
    if(length(mdata)==0) mdata <- "NA"
    
    #summary
    cat("Output file:",outfile,"\n")
    cat("Random parameters:",par,"\n")
    if(nofix==0){cat("There were no fixed parameters.\n")
    } else {cat("Fixed parameters:",parfix,"\n")}
    cat("Number of analyzed subjects:",nsub,"\n")
    cat("Number of output equations:",numeqt,"\n")
    cat("Additional covariates:",covnames,"\n")
    if (negflag){ cat("WARNING: There were negative or non-real pop/post predictions.\n")}
    coninterp <- switch(1+converge,"The run did not converge before the last cycle.","The run converged.","","WARNING: The run ended with a Hessian Error.")
    cat(coninterp,"\n")
    
    outlist <- list(nsub=nsub,nvar=nvar,nofix=nofix,par=par,parfix=parfix,covnames=covnames,ab=ab,valfix=valfix,
                    icyctot=icyctot,numeqt=numeqt,ndrug=ndrug,ndose=ndose,ncov=ncov,
                    nobs=nobs,nobsmax=nobsmax,ypredpop=ypredpop,
                    ypredbay=ypredbay,parbay=parbay,ilog=ilog,iic=iic,
                    imean=imean,imed=imed,isd=isd,icv=icv,igamlam=igamlam,lpar=lpar,lsd=lsd,lcv=lcv,
                    sdata=sdata,dosecov=dosecov,outputs=outputs,negflag=negflag)
  } 
  
  if(vernum==3) { #versions 1.3 and 1.4
    #number of subjects
    nsub <- ITdims[1]
    #number of random parameters
    nvar <- ITdims[2]
    #number of fixed parameters
    nofix <- ITdims[3]
    #maximum cycles
    icycmax <- as.numeric(ITdims[4])
    #final cycle number
    icyctot <- as.numeric(ITdims[5])
    #stopping tolerance
    stoptol <- as.numeric(ITdims[6])
    #convergence
    converge <- as.numeric(ITdims[7])
    #ODE tolerance
    ODEtol <- as.numeric(ITdims[8])
    #number of output equations
    numeqt <- ITdims[9]
    #gammma estimated (0) or fixed (1) for each output equation
    ERRmod <- scan(outfile,quiet=T,skip=12,n=1,comment.char="#")
    #number of drugs
    ndrug <- scan(outfile,quiet=T,skip=12+numeqt,n=1,comment.char="#")
    #salt fraction for each drug
    salt <- scan(outfile,quiet=T,skip=13+numeqt,n=ndrug,comment.char="#")
    #number of dose events for each subjects
    ndose <- scan(outfile,quiet=T,skip=13+numeqt+ndrug,n=nsub,comment.char="#")
    #number of additional covariates
    ncov <- scan(outfile,quiet=T,skip=13+numeqt+ndrug+nsub,n=1,comment.char="#")
    #number of observations for each subject
    nobs <- scan(outfile,quiet=T,skip=14+numeqt+ndrug+nsub,n=nsub,comment.char="#")
    nobsmax <- max(nobs)
    
    #get table of contents
    toc <- scan(outfile,quiet=T,skip=16+numeqt+ndrug+2*nsub,n=21,comment.char="#") 
    
    #get random parameter names
    par <- scan(outfile,what="character",n=nvar,skip=toc[1],quiet=T)
    #get fixed parameter names
    parfix <- if(nofix>0) scan(outfile,what="character",skip=toc[2],n=nofix,quiet=T)
    #get initial ranges for random parameters
    ab <- matrix(scan(outfile,n=nvar*3,skip=toc[3],quiet=T,what="character"),nrow=nvar,ncol=3,byrow=T)
    fixedpos <- which(ab[,3]=="NO")
    if(length(fixedpos)==0) fixedpos <- NULL
    ab <- matrix(as.numeric(ab[,1:2]),ncol=2)
    #get values for fixed parameters
    valfix <- if(nofix>0) scan(outfile,skip=toc[4],n=nofix,quiet=T)
    #get the covariate names
    covNameOffset <- switch(vernum,2,0,0)
    if (ncov>0){
      covnames <- scan(outfile,what="character",skip=toc[5],n=ncov+covNameOffset,quiet=T)
    } else {covnames <- NA}
    #get aic-bic
    ic <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[6],n=2,quiet=T,what=""))),nrow=1,ncol=2,byrow=T)
    #get population predictions at observation times
    temp1 <-as.numeric(sub("D","E",scan(outfile,skip=toc[7],n=numeqt*sum(nobs)*2,quiet=T,what="")))
    
    #get posterior predictions at observation times
    temp2 <-as.numeric(sub("D","E",scan(outfile,skip=toc[8],n=numeqt*sum(nobs)*2,quiet=T,what="")))
    
    #flag negative pop or post preds
    if (any(temp1<0,na.rm=T) | any(temp2<0,na.rm=T)){
      #    temp1[temp1<0] <- 0
      #    temp2[temp2<0] <- 0
      negflag <- T
    }
    
    ypredpop <- array(dim=c(nsub,numeqt,nobsmax,2))
    ypredbay <- array(dim=c(nsub,numeqt,nobsmax,2))
    count <- 1
    cat("\nPopulation and posterior predictions at each subject's observation times.\n")
    flush.console()
    pb <- txtProgressBar(min = 1, max = nsub, style = 3)
    for (jsub in 1:nsub){
      setTxtProgressBar(pb,jsub)
      for (ieq in 1:numeqt){
        for (iobs in 1:nobs[jsub]){
          for (icen in 1:2){
            ypredpop[jsub,ieq,iobs,icen]<-temp1[count]
            ypredbay[jsub,ieq,iobs,icen]<-temp2[count]
            count <- count+1
          }
        }
      }
    }
    count <- 1
    #get MAP bayesian parameter estimate
    temp <-as.numeric(sub("D","E",scan(outfile,skip=toc[9],n=nsub*2*nvar,quiet=T,what="")))
    parbay <- array(dim=c(nsub,nvar,2))
    count <- 1
    cat("\nBayesian posterior parameters.\n")
    flush.console()
    pb <- txtProgressBar(min = 1, max = nsub, style = 3)
    for (jsub in 1:nsub){
      setTxtProgressBar(pb,jsub)
      for (ivar in 1:nvar){
        for (icen in 1:2){
          parbay[jsub,ivar,icen]<-temp[count]
          count <- count+1
          
        }
      }
    }
    count <- 1
    
    #CYCLE INFORMATION
    cat("\nCycle information.\n")
    flush.console()
    if (icyctot > 0){
      #get log-likelihoods
      ilog <- as.numeric(sub("D","E",scan(outfile,skip=toc[10],n=icyctot,quiet=T,what="")))
      #get means
      imean <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[11],n=icyctot*nvar,quiet=T,what=""))),nrow=icyctot,ncol=nvar,byrow=T)
      dimnames(imean) <- list(cycle=1:icyctot,par=par)
      #get medians
      imed <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[12],n=icyctot*nvar,quiet=T,what=""))),nrow=icyctot,ncol=nvar,byrow=T)
      dimnames(imed) <- list(cycle=1:icyctot,par=par)
      #get SDs
      if(nsub>1) {
        isd <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[13],n=icyctot*nvar,quiet=T,what=""))),nrow=icyctot,ncol=nvar,byrow=T)
      } else { isd <- matrix(NA,nrow=icyctot,ncol=nvar)}
      dimnames(isd) <- list(cycle=1:icyctot,par=par)
      #get CVs
      if(nsub>1) {
        icv <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[14],n=icyctot*nvar,quiet=T,what=""))),nrow=icyctot,ncol=nvar,byrow=T)
      } else { icv <- matrix(NA,nrow=icyctot,ncol=nvar)}
      dimnames(icv) <- list(cycle=1:icyctot,par=par)
      
      #get gamlam values
      igamlam <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[15],n=numeqt*icyctot,quiet=T,what=""))),ncol=numeqt,byrow=T)
      
      #calculate cycle aic and bic
      if(all(diff(as.numeric(igamlam))==0)){q <- 0} else {q <- 1}
      aic <- -2*ilog + (nvar^2 + 3*nvar)/2 + q
      bic <- -2*ilog + 0.5*((nvar^2 + 3*nvar)/2 + q) * log(sum(nobs))
      iic <- data.frame(cbind(aic,bic))
      names(iic) <- c("AIC","BIC")
      
    } else {
      ilog <- NA
      iic <- NA
      imean <- NA
      isd <- NA
      icv <- NA
      igamlam <- NA
    }
    
    #LAST CYCLE
    #get par values
    lpar <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[16],n=nsub*nvar,quiet=T,what=""))),nrow=nsub,ncol=nvar,byrow=T)
    dimnames(lpar) <- list(nsub=1:nsub,par=par)
    #get par SDs
    lsd <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[17],n=nsub*nvar,quiet=T,what=""))),nrow=nsub,ncol=nvar,byrow=T)
    dimnames(lsd) <- list(nsub=1:nsub,par=par)
    #get par CVs
    lcv <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[18],n=nsub*nvar,quiet=T,what=""))),nrow=nsub,ncol=nvar,byrow=T)
    dimnames(lcv) <- list(nsub=1:nsub,par=par)
    
    #SUBJECT DATA
    cat("\nSubject and covariate data.\n")
    flush.console()
    sdata <- matrix(scan(outfile,skip=toc[19],n=nsub*5,quiet=T,what="character"),nrow=nsub,ncol=5,byrow=T)
    sdata <- data.frame(id=checkID(sdata[,2]),nsub=1:nsub,age=as.numeric(sdata[,3]),
                        sex=sdata[,4],ht=as.numeric(sdata[,5]))
    #dose covariates
    doseCovOffset <- switch(vernum,3,1,1)
    dosecov <- cbind(rep(1:nsub,ndose),matrix(as.numeric(sub("D","E",scan(outfile,skip=toc[20],n=sum(ndose)*(2*ndrug+doseCovOffset+ncov),quiet=T,what=""))),nrow=sum(ndose),byrow=T))
    
    #outputs
    outputs <- switch(vernum,
                      cbind(rep(1:nsub,nobs),matrix(as.numeric(sub("D","E",scan(outfile,skip=toc[21],n=sum(nobs)*(numeqt+1),quiet=T,what=""))),nrow=sum(nobs),byrow=T)),
                      matrix(as.numeric(sub("D","E",scan(outfile,skip=toc[21],n=sum(nobs)*(numeqt)*8,quiet=T,what=""))),ncol=8,byrow=T),
                      matrix(as.numeric(sub("D","E",scan(outfile,skip=toc[21],n=sum(nobs)*(numeqt)*8,quiet=T,what=""))),ncol=8,byrow=T))
    outputs <- data.frame(outputs)
    names(outputs) <- c("id","time","outeq","out","c0","c1","c2","c3")
    
    #data filename
    mdata <- scan(outfile,skip=toc[21]+sum(nobs)*numeqt,quiet=T,what="character",n=1)
    if(length(mdata)==0) mdata <- "NA"
    
    #summary
    cat("Output file:",outfile,"\n")
    cat("Random parameters:",par,"\n")
    if(nofix==0){cat("There were no fixed parameters.\n")
    } else {cat("Fixed parameters:",parfix,"\n")}
    cat("Number of analyzed subjects:",nsub,"\n")
    cat("Number of output equations:",numeqt,"\n")
    cat("Additional covariates:",covnames,"\n")
    if (negflag){ cat("WARNING: There were negative or non-real pop/post predictions.\n")}
    coninterp <- switch(1+converge,"The run did not converge before the last cycle.","The run converged.","","WARNING: The run ended with a Hessian Error.")
    cat(coninterp,"\n")
    
    outlist <- list(nsub=nsub,nvar=nvar,nofix=nofix,par=par,parfix=parfix,covnames=covnames,ab=ab,fixedpos=fixedpos,valfix=valfix,
                    icycmax=icycmax,icyctot=icyctot,stoptol=stoptol,converge=converge,ODEtol=ODEtol,numeqt=numeqt,ERRmod=ERRmod,
                    ndrug=ndrug,salt=salt,ndose=ndose,ncov=ncov,nobs=nobs,nobsmax=nobsmax,ypredpop=ypredpop,
                    ypredbay=ypredbay,parbay=parbay,ilog=ilog,iic=iic,
                    imean=imean,imed=imed,isd=isd,icv=icv,igamlam=igamlam,lpar=lpar,lsd=lsd,lcv=lcv,
                    sdata=sdata,dosecov=dosecov,outputs=outputs,negflag=negflag,mdata=mdata)  
  }
  
  
  if(vernum==4) {  #version 1.5
    #number of subjects
    nsub <- as.numeric(ITdims[1])
    #number of random parameters
    nvar <- as.numeric(ITdims[2])
    #number of fixed parameters
    nofix <- as.numeric(ITdims[3])
    #number of random fixed parameters
    nranfix <- as.numeric(ITdims[4])
    #maximum cycles
    icycmax <- as.numeric(ITdims[5])
    #final cycle number
    icyctot <- as.numeric(ITdims[6])
    #stopping tolerance
    stoptol <- as.numeric(ITdims[7])
    #convergence
    converge <- as.numeric(ITdims[8])
    #ODE tolerance
    ODEtol <- as.numeric(ITdims[9])
    #number of output equations
    numeqt <- ITdims[10]
    #gammma estimated (0) or fixed (1) for each output equation
    ERRmod <- scan(outfile,quiet=T,skip=13,n=1,comment.char="#")
    #number of drugs
    ndrug <- scan(outfile,quiet=T,skip=13+numeqt,n=1,comment.char="#")
    #salt fraction for each drug
    salt <- scan(outfile,quiet=T,skip=14+numeqt,n=ndrug,comment.char="#")
    #number of dose events for each subjects
    ndose <- scan(outfile,quiet=T,skip=14+numeqt+ndrug,n=nsub,comment.char="#")
    #number of additional covariates
    ncov <- scan(outfile,quiet=T,skip=14+numeqt+ndrug+nsub,n=1,comment.char="#")
    #number of observations for each subject
    nobs <- scan(outfile,quiet=T,skip=15+numeqt+ndrug+nsub,n=nsub,comment.char="#")
    nobsmax <- max(nobs)
    
    #get table of contents
    toc <- scan(outfile,quiet=T,skip=17+numeqt+ndrug+2*nsub,n=23,comment.char="#") 
    #get random parameter names
    par <- scan(outfile,what="character",n=nvar,skip=toc[1],quiet=T)
    #get fixed parameter names
    parfix <- if(nofix>0) scan(outfile,what="character",skip=toc[2],n=nofix,quiet=T)
    #get fixed but randome parameter names
    parranfix <- if(nranfix>0) scan(outfile,what="character",skip=toc[3],n=nranfix,quiet=T)
    #get initial ranges for random parameters
    ab <- matrix(scan(outfile,n=nvar*3,skip=toc[4],quiet=T,what="character"),nrow=nvar,ncol=3,byrow=T)
    fixedpos <- which(ab[,3]=="NO")
    if(length(fixedpos)==0) fixedpos <- NULL
    ab <- matrix(as.numeric(ab[,1:2]),ncol=2)
    #get values for fixed parameters
    valfix <- if(nofix>0) scan(outfile,skip=toc[5],n=nofix,quiet=T)
    #get values for random fixed parameters
    valfix <- if(nranfix>0) scan(outfile,skip=toc[6],n=nranfix,quiet=T)
    #get the covariate names
    covNameOffset <- switch(vernum,2,0,0,0)
    if (ncov>0){
      covnames <- scan(outfile,what="character",skip=toc[7],n=ncov+covNameOffset,quiet=T)
    } else {covnames <- NA}
    #get aic-bic
    ic <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[8],n=2,quiet=T,what=""))),nrow=1,ncol=2,byrow=T)
    #get population predictions at observation times
    temp1 <-as.numeric(sub("D","E",scan(outfile,skip=toc[9],n=numeqt*sum(nobs)*2,quiet=T,what="")))
    
    #get posterior predictions at observation times
    temp2 <-as.numeric(sub("D","E",scan(outfile,skip=toc[10],n=numeqt*sum(nobs)*2,quiet=T,what="")))
    
    #flag negative pop or post preds
    if (any(temp1<0,na.rm=T) | any(temp2<0,na.rm=T)){
      #    temp1[temp1<0] <- 0
      #    temp2[temp2<0] <- 0
      negflag <- T
    }
    
    ypredpop <- array(dim=c(nsub,numeqt,nobsmax,2))
    ypredbay <- array(dim=c(nsub,numeqt,nobsmax,2))
    count <- 1
    cat("\nPopulation and posterior predictions at each subject's observation times.\n")
    flush.console()
    pb <- txtProgressBar(min = 1, max = nsub, style = 3)
    for (jsub in 1:nsub){
      setTxtProgressBar(pb,jsub)
      for (ieq in 1:numeqt){
        for (iobs in 1:nobs[jsub]){
          for (icen in 1:2){
            ypredpop[jsub,ieq,iobs,icen]<-temp1[count]
            ypredbay[jsub,ieq,iobs,icen]<-temp2[count]
            count <- count+1
          }
        }
      }
    }
    count <- 1
    #get MAP bayesian parameter estimate
    temp <-as.numeric(sub("D","E",scan(outfile,skip=toc[11],n=nsub*2*nvar,quiet=T,what="")))
    parbay <- array(dim=c(nsub,nvar,2))
    count <- 1
    cat("\nBayesian posterior parameters.\n")
    flush.console()
    pb <- txtProgressBar(min = 1, max = nsub, style = 3)
    for (jsub in 1:nsub){
      setTxtProgressBar(pb,jsub)
      for (ivar in 1:nvar){
        for (icen in 1:2){
          parbay[jsub,ivar,icen]<-temp[count]
          count <- count+1
          
        }
      }
    }
    count <- 1
    
    #CYCLE INFORMATION
    cat("\nCycle information.\n")
    flush.console()
    if (icyctot > 0){
      #get log-likelihoods
      ilog <- as.numeric(sub("D","E",scan(outfile,skip=toc[12],n=icyctot,quiet=T,what="")))
      #get means
      imean <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[13],n=icyctot*nvar,quiet=T,what=""))),nrow=icyctot,ncol=nvar,byrow=T)
      dimnames(imean) <- list(cycle=1:icyctot,par=par)
      #get medians
      imed <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[14],n=icyctot*nvar,quiet=T,what=""))),nrow=icyctot,ncol=nvar,byrow=T)
      dimnames(imed) <- list(cycle=1:icyctot,par=par)
      #get SDs
      if(nsub>1) {
        isd <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[15],n=icyctot*nvar,quiet=T,what=""))),nrow=icyctot,ncol=nvar,byrow=T)
      } else { isd <- matrix(NA,nrow=icyctot,ncol=nvar)}
      dimnames(isd) <- list(cycle=1:icyctot,par=par)
      #get CVs
      if(nsub>1) {
        icv <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[16],n=icyctot*nvar,quiet=T,what=""))),nrow=icyctot,ncol=nvar,byrow=T)
      } else { icv <- matrix(NA,nrow=icyctot,ncol=nvar)}
      dimnames(icv) <- list(cycle=1:icyctot,par=par)
      
      #get gamlam values
      igamlam <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[17],n=numeqt*icyctot,quiet=T,what=""))),ncol=numeqt,byrow=T)
      
      #calculate cycle aic and bic
      if(all(diff(as.numeric(igamlam))==0)){q <- 0} else {q <- 1}
      aic <- -2*ilog + (nvar^2 + 3*nvar)/2 + q
      bic <- -2*ilog + 0.5*((nvar^2 + 3*nvar)/2 + q) * log(sum(nobs))
      iic <- data.frame(cbind(aic,bic))
      names(iic) <- c("AIC","BIC")
      
    } else {
      ilog <- NA
      iic <- NA
      imean <- NA
      isd <- NA
      icv <- NA
      igamlam <- NA
    }
    
    #LAST CYCLE
    #get par values
    lpar <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[18],n=nsub*nvar,quiet=T,what=""))),nrow=nsub,ncol=nvar,byrow=T)
    dimnames(lpar) <- list(nsub=1:nsub,par=par)
    #get par SDs
    lsd <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[19],n=nsub*nvar,quiet=T,what=""))),nrow=nsub,ncol=nvar,byrow=T)
    dimnames(lsd) <- list(nsub=1:nsub,par=par)
    #get par CVs
    lcv <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[20],n=nsub*nvar,quiet=T,what=""))),nrow=nsub,ncol=nvar,byrow=T)
    dimnames(lcv) <- list(nsub=1:nsub,par=par)
    
    #SUBJECT DATA
    cat("\nSubject and covariate data.\n")
    flush.console()
    sdata <- matrix(scan(outfile,skip=toc[21],n=nsub*5,quiet=T,what="character"),nrow=nsub,ncol=5,byrow=T)
    sdata <- data.frame(id=checkID(sdata[,2]),nsub=1:nsub,age=as.numeric(sdata[,3]),
                        sex=sdata[,4],ht=as.numeric(sdata[,5]))
    #dose covariates
    doseCovOffset <- switch(vernum,3,1,1,1)
    dosecov <- cbind(rep(1:nsub,ndose),matrix(as.numeric(sub("D","E",scan(outfile,skip=toc[22],n=sum(ndose)*(2*ndrug+doseCovOffset+ncov),quiet=T,what=""))),nrow=sum(ndose),byrow=T))
    
    #outputs
    outputs <- matrix(as.numeric(sub("D","E",scan(outfile,skip=toc[23],n=sum(nobs)*(numeqt)*8,quiet=T,what=""))),ncol=8,byrow=T)
    outputs <- data.frame(outputs)
    names(outputs) <- c("id","time","outeq","out","c0","c1","c2","c3")
    
    #data filename
    mdata <- scan(outfile,skip=toc[23]+sum(nobs)*numeqt,quiet=T,what="character",n=1)
    if(length(mdata)==0) mdata <- "NA"
    
    #summary
    cat("Output file:",outfile,"\n")
    cat("Random parameters:",par,"\n")
    if(nranfix==0){cat("There were no fixed but unknown parameters.\n")
    } else {cat("Fixed unknown parameters:",parranfix,"\n")}
    if(nofix==0){cat("There were no fixed parameters.\n")
    } else {cat("Fixed parameters:",parfix,"\n")}
    cat("Number of analyzed subjects:",nsub,"\n")
    cat("Number of output equations:",numeqt,"\n")
    cat("Additional covariates:",covnames,"\n")
    if (negflag){ cat("WARNING: There were negative or non-real pop/post predictions.\n")}
    coninterp <- switch(1+converge,"The run did not converge before the last cycle.","The run converged.","","WARNING: The run ended with a Hessian Error.")
    cat(coninterp,"\n")
    
    outlist <- list(nsub=nsub,nvar=nvar,nranfix=nranfix,nofix=nofix,par=par,parranfix=parranfix,parfix=parfix,covnames=covnames,ab=ab,fixedpos=fixedpos,valfix=valfix,
                    icycmax=icycmax,icyctot=icyctot,stoptol=stoptol,converge=converge,ODEtol=ODEtol,numeqt=numeqt,ERRmod=ERRmod,
                    ndrug=ndrug,salt=salt,ndose=ndose,ncov=ncov,nobs=nobs,nobsmax=nobsmax,ypredpop=ypredpop,
                    ypredbay=ypredbay,parbay=parbay,ilog=ilog,iic=iic,
                    imean=imean,imed=imed,isd=isd,icv=icv,igamlam=igamlam,lpar=lpar,lsd=lsd,lcv=lcv,
                    sdata=sdata,dosecov=dosecov,outputs=outputs,negflag=negflag,mdata=mdata)  
  }
  
  
  class(outlist)="IT2B"
  return(outlist)
}



