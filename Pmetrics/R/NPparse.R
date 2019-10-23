#' \code{NPparse} processes the output from an NPAG run into a list.
#'
#' This function can take some time to process the RFILE, depending on the number of subjects,
#'  doses, observations, etc.  Typical wait times are a few seconds up to 5 minutes.
#'  When processing is complete a summary of the extracted data will be reported on the console.
#'
#' @title Parse Pmetrics NPAG Output
#' @param outfile This is the filename of the output from NPAG. Typically,
#' the file will be called NP_RF0001.txt, and this is the default.
#' @return The output of \code{NPparse} is a list with the following objects and
#'  of the class \emph{NPAG}.
#'  \item{nsub }{Number of subjects}
#'  \item{nactve }{Number of active grid points at the final cycle}
#'  \item{nvar }{Number of random variables or parameters in the model}
#'  \item{nofix }{Number of fixed variables or parameters in the model}
#'  \item{par }{Names of random parameters}
#'  \item{parfix }{Names of fixed parameters}
#'  \item{covnames }{Names of covariates}
#'  \item{ab }{Initial boundaries for each random parameter}
#'  \item{valfix }{Values for fixed parameters}
#'  \item{ndim }{Number of differential equations in model, or 0 for only output equation, or -1 for analytic
#' solution (algabraic)}
#'  \item{indpts }{Index for the initial number of gridpoints in the model}
#'  \item{icycst }{Starting cycle number}
#'  \item{icycmax }{Maximum number of cycles specified by the user}
#'  \item{icyctot }{Number of cycles run.  If less than \code{icycmax}, convergence occurred.}
#'  \item{converge }{Boolean value if convergence occurred.}
#'  \item{ODEtol }{Ordindary Differential Equation solver tolerance.}
#'  \item{prior }{Prior density for the run, either \dQuote{UNIFORM} or the name of the user-specified density file, typically \dQuote{DEN0001}.}
#'  \item{ERRmod }{Assay error model: 1 for SD; 2 for SD*gamma; 3 for additive lambda model; and 4 for gamma only}
#'  \item{numeqt }{Number of output equations}
#'  \item{ndrug }{Number of drug inputs}
#'  \item{salt }{Vector of values of the salt fraction for each \code{ndrug}}
#'  \item{ndose }{Vector of the number of doses for each subject in the population}
#'  \item{ncov }{Number of covariates in the model}
#'  \item{nobs }{Vector of the number of observations for each subject in the population}
#'  \item{nobsmax }{Maximum number of observation in any individual subject}
#'  \item{numt }{Vector of the number of time points for each subject at which a prediction is generated for each \emph{numeqt} output equation}
#'  \item{corden }{Final cycle joint population density of parameter estimates}
#'  \item{postden }{Array of posterior parameter value distributions for the first 100  subjects at each observation time point.
#'  \emph{postden[nsub,nactvepost,density]} where \emph{nactvepost} is the posterior grid point}
#'  \item{pyjgx }{Matrix of posterior probability of each \emph{nactve} point for each subject, given that subject's data}
#'  \item{ypredpop }{Array of population model predictions for each subject at each observation time point.
#'  \emph{ypredpop[nsub,numeqt,time,type]} where \emph{type} is 1=mean, 2=median, 3=mode of the population prior used to calculate ypredpop}
#'  \item{ypredbay }{Array of Bayesian posterior model predictions for each subject at each observation time point.
#'  \emph{ypredbay[nsub,numeqt,time,type]} where \emph{type} is 1=mean, 2=median, 3=mode of the population prior used to calculate ypredbay}
#'  \item{ttpred }{Matrix of the prediction time points for each subject, with \emph{nsub} rows and max(\emph{numt}) columns}
#'  \item{exx }{Array of the mean, median, and mode of the posterior marginal distribution for each parameter in each subject, of the form \emph{exx[nvar,type,nsub]}}
#'  \item{ypredpopt }{Array of population model predictions for each subject at each
#'  \emph{ttpred} time point,  of the form \emph{ypredpopt[nsub,  numeqt,  time,  type]},
#'  where type is 1=mean,  2=median,  3=mode of the population prior used to calculate
#'  \emph{ypredpopt}}
#'  \item{ilog }{Matrix of cycle number and associated log-likelihood}
#'  \item{iic }{Matrix with cycle number and Akaike Information Criterion (AIC) and Bayesian Information Criterion (BIC) for each cycle}
#'  \item{imean }{Matrix of cycle numbers and associated means for each random parameter}
#'  \item{isd }{Matrix of cycle numbers and associated standard deviations for each random parameter}
#'  \item{iaddl }{Array of additional information for each random parameter in each cycle,
#'  of the form \emph{iaddl[info, nvar, cycle]},  where info is a value from 1 to 12:
#'  1= mode; 2= skewness; 3= kurtosis; 4-8 give percentiles of the distribution (4=2.5\%; 5=25\%;
#'  6=50\% [median],  7=75\%; 8=97.5\%); 9= the standard deviation of a normal distribution
#'  with the same interquartile range; 10=the standard deviation of a normal distribution
#'  with the same 95\% range; 11=the average of 9 and 10; 12=the \% scaled information}
#'  \item{igamlam }{Matrix of cycle number and associated gamma or lambda}
#'  \item{blog }{Vector of each subject's Bayesian posterior log-likelihood}
#'  \item{bmean }{Matrix of subject numbers and associated Bayesian posterior means for each random parameter}
#'  \item{bsd }{Matrix of subject numbers and associated Bayesian posterior standard deviations for each random parameter}
#'  \item{baddl }{Array of Bayesian posterior additional information for each random parameter
#'  for each subject, of the form \emph{baddl[info, nvar, nsub]},  where info is the same as for \emph{iaddl}.}
#'  \item{bauc }{Matrix of AUC blocks for each subject with 5 columns:
#'  [nsub,  numeqt,  nblock,  tau,  auc]; \emph{nsub} and \emph{numeqt} are as previously defined;
#'  \emph{nblock} is the AUC block as defined by successive dose reset (evid=4) events;
#'  \emph{tau} is the time interval for that block; \emph{auc} is the AUC for that block}
#'  \item{sdata }{Subject data consisting of 5 columns: [id,  nsub,  age,  sex,  ht],
#'  \emph{id} is the original identification number in the .csv matrix file;
#'  \emph{nsub} is the sequential subject number in the NPAG run; \emph{age},
#'  \emph{sex} and \emph{ht} will be missing for .csv input and present if included in .wrk input files}
#'  \item{dosecov }{Matrix with all dosing information for each subject,  including times,  routes,  amounts,  and associated covariate values}
#'  \item{outputs }{Matrix with measured outputs for each subject and associated assay error polynomials.
#'   The order of the columns is nsub, time, numeqt, observation, c0, c1, c2, c3, where the last
#'   four columns are the coefficients of the assay error polynomial for that observation, such that
#'   SD[obs] = c0 + c1*[obs] + c2*[obs]**2 + c3*[obs]**3}
#'  \item{negflag }{A flag indicating that some negative predictions were changed to missing.
#'  This means that the model may be misspecified.}
#'  \item{mdata }{The filename of the data used in the run.}
#' @author Michael Neely



NPparse <- function(outfile="NP_RF0001.TXT"){
  #require(utils)
  cat("\n\n\nParsing NPAG results...\n\n")
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
                   " VERSION 1.4 - AUG 2011 "=4,
                   " VERSION 1.5 - JUL 2012 "=4,
                   " VERSION 1.6 - APR 2013 "=5,
                   " VERSION 1.7 - MAR 2014 "=6,
                   " VERSION 1.8 - OCT 2015 "=7,
                   7)
  
  # Vernum<3 ----------------------------------------------------------------
  
  #if version 1.2 or less
  if(vernum<3){
    NPdims <- scan(outfile,quiet=T,skip=3,nlines=9,what="character",comment.char="#")
    #number of subjects
    nsub <- as.numeric(NPdims[1])
    #number of active grid points
    nactve <- as.numeric(NPdims[2])
    #number of random parameters
    nvar <- as.numeric(NPdims[3])
    #number of fixed parameters
    nofix <- as.numeric(NPdims[4])
    #number of dimensions (always 3 for mean, median, mode)
    ndim <- as.numeric(NPdims[5])
    #index of grid points
    indpts <- as.numeric(NPdims[6])
    #final cycle number
    icyctot <- as.numeric(NPdims[7])
    #number of output equations
    numeqt <- as.numeric(NPdims[8])
    #number of drugs
    ndrug <- as.numeric(NPdims[9])
    #number of dose events for each subjects
    ndose <- scan(outfile,quiet=T,skip=12,n=nsub,comment.char="#")
    #number of additional covariates
    ncov <- scan(outfile,quiet=T,skip=12+nsub,n=1,comment.char="#")
    #number of observations for each subject
    nobs <- scan(outfile,quiet=T,skip=13+nsub,n=nsub,comment.char="#")
    nobsmax <- max(nobs)
    #number of prediction times for each subject
    numt <- scan(outfile,quiet=T,skip=13+2*nsub,n=nsub,comment.char="#")
    #number of AUC blocks over all subjects
    nauc <- scan(outfile,quiet=T,skip=13+3*nsub,n=1,comment.char="#")
    #matrix with subj num, num of max times, and num of periods+1 for AUC tables
    if(nauc > 0) {aucM <- matrix(scan(outfile,quiet=T,skip=16+3*nsub,n=nauc*3),nrow=nauc,ncol=3,byrow=T)} else {aucM <- NA}
    #get table of contents
    toc <- scan(outfile,quiet=T,skip=18+3*nsub+nauc,n=26,comment.char="#")
    #get random parameter names
    par <- scan(outfile,what="character",n=nvar,skip=toc[1],quiet=T)
    #get fixed parameter names
    parfix <- if(nofix>0) scan(outfile,what="character",skip=toc[2],n=nofix,quiet=T)
    #get initial ranges for random parameters
    ab <- matrix(as.numeric(sub("D","E",scan(outfile,n=nvar*2,skip=toc[3],quiet=T,what=""))),nrow=nvar,ncol=2,byrow=T)
    #get values for fixed parameters
    valfix <- if(nofix>0) scan(outfile,skip=toc[4],n=nofix,quiet=T)
    #get the covariate names
    if (ncov>0){
      covnames <- scan(outfile,what="character",skip=toc[5],n=ncov+2,quiet=T)
    } else {covnames <- NA} 
    #get the population density
    corden <- matrix(as.numeric(sub("D","E",scan(outfile,skip=toc[6],n=nactve*(nvar+1),quiet=T,what=""))),nrow=nactve,ncol=nvar+1,byrow=T)
    #get predicted
    pyjgx <- matrix(as.numeric(sub("D","E",scan(outfile,skip=toc[7],n=nsub*nactve,quiet=T,what=""))),nrow=nsub,ncol=nactve,byrow=T)
    #get population predictions at observation times
    temp1 <-as.numeric(sub("D","E",scan(outfile,skip=toc[8],n=numeqt*sum(nobs)*3,quiet=T,what="")))
    #get posterior predictions at observation times
    temp2 <-as.numeric(sub("D","E",scan(outfile,skip=toc[9],n=numeqt*sum(nobs)*3,quiet=T,what="")))
    #flag negative pop or post preds 
    if (any(temp1<0,na.rm=T) | any(temp2<0,na.rm=T)){
      negflag <- T
    }
    ypredpop <- array(dim=c(nsub,numeqt,nobsmax,3))
    ypredbay <- array(dim=c(nsub,numeqt,nobsmax,3))
    count <- 1
    cat("\nPopulation and posterior predictions at each subject's observation times.\n")
    flush.console()
    if(nsub>1) pb <- txtProgressBar(min = 1, max = nsub, style = 3)
    for (jsub in 1:nsub){
      if(nsub>1) setTxtProgressBar(pb,jsub)
      for (ieq in 1:numeqt){
        for (iobs in 1:nobs[jsub]){
          for (icen in 1:3){
            ypredpop[jsub,ieq,iobs,icen]<-temp1[count]
            ypredbay[jsub,ieq,iobs,icen]<-temp2[count]
            count <- count+1
          }
        }
      }
    }
    count <- 1
    #get prediction times for each subject
    temp <-as.numeric(sub("D","E",scan(outfile,skip=toc[10],n=sum(numt),quiet=T,what="")))
    ttpred <- matrix(nrow=nsub,ncol=max(numt))
    cat("\nPrediction times (tpred) for each subject.\n")
    flush.console()
    if(nsub>1) pb <- txtProgressBar(min = 1, max = nsub, style = 3)
    for (jsub in 1:nsub){
      if(nsub>1) setTxtProgressBar(pb,jsub)
      for (j in 1:numt[jsub]){
        ttpred[jsub,j] <- temp[count]
        count <- count+1
      }
    }
    count <- 1
    #get population predictions at each ttpred time
    temp <-as.numeric(sub("D","E",scan(outfile,skip=toc[11],n=numeqt*sum(numt)*3,quiet=T,what="")))
    if(any(temp<0,na.rm=T)){
      temp[temp<0] <- 0
      negflag <- T
    }
    ypredpopt <- array(dim=c(nsub,numeqt,max(numt),3))
    cat("\nPopulation predictions for each subject and tpred.\n")
    flush.console()
    if(nsub>1) pb <- txtProgressBar(min = 1, max = nsub, style = 3)
    for (jsub in 1:nsub){
      if(nsub>1) setTxtProgressBar(pb,jsub)
      for (ieq in 1:numeqt){
        for (j in 1:numt[jsub]){
          for (icen in 1:3){
            ypredpopt[jsub,ieq,j,icen]<-temp[count]
            count <- count+1
          }
        }
      }
    }
    count <- 1 
    #get mean, median and mode of posterior values for each parameter in each subject
    exx <- array(data=as.numeric(sub("D","E",scan(outfile,skip=toc[12],n=nsub*3*nvar,quiet=T,what=""))),dim=c(nvar,3,nsub))
    
    #CYCLE INFORMATION
    cat("\nCycle information.\n")
    flush.console()
    if (icyctot > 0){
      #get log-likelihoods
      ilog <- as.numeric(sub("D","E",scan(outfile,skip=toc[13],n=icyctot,quiet=T,what="")))
      #get aic-bic
      iic <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[14],n=icyctot*2,quiet=T,what=""))),nrow=icyctot,ncol=2,byrow=T)
      #get means
      imean <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[15],n=icyctot*nvar,quiet=T,what=""))),nrow=icyctot,ncol=nvar,byrow=T)
      #get SDs
      if(nsub>1) {
        isd <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[16],n=icyctot*nvar,quiet=T,what=""))),nrow=icyctot,ncol=nvar,byrow=T)
      } else { isd <- matrix(NA,nrow=icyctot,ncol=nvar)}
      isd[isd==0] <- NA
      #get additional information
      iaddl <- array(data=as.numeric(sub("D","E",scan(outfile,skip=toc[17],n=icyctot*nvar*12,quiet=T,what=""))),dim=c(12,nvar,icyctot))
      #get gamlam values
      igamlam <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[18],n=icyctot,quiet=T,what=""))),ncol=1,byrow=T)
      igamlam[igamlam==-99] <- 1
      
    } else {
      ilog <- NA
      iic <- NA
      imean <- NA
      isd <- NA
      iaddl <- NA
      igamlam <- NA
    }
    
    #BAYESIAN INFORMATION
    cat("\nBayesian posterior parameters.\n")
    flush.console()
    #get log-likelihoods
    blog <- as.numeric(sub("D","E",scan(outfile,skip=toc[19],n=nsub,quiet=T,what="")))
    #get means
    bmean <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[20],n=nsub*nvar,quiet=T,what=""))),nrow=nsub,ncol=nvar,byrow=T)
    #get SDs
    bsd <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[21],n=nsub*nvar,quiet=T,what=""))),nrow=nsub,ncol=nvar,byrow=T)
    #get additional information
    baddl <- array(data=as.numeric(sub("D","E",scan(outfile,skip=toc[22],n=nsub*nvar*12,quiet=T,what=""))),dim=c(12,nvar,nsub))
    #get gamlam values
    if(nauc>0){
      bauc <- matrix(as.numeric(sub("D","E",scan(outfile,skip=toc[23],n=numeqt*5*sum(aucM[,3]),quiet=T,what=""))),nrow=numeqt*sum(aucM[,3]),ncol=5,byrow=T)
    } else {bauc <- NA}
    #SUBJECT DATA
    cat("\nSubject and covariate data.\n")
    flush.console()
    sdata <- matrix(scan(outfile,skip=toc[24],n=nsub*5,quiet=T,what="character"),nrow=nsub,ncol=5,byrow=T)
    sdata <- data.frame(id=checkID(sdata[,2]),nsub=1:nsub,age=as.numeric(sdata[,3]),
                        sex=sdata[,4],ht=as.numeric(sdata[,5]))
    #dose covariates
    dosecov <- cbind(rep(1:nsub,ndose),matrix(as.numeric(sub("D","E",scan(outfile,skip=toc[25],n=sum(ndose)*(2*ndrug+3+ncov),quiet=T,what=""))),nrow=sum(ndose),byrow=T))
    #outputs
    outputs <- cbind(rep(1:nsub,nobs),matrix(as.numeric(sub("D","E",scan(outfile,skip=toc[26],n=sum(nobs)*(numeqt+1),quiet=T,what=""))),nrow=sum(nobs),byrow=T))
    #data filename
    mdata <- scan(outfile,skip=toc[26]+sum(nobs)*numeqt,quiet=T,what="character",n=1)
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
    outlist <- list(nsub=nsub,nactve=nactve,nvar=nvar,nofix=nofix,par=par,parfix=parfix,covnames=covnames,ab=ab,valfix=valfix,ndim=ndim,
                    indpts=indpts,icyctot=icyctot,numeqt=numeqt,ndrug=ndrug,ndose=ndose,ncov=ncov,
                    nobs=nobs,nobsmax=nobsmax,numt=numt,corden=corden,pyjgx=pyjgx,ypredpop=ypredpop,
                    ypredbay=ypredbay,ttpred=ttpred,exx=exx,ypredpopt=ypredpopt,ilog=ilog,
                    iic=iic,imean=imean,isd=isd,iaddl=iaddl,igamlam=igamlam,blog=blog,bmean=bmean,
                    bsd=bsd,baddl=baddl,bauc=bauc,sdata=sdata,dosecov=dosecov,outputs=outputs,negflag=negflag)
    
  } 
  
  # Verum==3 ----------------------------------------------------------------
  
  #if version 1.3  
  if(vernum==3){
    NPdims <- scan(outfile,quiet=T,skip=3,nlines=15,what="character",comment.char="#")
    #number of subjects
    nsub <- as.numeric(NPdims[1])
    #number of active grid points
    nactve <- as.numeric(NPdims[2])
    #number of random parameters
    nvar <- as.numeric(NPdims[3])
    #number of fixed parameters
    nofix <- as.numeric(NPdims[4])
    #number of dimensions (always 3 for mean, median, mode)
    ndim <- as.numeric(NPdims[5])
    #index of grid points
    indpts <- as.numeric(NPdims[6])
    #start cycle number
    icycst <- as.numeric(NPdims[7])
    #maximum cycles
    icycmax <- as.numeric(NPdims[8])
    #final cycle number
    icyctot <- as.numeric(NPdims[9])
    #convergence
    converge <- as.numeric(NPdims[10])==1
    #ODE tolerance
    ODEtol <- as.numeric(NPdims[11])
    #prior density
    prior <- NPdims[12]
    #assay error model
    ERRmod <- as.numeric(NPdims[13])
    #number of output equations
    numeqt <- as.numeric(NPdims[14])
    #number of drugs
    ndrug <- as.numeric(NPdims[15])
    #number of dose events for each subjects
    ndose <- scan(outfile,quiet=T,skip=19,n=nsub,comment.char="#")
    #number of additional covariates
    ncov <- scan(outfile,quiet=T,skip=19+nsub,n=1,comment.char="#")
    #number of observations for each subject
    nobs <- scan(outfile,quiet=T,skip=20+nsub,n=nsub,comment.char="#")
    nobsmax <- max(nobs)
    #number of prediction times for each subject
    numt <- scan(outfile,quiet=T,skip=20+2*nsub,n=nsub,comment.char="#")
    #number of AUC blocks over all subjects
    nauc <- scan(outfile,quiet=T,skip=20+3*nsub,n=1,comment.char="#")
    #matrix with subj num, num of max times, and num of periods+1 for AUC tables
    if(nauc>0) {aucM <- matrix(scan(outfile,quiet=T,skip=23+3*nsub,n=nauc*3),nrow=nauc,ncol=3,byrow=T)} else {aucM <- NA}
    
    #get table of contents
    toc <- scan(outfile,quiet=T,skip=25+3*nsub+nauc,n=26,comment.char="#")
    #get random parameter names
    par <- scan(outfile,what="character",n=nvar,skip=toc[1],quiet=T)
    #get fixed parameter names
    parfix <- if(nofix>0) scan(outfile,what="character",skip=toc[2],n=nofix,quiet=T)
    #get initial ranges for random parameters
    ab <- matrix(as.numeric(sub("D","E",scan(outfile,n=nvar*2,skip=toc[3],quiet=T,what=""))),nrow=nvar,ncol=2,byrow=T)
    #get values for fixed parameters
    valfix <- if(nofix>0) scan(outfile,skip=toc[4],n=nofix,quiet=T)
    #get the covariate names
    if (ncov>0){
      covnames <- scan(outfile,what="character",skip=toc[5],n=ncov,quiet=T)
    } else {covnames <- NA} 
    #get the population density
    corden <- matrix(as.numeric(sub("D","E",scan(outfile,skip=toc[6],n=nactve*(nvar+1),quiet=T,what=""))),nrow=nactve,ncol=nvar+1,byrow=T)
    #get predicted
    pyjgx <- matrix(as.numeric(sub("D","E",scan(outfile,skip=toc[7],n=nsub*nactve,quiet=T,what=""))),nrow=nsub,ncol=nactve,byrow=T)
    #get population predictions at observation times
    temp1 <-as.numeric(sub("D","E",scan(outfile,skip=toc[8],n=numeqt*sum(nobs)*3,quiet=T,what="")))
    #get posterior predictions at observation times
    temp2 <-as.numeric(sub("D","E",scan(outfile,skip=toc[9],n=numeqt*sum(nobs)*3,quiet=T,what="")))
    #flag negative pop or post preds 
    if (any(temp1<0,na.rm=T) | any(temp2<0,na.rm=T)){
      negflag <- T
    }
    ypredpop <- array(dim=c(nsub,numeqt,nobsmax,3))
    ypredbay <- array(dim=c(nsub,numeqt,nobsmax,3))
    count <- 1
    cat("\nPopulation and posterior predictions at each subject's observation times.\n")
    flush.console()
    if(nsub>1) pb <- txtProgressBar(min = 1, max = nsub, style = 3)
    for (jsub in 1:nsub){
      if(nsub>1) setTxtProgressBar(pb,jsub)
      for (ieq in 1:numeqt){
        for (iobs in 1:nobs[jsub]){
          for (icen in 1:3){
            ypredpop[jsub,ieq,iobs,icen]<-temp1[count]
            ypredbay[jsub,ieq,iobs,icen]<-temp2[count]
            count <- count+1
          }
        }
      }
    }
    count <- 1
    #get prediction times for each subject
    temp <-as.numeric(sub("D","E",scan(outfile,skip=toc[10],n=sum(numt),quiet=T,what="")))
    ttpred <- matrix(nrow=nsub,ncol=max(numt))
    cat("\nPrediction times (tpred) for each subject.\n")
    flush.console()
    if(nsub>1) pb <- txtProgressBar(min = 1, max = nsub, style = 3)
    for (jsub in 1:nsub){
      if(nsub>1) setTxtProgressBar(pb,jsub)
      for (j in 1:numt[jsub]){
        ttpred[jsub,j] <- temp[count]
        count <- count+1
      }
    }
    count <- 1
    #get population predictions at each ttpred time
    temp <-as.numeric(sub("D","E",scan(outfile,skip=toc[11],n=numeqt*sum(numt)*3,quiet=T,what="")))
    if(any(temp<0,na.rm=T)){
      temp[temp<0] <- 0
      negflag <- T
    }
    ypredpopt <- array(dim=c(nsub,numeqt,max(numt),3))
    cat("\nPopulation predictions for each subject and tpred.\n")
    flush.console()
    if(nsub>1) pb <- txtProgressBar(min = 1, max = nsub, style = 3)
    for (jsub in 1:nsub){
      if(nsub>1) setTxtProgressBar(pb,jsub)
      for (ieq in 1:numeqt){
        for (j in 1:numt[jsub]){
          for (icen in 1:3){
            ypredpopt[jsub,ieq,j,icen]<-temp[count]
            count <- count+1
          }
        }
      }
    }
    count <- 1 
    #get mean, median and mode of posterior values for each parameter in each subject
    exx <- array(data=as.numeric(sub("D","E",scan(outfile,skip=toc[12],n=nsub*3*nvar,quiet=T,what=""))),dim=c(nvar,3,nsub))
    
    #CYCLE INFORMATION
    cat("\nCycle information.\n")
    flush.console()
    if (icyctot > 0){
      #get log-likelihoods
      ilog <- as.numeric(sub("D","E",scan(outfile,skip=toc[13],n=icyctot,quiet=T,what="")))
      #get aic-bic
      iic <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[14],n=icyctot*2,quiet=T,what=""))),nrow=icyctot,ncol=2,byrow=T)
      #get means
      imean <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[15],n=icyctot*nvar,quiet=T,what=""))),nrow=icyctot,ncol=nvar,byrow=T)
      #get SDs
      if(nsub>1) {
        isd <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[16],n=icyctot*nvar,quiet=T,what=""))),nrow=icyctot,ncol=nvar,byrow=T)
      } else { isd <- matrix(NA,nrow=icyctot,ncol=nvar)}
      isd[isd==0] <- NA
      #get additional information
      iaddl <- array(data=as.numeric(sub("D","E",scan(outfile,skip=toc[17],n=icyctot*nvar*12,quiet=T,what=""))),dim=c(12,nvar,icyctot))
      #get gamlam values
      igamlam <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[18],n=icyctot,quiet=T,what=""))),ncol=1,byrow=T)
      igamlam[igamlam==-99] <- 1
      
    } else {
      ilog <- NA
      iic <- NA
      imean <- NA
      isd <- NA
      iaddl <- NA
      igamlam <- NA
    }
    
    #BAYESIAN INFORMATION
    cat("\nBayesian posterior parameters.\n")
    flush.console()
    #get log-likelihoods
    blog <- as.numeric(sub("D","E",scan(outfile,skip=toc[19],n=nsub,quiet=T,what="")))
    #get means
    bmean <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[20],n=nsub*nvar,quiet=T,what=""))),nrow=nsub,ncol=nvar,byrow=T)
    #get SDs
    bsd <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[21],n=nsub*nvar,quiet=T,what=""))),nrow=nsub,ncol=nvar,byrow=T)
    #get additional information
    baddl <- array(data=as.numeric(sub("D","E",scan(outfile,skip=toc[22],n=nsub*nvar*12,quiet=T,what=""))),dim=c(12,nvar,nsub))
    #get gamlam values
    if(nauc>0){
      bauc <- matrix(as.numeric(sub("D","E",scan(outfile,skip=toc[23],n=numeqt*5*sum(aucM[,3]),quiet=T,what=""))),nrow=numeqt*sum(aucM[,3]),ncol=5,byrow=T)
    } else {bauc <- NA}
    #SUBJECT DATA
    cat("\nSubject and covariate data.\n")
    flush.console()
    sdata <- matrix(scan(outfile,skip=toc[24],n=nsub*5,quiet=T,what="character"),nrow=nsub,ncol=5,byrow=T)
    sdata <- data.frame(id=checkID(sdata[,2]),nsub=1:nsub,age=as.numeric(sdata[,3]),
                        sex=sdata[,4],ht=as.numeric(sdata[,5]))
    #dose covariates
    dosecov <- cbind(rep(1:nsub,ndose),matrix(as.numeric(sub("D","E",scan(outfile,skip=toc[25],n=sum(ndose)*(2*ndrug+1+ncov),quiet=T,what=""))),nrow=sum(ndose),byrow=T))
    #outputs
    outputs <- matrix(as.numeric(sub("D","E",scan(outfile,skip=toc[26],n=sum(nobs)*numeqt*8,quiet=T,what=""))),ncol=8,byrow=T)
    
    #data filename
    mdata <- scan(outfile,skip=toc[26]+sum(nobs)*numeqt,quiet=T,what="character",n=1)
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
    
    outlist <- list(nsub=nsub,nactve=nactve,nvar=nvar,nofix=nofix,par=par,parfix=parfix,covnames=covnames,ab=ab,valfix=valfix,ndim=ndim,
                    indpts=indpts,icycst=icycst,icycmax=icycmax,icyctot=icyctot,converge=converge,ODEtol=ODEtol,prior=prior,
                    ERRmod=ERRmod,numeqt=numeqt,ndrug=ndrug,ndose=ndose,ncov=ncov,
                    nobs=nobs,nobsmax=nobsmax,numt=numt,corden=corden,pyjgx=pyjgx,ypredpop=ypredpop,
                    ypredbay=ypredbay,ttpred=ttpred,exx=exx,ypredpopt=ypredpopt,ilog=ilog,
                    iic=iic,imean=imean,isd=isd,iaddl=iaddl,igamlam=igamlam,blog=blog,bmean=bmean,
                    bsd=bsd,baddl=baddl,bauc=bauc,sdata=sdata,dosecov=dosecov,outputs=outputs,negflag=negflag) 
    
  }
  
  # Vernum==4 ---------------------------------------------------------------
  
  #if version 1.4 or 1.5
  if(vernum==4){
    NPdims <- scan(outfile,quiet=T,skip=3,nlines=15,what="character",comment.char="#")
    #number of subjects
    nsub <- as.numeric(NPdims[1])
    #number of active grid points
    nactve <- as.numeric(NPdims[2])
    #number of random parameters
    nvar <- as.numeric(NPdims[3])
    #number of fixed parameters
    nofix <- as.numeric(NPdims[4])
    #number of dimensions (always 3 for mean, median, mode)
    ndim <- as.numeric(NPdims[5])
    #index of grid points
    indpts <- as.numeric(NPdims[6])
    #start cycle number
    icycst <- as.numeric(NPdims[7])
    #maximum cycles
    icycmax <- as.numeric(NPdims[8])
    #final cycle number
    icyctot <- as.numeric(NPdims[9])
    #convergence
    converge <- as.numeric(NPdims[10])
    #ODE tolerance
    ODEtol <- as.numeric(NPdims[11])
    #prior density
    prior <- NPdims[12]
    #assay error model
    ERRmod <- as.numeric(NPdims[13])
    #number of output equations
    numeqt <- as.numeric(NPdims[14])
    #number of drugs
    ndrug <- as.numeric(NPdims[15])
    #salt fraction for each drug
    salt <- scan(outfile,quiet=T,skip=18,n=ndrug,comment.char="#")
    #number of dose events for each subjects
    ndose <- scan(outfile,quiet=T,skip=18+ndrug,n=nsub,comment.char="#")
    #number of additional covariates
    ncov <- scan(outfile,quiet=T,skip=18+ndrug+nsub,n=1,comment.char="#")
    #number of observations for each subject
    nobs <- scan(outfile,quiet=T,skip=19+ndrug+nsub,n=nsub,comment.char="#")
    nobsmax <- max(nobs)
    #number of prediction times for each subject
    numt <- scan(outfile,quiet=T,skip=19+ndrug+2*nsub,n=nsub,comment.char="#")
    #number of AUC blocks over all subjects
    nauc <- scan(outfile,quiet=T,skip=19+ndrug+3*nsub,n=1,comment.char="#")
    #matrix with subj num, num of max times, and num of periods+1 for AUC tables
    if(nauc>0) {aucM <- matrix(scan(outfile,quiet=T,skip=22+ndrug+3*nsub,n=nauc*3),nrow=nauc,ncol=3,byrow=T)} else aucM <- NA
    
    #get table of contents
    toc <- scan(outfile,quiet=T,skip=24+ndrug+3*nsub+nauc,n=26,comment.char="#")
    #get random parameter names
    par <- scan(outfile,what="character",n=nvar,skip=toc[1],quiet=T)
    #get fixed parameter names
    parfix <- if(nofix>0) scan(outfile,what="character",skip=toc[2],n=nofix,quiet=T)
    #get initial ranges for random parameters
    ab <- matrix(as.numeric(sub("D","E",scan(outfile,n=nvar*2,skip=toc[3],quiet=T,what=""))),nrow=nvar,ncol=2,byrow=T)
    #get values for fixed parameters
    valfix <- if(nofix>0) scan(outfile,skip=toc[4],n=nofix,quiet=T)
    #get the covariate names
    if (ncov>0){
      covnames <- scan(outfile,what="character",skip=toc[5],n=ncov,quiet=T)
    } else {covnames <- NA} 
    #get the population density
    corden <- matrix(as.numeric(sub("D","E",scan(outfile,skip=toc[6],n=nactve*(nvar+1),quiet=T,what=""))),nrow=nactve,ncol=nvar+1,byrow=T)
    #get predicted
    pyjgx <- matrix(as.numeric(sub("D","E",scan(outfile,skip=toc[7],n=nsub*nactve,quiet=T,what=""))),nrow=nsub,ncol=nactve,byrow=T)
    #get population predictions at observation times
    temp1 <-as.numeric(sub("D","E",scan(outfile,skip=toc[8],n=numeqt*sum(nobs)*3,quiet=T,what="")))
    #get posterior predictions at observation times
    temp2 <-as.numeric(sub("D","E",scan(outfile,skip=toc[9],n=numeqt*sum(nobs)*3,quiet=T,what="")))
    #flag negative pop or post preds 
    if (any(temp1<0,na.rm=T) | any(temp2<0,na.rm=T)){
      negflag <- T
    }
    ypredpop <- array(dim=c(nsub,numeqt,nobsmax,3))
    ypredbay <- array(dim=c(nsub,numeqt,nobsmax,3))
    count <- 1
    cat("\nPopulation and posterior predictions at each subject's observation times.\n")
    flush.console()
    if(nsub>1) pb <- txtProgressBar(min = 1, max = nsub, style = 3)
    for (jsub in 1:nsub){
      if(nsub>1) setTxtProgressBar(pb,jsub)
      for (ieq in 1:numeqt){
        for (iobs in 1:nobs[jsub]){
          for (icen in 1:3){
            ypredpop[jsub,ieq,iobs,icen]<-temp1[count]
            ypredbay[jsub,ieq,iobs,icen]<-temp2[count]
            count <- count+1
          }
        }
      }
    }
    count <- 1
    #get prediction times for each subject
    temp <-as.numeric(sub("D","E",scan(outfile,skip=toc[10],n=sum(numt),quiet=T,what="")))
    ttpred <- matrix(nrow=nsub,ncol=max(numt))
    cat("\nPrediction times (tpred) for each subject.\n")
    flush.console()
    if(nsub>1) pb <- txtProgressBar(min = 1, max = nsub, style = 3)
    for (jsub in 1:nsub){
      if(nsub>1) setTxtProgressBar(pb,jsub)
      for (j in 1:numt[jsub]){
        ttpred[jsub,j] <- temp[count]
        count <- count+1
      }
    }
    count <- 1
    #get population predictions at each ttpred time
    temp <-as.numeric(sub("D","E",scan(outfile,skip=toc[11],n=numeqt*sum(numt)*3,quiet=T,what="")))
    if(any(temp<0,na.rm=T)){
      temp[temp<0] <- 0
      negflag <- T
    }
    ypredpopt <- array(dim=c(nsub,numeqt,max(numt),3))
    cat("\nPopulation predictions for each subject and tpred.\n")
    flush.console()
    if(nsub>1) pb <- txtProgressBar(min = 1, max = nsub, style = 3)
    for (jsub in 1:nsub){
      if(nsub>1) setTxtProgressBar(pb,jsub)
      for (ieq in 1:numeqt){
        for (j in 1:numt[jsub]){
          for (icen in 1:3){
            ypredpopt[jsub,ieq,j,icen]<-temp[count]
            count <- count+1
          }
        }
      }
    }
    count <- 1 
    #get mean, median and mode of posterior values for each parameter in each subject
    exx <- array(data=as.numeric(sub("D","E",scan(outfile,skip=toc[12],n=nsub*3*nvar,quiet=T,what=""))),dim=c(nvar,3,nsub))
    
    #CYCLE INFORMATION
    cat("\nCycle information.\n")
    flush.console()
    if (icyctot > 0){
      #get log-likelihoods
      ilog <- as.numeric(sub("D","E",scan(outfile,skip=toc[13],n=icyctot,quiet=T,what="")))
      #get aic-bic
      iic <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[14],n=icyctot*2,quiet=T,what=""))),nrow=icyctot,ncol=2,byrow=T)
      #get means
      imean <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[15],n=icyctot*nvar,quiet=T,what=""))),nrow=icyctot,ncol=nvar,byrow=T)
      #get SDs
      if(nsub>1) {
        isd <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[16],n=icyctot*nvar,quiet=T,what=""))),nrow=icyctot,ncol=nvar,byrow=T)
      } else { isd <- matrix(NA,nrow=icyctot,ncol=nvar)}
      isd[isd==0] <- NA
      #get additional information
      iaddl <- array(data=as.numeric(sub("D","E",scan(outfile,skip=toc[17],n=icyctot*nvar*12,quiet=T,what=""))),dim=c(12,nvar,icyctot))
      #get gamlam values
      igamlam <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[18],n=icyctot,quiet=T,what=""))),ncol=1,byrow=T)
      igamlam[igamlam==-99] <- 1
      
    } else {
      ilog <- NA
      iic <- NA
      imean <- NA
      isd <- NA
      iaddl <- NA
      igamlam <- NA
    }
    
    #BAYESIAN INFORMATION
    cat("\nBayesian posterior parameters.\n")
    flush.console()
    #get log-likelihoods
    blog <- as.numeric(sub("D","E",scan(outfile,skip=toc[19],n=nsub,quiet=T,what="")))
    #get means
    bmean <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[20],n=nsub*nvar,quiet=T,what=""))),nrow=nsub,ncol=nvar,byrow=T)
    #get SDs
    bsd <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[21],n=nsub*nvar,quiet=T,what=""))),nrow=nsub,ncol=nvar,byrow=T)
    #get additional information
    baddl <- array(data=as.numeric(sub("D","E",scan(outfile,skip=toc[22],n=nsub*nvar*12,quiet=T,what=""))),dim=c(12,nvar,nsub))
    #get gamlam values
    if(nauc>0){
      bauc <- matrix(as.numeric(sub("D","E",scan(outfile,skip=toc[23],n=numeqt*5*sum(aucM[,3]),quiet=T,what=""))),nrow=numeqt*sum(aucM[,3]),ncol=5,byrow=T)
    } else {bauc <- NA}
    #SUBJECT DATA
    cat("\nSubject and covariate data.\n")
    flush.console()
    sdata <- matrix(scan(outfile,skip=toc[24],n=nsub*5,quiet=T,what="character"),nrow=nsub,ncol=5,byrow=T)
    sdata <- data.frame(id=checkID(sdata[,2]),nsub=1:nsub,age=as.numeric(sdata[,3]),
                        sex=sdata[,4],ht=as.numeric(sdata[,5]))
    #dose covariates
    dosecov <- cbind(rep(1:nsub,ndose),matrix(as.numeric(sub("D","E",scan(outfile,skip=toc[25],n=sum(ndose)*(2*ndrug+1+ncov),quiet=T,what=""))),nrow=sum(ndose),byrow=T))
    #outputs
    outputs <- matrix(as.numeric(sub("D","E",scan(outfile,skip=toc[26],n=sum(nobs)*numeqt*8,quiet=T,what=""))),ncol=8,byrow=T)
    
    #data filename
    mdata <- tail(scan(outfile,skip=toc[26],quiet=T,what="character",n=1+sum(nobs)*numeqt*8),1)
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
    
    outlist <- list(nsub=nsub,nactve=nactve,nvar=nvar,nofix=nofix,par=par,parfix=parfix,covnames=covnames,ab=ab,valfix=valfix,ndim=ndim,
                    indpts=indpts,icycst=icycst,icycmax=icycmax,icyctot=icyctot,converge=converge,ODEtol=ODEtol,prior=prior,
                    ERRmod=ERRmod,numeqt=numeqt,ndrug=ndrug,salt=salt,ndose=ndose,ncov=ncov,
                    nobs=nobs,nobsmax=nobsmax,numt=numt,corden=corden,pyjgx=pyjgx,ypredpop=ypredpop,
                    ypredbay=ypredbay,ttpred=ttpred,exx=exx,ypredpopt=ypredpopt,ilog=ilog,
                    iic=iic,imean=imean,isd=isd,iaddl=iaddl,igamlam=igamlam,blog=blog,bmean=bmean,
                    bsd=bsd,baddl=baddl,bauc=bauc,sdata=sdata,dosecov=dosecov,outputs=outputs,negflag=negflag,mdata=mdata) 
    
  }
  
  # Vernum==5 ---------------------------------------------------------------
  
  #if version 1.6
  if(vernum==5){
    #number of subjects
    nsub <- as.numeric(scan(outfile,quiet=T,skip=3,nlines=1,what="character",comment.char="#"))
    npost <- ifelse(nsub>100,100,nsub)
    NPdims <- scan(outfile,quiet=T,skip=3,nlines=15+npost,what="character",comment.char="#")
    #number of active grid points for population
    nactve <- as.numeric(NPdims[2])
    #number of active grid points for each subject
    add <- npost+2
    nactvepost <- as.numeric(NPdims[3:add])
    #number of random parameters
    nvar <- as.numeric(NPdims[add+1])
    #number of fixed parameters
    nofix <- as.numeric(NPdims[add+2])
    #number of dimensions (always 3 for mean, median, mode)
    ndim <- as.numeric(NPdims[add+3])
    #index of grid points
    indpts <- as.numeric(NPdims[add+4])
    #start cycle number
    icycst <- as.numeric(NPdims[add+5])
    #maximum cycles
    icycmax <- as.numeric(NPdims[add+6])
    #final cycle number
    icyctot <- as.numeric(NPdims[add+7])
    #convergence
    converge <- as.numeric(NPdims[add+8])
    #ODE tolerance
    ODEtol <- as.numeric(NPdims[add+9])
    #prior density
    prior <- NPdims[add+10]
    #assay error model
    ERRmod <- as.numeric(NPdims[add+11])
    #number of output equations
    numeqt <- as.numeric(NPdims[add+12])
    #number of drugs
    ndrug <- as.numeric(NPdims[add+13])
    #salt fraction for each drug
    salt <- scan(outfile,quiet=T,skip=18+npost,n=ndrug,comment.char="#")
    #number of dose events for each subjects
    ndose <- scan(outfile,quiet=T,skip=18+npost+ndrug,n=nsub,comment.char="#")
    #number of additional covariates
    ncov <- scan(outfile,quiet=T,skip=18+ndrug+npost+nsub,n=1,comment.char="#")
    #number of observations for each subject
    nobs <- scan(outfile,quiet=T,skip=19+ndrug+npost+nsub,n=nsub,comment.char="#")
    nobsmax <- max(nobs)
    #number of prediction times for each subject
    numt <- scan(outfile,quiet=T,skip=19+ndrug+npost+2*nsub,n=nsub,comment.char="#")
    #number of AUC blocks over all subjects
    nauc <- scan(outfile,quiet=T,skip=19+ndrug+npost+3*nsub,n=1,comment.char="#")
    #matrix with subj num, num of max times, and num of periods+1 for AUC tables
    if(nauc>0) {aucM <- matrix(scan(outfile,quiet=T,skip=22+ndrug+npost+3*nsub,n=nauc*3),nrow=nauc,ncol=3,byrow=T)} else aucM <- NA
    
    #get table of contents
    toc <- scan(outfile,quiet=T,skip=24+ndrug+npost+3*nsub+nauc,n=27,comment.char="#")
    #get random parameter names
    par <- scan(outfile,what="character",n=nvar,skip=toc[1],quiet=T)
    #get fixed parameter names
    parfix <- if(nofix>0) scan(outfile,what="character",skip=toc[2],n=nofix,quiet=T)
    #get initial ranges for random parameters
    ab <- matrix(as.numeric(sub("D","E",scan(outfile,n=nvar*2,skip=toc[3],quiet=T,what=""))),nrow=nvar,ncol=2,byrow=T)
    #get values for fixed parameters
    valfix <- if(nofix>0) scan(outfile,skip=toc[4],n=nofix,quiet=T)
    #get the covariate names
    if (ncov>0){
      covnames <- scan(outfile,what="character",skip=toc[5],n=ncov,quiet=T)
    } else {covnames <- NA}
    #get the population density
    corden <- matrix(as.numeric(sub("D","E",scan(outfile,skip=toc[6],n=nactve*(nvar+1),quiet=T,what=""))),nrow=nactve,ncol=nvar+1,byrow=T)
    #get the posterior densities
    postden <- array(dim=c(npost,max(nactvepost[1:npost]),nvar+1),
                     dimnames=list(subj=1:npost,
                                   nactvepost=1:max(nactvepost[1:npost]),
                                   density=c(par,"prob")))
    temp1 <-  as.numeric(sub("D","E",scan(outfile,skip=toc[7],n=sum(nactvepost[1:npost]*(nvar+1)),quiet=T,what="")))
    count <- 1
    cat("\nBaesian posterior densities for each subject.\n")
    flush.console()
    if(npost>1) pb <- txtProgressBar(min = 1, max = npost, style = 3)
    for (jsub in 1:npost){
      if(npost>1) setTxtProgressBar(pb,jsub)
      for (iactve in 1:nactvepost[jsub]){
        for (ivar in 1:(nvar+1)){
          postden[jsub,iactve,ivar]<-temp1[count]
          count <- count+1
        }
        #get probs for each posterior point
        postden[jsub,iactve,nvar+1] <- postden[jsub,iactve,nvar+1] * prod(ab[,2]-ab[,1]) / nactvepost[jsub]
      }
    }
    count <- 1
    #get predicted
    pyjgx <- matrix(as.numeric(sub("D","E",scan(outfile,skip=toc[8],n=nsub*nactve,quiet=T,what=""))),nrow=nsub,ncol=nactve,byrow=T)
    
    #get population predictions at observation times
    temp1 <- as.numeric(sub("D","E",scan(outfile,skip=toc[9],n=numeqt*sum(nobs)*3,quiet=T,what="")))
    
    #get posterior predictions at observation times
    temp2 <-as.numeric(sub("D","E",scan(outfile,skip=toc[10],n=numeqt*sum(nobs)*3,quiet=T,what="")))
    
    #flag negative pop or post preds 
    if (any(temp1<0,na.rm=T) | any(temp2<0,na.rm=T)){
      negflag <- T
    }
    
    ypredpop <- array(dim=c(nsub,numeqt,nobsmax,3))
    ypredbay <- array(dim=c(nsub,numeqt,nobsmax,3))
    count <- 1
    cat("\nPopulation and posterior predictions at each subject's observation times.\n")
    flush.console()
    if(nsub>1) pb <- txtProgressBar(min = 1, max = nsub, style = 3)
    for (jsub in 1:nsub){
      if(nsub>1) setTxtProgressBar(pb,jsub)
      for (ieq in 1:numeqt){
        for (iobs in 1:nobs[jsub]){
          for (icen in 1:3){
            ypredpop[jsub,ieq,iobs,icen]<-temp1[count]
            ypredbay[jsub,ieq,iobs,icen]<-temp2[count]
            count <- count+1
          }
        }
      }
    }
    count <- 1
    #get prediction times for each subject
    temp <-as.numeric(sub("D","E",scan(outfile,skip=toc[11],n=sum(numt),quiet=T,what="")))
    ttpred <- matrix(nrow=nsub,ncol=max(numt))
    cat("\nPrediction times (tpred) for each subject.\n")
    flush.console()
    if(nsub>1) pb <- txtProgressBar(min = 1, max = nsub, style = 3)
    for (jsub in 1:nsub){
      if(nsub>1) setTxtProgressBar(pb,jsub)
      for (j in 1:numt[jsub]){
        ttpred[jsub,j] <- temp[count]
        count <- count+1
      }
    }
    count <- 1
    #get population predictions at each ttpred time
    temp <-as.numeric(sub("D","E",scan(outfile,skip=toc[12],n=numeqt*sum(numt)*3,quiet=T,what="")))
    if(any(temp<0,na.rm=T)){
      temp[temp<0] <- 0
      negflag <- T
    }
    ypredpopt <- array(dim=c(nsub,numeqt,max(numt),3))
    cat("\nPopulation predictions for each subject and tpred.\n")
    flush.console()
    if(nsub>1) pb <- txtProgressBar(min = 1, max = nsub, style = 3)
    for (jsub in 1:nsub){
      if(nsub>1) setTxtProgressBar(pb,jsub)
      for (ieq in 1:numeqt){
        for (j in 1:numt[jsub]){
          for (icen in 1:3){
            ypredpopt[jsub,ieq,j,icen]<-temp[count]
            count <- count+1
          }
        }
      }
    }
    count <- 1 
    #get mean, median and mode of posterior values for each parameter in each subject
    exx <- array(data=as.numeric(sub("D","E",scan(outfile,skip=toc[13],n=nsub*3*nvar,quiet=T,what=""))),dim=c(nvar,3,nsub))
    
    #CYCLE INFORMATION
    cat("\nCycle information.\n")
    flush.console()
    if (icyctot > 0){
      #get log-likelihoods
      ilog <- as.numeric(sub("D","E",scan(outfile,skip=toc[14],n=icyctot,quiet=T,what="")))
      #get aic-bic
      iic <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[15],n=icyctot*2,quiet=T,what=""))),nrow=icyctot,ncol=2,byrow=T)
      #get means
      imean <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[16],n=icyctot*nvar,quiet=T,what=""))),nrow=icyctot,ncol=nvar,byrow=T)
      #get SDs
      if(nsub>1) {
        isd <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[17],n=icyctot*nvar,quiet=T,what=""))),nrow=icyctot,ncol=nvar,byrow=T)
      } else { isd <- matrix(NA,nrow=icyctot,ncol=nvar)}
      isd[isd==0] <- NA
      #get additional information
      iaddl <- array(data=as.numeric(sub("D","E",scan(outfile,skip=toc[18],n=icyctot*nvar*12,quiet=T,what=""))),dim=c(12,nvar,icyctot))
      #get gamlam values
      igamlam <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[19],n=icyctot,quiet=T,what=""))),ncol=1,byrow=T)
      igamlam[igamlam==-99] <- 1
      
    } else {
      ilog <- NA
      iic <- NA
      imean <- NA
      isd <- NA
      iaddl <- NA
      igamlam <- NA
    }
    
    #BAYESIAN INFORMATION
    cat("\nBayesian posterior parameters.\n")
    flush.console()
    #get log-likelihoods
    blog <- as.numeric(sub("D","E",scan(outfile,skip=toc[20],n=nsub,quiet=T,what="")))
    #get means
    bmean <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[21],n=nsub*nvar,quiet=T,what=""))),nrow=nsub,ncol=nvar,byrow=T)
    #get SDs
    bsd <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[22],n=nsub*nvar,quiet=T,what=""))),nrow=nsub,ncol=nvar,byrow=T)
    #get additional information
    baddl <- array(data=as.numeric(sub("D","E",scan(outfile,skip=toc[23],n=nsub*nvar*12,quiet=T,what=""))),dim=c(12,nvar,nsub))
    #get gamlam values
    if(nauc>0){
      bauc <- matrix(as.numeric(sub("D","E",scan(outfile,skip=toc[24],n=numeqt*5*sum(aucM[,3]),quiet=T,what=""))),nrow=numeqt*sum(aucM[,3]),ncol=5,byrow=T)
    } else {bauc <- NA}
    #SUBJECT DATA
    cat("\nSubject and covariate data.\n")
    flush.console()
    sdata <- matrix(scan(outfile,skip=toc[25],n=nsub*5,quiet=T,what="character"),nrow=nsub,ncol=5,byrow=T)
    sdata <- data.frame(id=checkID(sdata[,2]),nsub=1:nsub,age=as.numeric(sdata[,3]),
                        sex=sdata[,4],ht=as.numeric(sdata[,5]))
    #dose covariates
    dosecov <- cbind(rep(1:nsub,ndose),matrix(as.numeric(sub("D","E",scan(outfile,skip=toc[26],n=sum(ndose)*(2*ndrug+1+ncov),quiet=T,what=""))),nrow=sum(ndose),byrow=T))
    #outputs
    outputs <- matrix(as.numeric(sub("D","E",scan(outfile,skip=toc[27],n=sum(nobs)*numeqt*8,quiet=T,what=""))),ncol=8,byrow=T)
    
    #data filename
    mdata <- scan(outfile,skip=toc[27]+sum(nobs)*numeqt,quiet=T,what="character",n=1)
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
    
    outlist <- list(nsub=nsub,nactve=nactve,nvar=nvar,nofix=nofix,par=par,parfix=parfix,covnames=covnames,ab=ab,valfix=valfix,ndim=ndim,
                    indpts=indpts,icycst=icycst,icycmax=icycmax,icyctot=icyctot,converge=converge,ODEtol=ODEtol,prior=prior,
                    ERRmod=ERRmod,numeqt=numeqt,ndrug=ndrug,salt=salt,ndose=ndose,ncov=ncov,
                    nobs=nobs,nobsmax=nobsmax,numt=numt,corden=corden,postden=postden,pyjgx=pyjgx,ypredpop=ypredpop,
                    ypredbay=ypredbay,ttpred=ttpred,exx=exx,ypredpopt=ypredpopt,ilog=ilog,
                    iic=iic,imean=imean,isd=isd,iaddl=iaddl,igamlam=igamlam,blog=blog,bmean=bmean,
                    bsd=bsd,baddl=baddl,bauc=bauc,sdata=sdata,dosecov=dosecov,outputs=outputs,negflag=negflag,mdata=mdata) 
    
  }
  
  # Vernum==6 ---------------------------------------------------------------
  
  #if version 1.7
  if(vernum==6){
    #number of subjects
    nsub <- as.numeric(scan(outfile,quiet=T,skip=3,nlines=1,what="character",comment.char="#"))
    npost <- ifelse(nsub>100,100,nsub)
    NPdims <- scan(outfile,quiet=T,skip=3,nlines=15+npost,what="character",comment.char="#")
    #number of active grid points for population
    nactve <- as.numeric(NPdims[2])
    #number of active grid points for each subject
    add <- npost+2
    nactvepost <- as.numeric(NPdims[3:add])
    #number of random parameters
    nvar <- as.numeric(NPdims[add+1])
    #number of fixed parameters
    nofix <- as.numeric(NPdims[add+2])
    #number of dimensions (always 3 for mean, median, mode)
    ndim <- as.numeric(NPdims[add+3])
    #index of grid points
    indpts <- as.numeric(NPdims[add+4])
    #start cycle number
    icycst <- as.numeric(NPdims[add+5])
    #maximum cycles
    icycmax <- as.numeric(NPdims[add+6])
    #final cycle number
    icyctot <- as.numeric(NPdims[add+7])
    #convergence
    converge <- as.numeric(NPdims[add+8])
    #ODE tolerance
    ODEtol <- as.numeric(NPdims[add+9])
    #prior density
    prior <- NPdims[add+10]
    #assay error model
    ERRmod <- as.numeric(NPdims[add+11])
    #number of output equations
    numeqt <- as.numeric(NPdims[add+12])
    #number of drugs
    ndrug <- as.numeric(NPdims[add+13])
    #salt fraction for each drug
    salt <- scan(outfile,quiet=T,skip=18+npost,n=ndrug,comment.char="#")
    #number of dose events for each subjects
    ndose <- scan(outfile,quiet=T,skip=18+npost+ndrug,n=nsub,comment.char="#")
    #number of additional covariates
    ncov <- scan(outfile,quiet=T,skip=18+ndrug+npost+nsub,n=1,comment.char="#")
    #number of observations for each subject
    nobs <- scan(outfile,quiet=T,skip=19+ndrug+npost+nsub,n=nsub,comment.char="#")
    nobsmax <- max(nobs)
    #number of prediction times for each subject
    numt <- scan(outfile,quiet=T,skip=19+ndrug+npost+2*nsub,n=nsub,comment.char="#")
    
    #get table of contents
    toc <- scan(outfile,quiet=T,skip=19+ndrug+npost+3*nsub,n=26,comment.char="#")
    #get random parameter names
    par <- scan(outfile,what="character",n=nvar,skip=toc[1],quiet=T)
    #get fixed parameter names
    parfix <- if(nofix>0) scan(outfile,what="character",skip=toc[2],n=nofix,quiet=T)
    #get initial ranges for random parameters
    ab <- matrix(as.numeric(sub("D","E",scan(outfile,n=nvar*2,skip=toc[3],quiet=T,what=""))),nrow=nvar,ncol=2,byrow=T)
    #get values for fixed parameters
    valfix <- if(nofix>0) scan(outfile,skip=toc[4],n=nofix,quiet=T)
    #get the covariate names
    if (ncov>0){
      covnames <- scan(outfile,what="character",skip=toc[5],n=ncov,quiet=T)
    } else {covnames <- NA}
    #get the population density
    corden <- matrix(as.numeric(sub("D","E",scan(outfile,skip=toc[6],n=nactve*(nvar+1),quiet=T,what=""))),nrow=nactve,ncol=nvar+1,byrow=T)
    #get the posterior densities
    postden <- array(dim=c(npost,max(nactvepost[1:npost]),nvar+1),
                     dimnames=list(subj=1:npost,
                                   nactvepost=1:max(nactvepost[1:npost]),
                                   density=c(par,"prob")))
    temp1 <-  as.numeric(sub("D","E",scan(outfile,skip=toc[7],n=sum(nactvepost[1:npost]*(nvar+1)),quiet=T,what="")))
    count <- 1
    cat("\nBaesian posterior densities for each subject.\n")
    flush.console()
    if(npost>1) pb <- txtProgressBar(min = 1, max = npost, style = 3)
    for (jsub in 1:npost){
      if(npost>1) setTxtProgressBar(pb,jsub)
      for (iactve in 1:nactvepost[jsub]){
        for (ivar in 1:(nvar+1)){
          postden[jsub,iactve,ivar]<-temp1[count]
          count <- count+1
        }
      }
    }
    count <- 1
    #get predicted
    pyjgx <- matrix(as.numeric(sub("D","E",scan(outfile,skip=toc[8],n=nsub*nactve,quiet=T,what=""))),nrow=nsub,ncol=nactve,byrow=T)
    
    #get population predictions at observation times
    temp1 <- as.numeric(sub("D","E",scan(outfile,skip=toc[9],n=numeqt*sum(nobs)*3,quiet=T,what="")))
    
    #get posterior predictions at observation times
    temp2 <-as.numeric(sub("D","E",scan(outfile,skip=toc[10],n=numeqt*sum(nobs)*3,quiet=T,what="")))
    
    #flag negative pop or post preds 
    if (any(temp1<0,na.rm=T) | any(temp2<0,na.rm=T)){
      negflag <- T
    }
    
    ypredpop <- array(dim=c(nsub,numeqt,nobsmax,3))
    ypredbay <- array(dim=c(nsub,numeqt,nobsmax,3))
    count <- 1
    cat("\nPopulation and posterior predictions at each subject's observation times.\n")
    flush.console()
    if(nsub>1) pb <- txtProgressBar(min = 1, max = nsub, style = 3)
    for (jsub in 1:nsub){
      if(nsub>1) setTxtProgressBar(pb,jsub)
      for (ieq in 1:numeqt){
        for (iobs in 1:nobs[jsub]){
          for (icen in 1:3){
            ypredpop[jsub,ieq,iobs,icen]<-temp1[count]
            ypredbay[jsub,ieq,iobs,icen]<-temp2[count]
            count <- count+1
          }
        }
      }
    }
    count <- 1
    #get prediction times for each subject
    temp <-as.numeric(sub("D","E",scan(outfile,skip=toc[11],n=sum(numt),quiet=T,what="")))
    ttpred <- matrix(nrow=nsub,ncol=max(numt))
    cat("\nPrediction times (tpred) for each subject.\n")
    flush.console()
    if(nsub>1) pb <- txtProgressBar(min = 1, max = nsub, style = 3)
    for (jsub in 1:nsub){
      if(nsub>1) setTxtProgressBar(pb,jsub)
      for (j in 1:numt[jsub]){
        ttpred[jsub,j] <- temp[count]
        count <- count+1
      }
    }
    count <- 1
    #get population predictions at each ttpred time
    temp <-as.numeric(sub("D","E",scan(outfile,skip=toc[12],n=numeqt*sum(numt)*3,quiet=T,what="")))
    if(any(temp<0,na.rm=T)){
      temp[temp<0] <- 0
      negflag <- T
    }
    ypredpopt <- array(dim=c(nsub,numeqt,max(numt),3))
    cat("\nPopulation predictions for each subject and tpred.\n")
    flush.console()
    if(nsub>1) pb <- txtProgressBar(min = 1, max = nsub, style = 3)
    for (jsub in 1:nsub){
      if(nsub>1) setTxtProgressBar(pb,jsub)
      for (ieq in 1:numeqt){
        for (j in 1:numt[jsub]){
          for (icen in 1:3){
            ypredpopt[jsub,ieq,j,icen]<-temp[count]
            count <- count+1
          }
        }
      }
    }
    dimnames(ypredpopt) <- list(id=1:nsub,outeq=1:numeqt,time=ttpred[which(numt==max(numt))[1],],icen=c("mean","median","mode"))
    count <- 1 
    #get mean, median and mode of posterior values for each parameter in each subject
    exx <- array(data=as.numeric(sub("D","E",scan(outfile,skip=toc[13],n=nsub*3*nvar,quiet=T,what=""))),dim=c(nvar,3,nsub))
    
    #CYCLE INFORMATION
    cat("\nCycle information.\n")
    flush.console()
    if (icyctot > 0){
      #get log-likelihoods
      ilog <- as.numeric(sub("D","E",scan(outfile,skip=toc[14],n=icyctot,quiet=T,what="")))
      #get aic-bic
      iic <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[15],n=icyctot*2,quiet=T,what=""))),nrow=icyctot,ncol=2,byrow=T)
      dimnames(iic) <- list(cycle=1:icyctot,type=c("AIC","BIC"))
      #get means
      imean <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[16],n=icyctot*nvar,quiet=T,what=""))),nrow=icyctot,ncol=nvar,byrow=T)
      dimnames(imean) <- list(cycle=1:icyctot,par=par)
      #get SDs
      if(nsub>1) {
        isd <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[17],n=icyctot*nvar,quiet=T,what=""))),nrow=icyctot,ncol=nvar,byrow=T)
      } else { isd <- matrix(NA,nrow=icyctot,ncol=nvar)}
      isd[isd==0] <- NA
      dimnames(isd) <- list(cycle=1:icyctot,par=par)
      #get additional information
      iaddl <- array(data=as.numeric(sub("D","E",scan(outfile,skip=toc[18],n=icyctot*nvar*12,quiet=T,what=""))),dim=c(12,nvar,icyctot))
      dimnames(iaddl)=list(info=c("mode","skew","kurt","2.5%","25%","50%","75%","97.5%","SD_IQR","SD_95","meanSD_IQR_95","%scaledInfo"),
                           par=par,cycle=1:icyctot)
      #get gamlam values
      igamlam <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[19],n=icyctot,quiet=T,what=""))),ncol=1,byrow=T)
      igamlam[igamlam==-99] <- 1
      
    } else {
      ilog <- NA
      iic <- NA
      imean <- NA
      isd <- NA
      iaddl <- NA
      igamlam <- NA
    }
    
    #BAYESIAN INFORMATION
    cat("\nBayesian posterior parameters.\n")
    flush.console()
    #get log-likelihoods
    blog <- as.numeric(sub("D","E",scan(outfile,skip=toc[20],n=nsub,quiet=T,what="")))
    #get means
    bmean <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[21],n=nsub*nvar,quiet=T,what=""))),nrow=nsub,ncol=nvar,byrow=T)
    dimnames(bmean) <- list(nsub=1:nsub,par=par)
    #get SDs
    bsd <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[22],n=nsub*nvar,quiet=T,what=""))),nrow=nsub,ncol=nvar,byrow=T)
    dimnames(bsd) <- list(nsub=1:nsub,par=par)
    
    #get additional information
    baddl <- array(data=as.numeric(sub("D","E",scan(outfile,skip=toc[23],n=nsub*nvar*12,quiet=T,what=""))),dim=c(12,nvar,nsub))
    dimnames(baddl)=list(info=c("mode","skew","kurt","2.5%","25%","50%","75%","97.5%","SD_IQR","SD_95","meanSD_IQR_95","%scaledInfo"),
                         par=par,nsub=1:nsub)
    #SUBJECT DATA
    cat("\nSubject and covariate data.\n")
    flush.console()
    sdata <- matrix(scan(outfile,skip=toc[24],n=nsub*5,quiet=T,what="character"),nrow=nsub,ncol=5,byrow=T)
    sdata <- data.frame(id=checkID(sdata[,2]),nsub=1:nsub,age=as.numeric(sdata[,3]),
                        sex=sdata[,4],ht=as.numeric(sdata[,5]))
    #dose covariates
    dosecov <- cbind(rep(1:nsub,ndose),matrix(as.numeric(sub("D","E",scan(outfile,skip=toc[25],n=sum(ndose)*(2*ndrug+1+ncov),quiet=T,what=""))),nrow=sum(ndose),byrow=T))
    #outputs
    outputs <- data.frame(matrix(as.numeric(sub("D","E",scan(outfile,skip=toc[26],n=sum(nobs)*numeqt*8,quiet=T,what=""))),ncol=8,byrow=T))
    names(outputs) <- c("id","time","outeq","out","c0","c1","c2","c3")
    #data filename
    mdata <- scan(outfile,skip=toc[26]+sum(nobs)*numeqt,quiet=T,what="character",n=1)
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
    
    outlist <- list(nsub=nsub,nactve=nactve,nvar=nvar,nofix=nofix,par=par,parfix=parfix,covnames=covnames,ab=ab,valfix=valfix,ndim=ndim,
                    indpts=indpts,icycst=icycst,icycmax=icycmax,icyctot=icyctot,converge=converge,ODEtol=ODEtol,prior=prior,
                    ERRmod=ERRmod,numeqt=numeqt,ndrug=ndrug,salt=salt,ndose=ndose,ncov=ncov,
                    nobs=nobs,nobsmax=nobsmax,numt=numt,corden=corden,postden=postden,pyjgx=pyjgx,ypredpop=ypredpop,
                    ypredbay=ypredbay,ttpred=ttpred,exx=exx,ypredpopt=ypredpopt,ilog=ilog,
                    iic=iic,imean=imean,isd=isd,iaddl=iaddl,igamlam=igamlam,blog=blog,bmean=bmean,
                    bsd=bsd,baddl=baddl,sdata=sdata,dosecov=dosecov,outputs=outputs,negflag=negflag,mdata=mdata) 
    
  } #end version 1.7 (vernum==6)
  
  
  
  
  
  # Vernum = 7 --------------------------------------------------------------
  
  #if version 1.8
  if(vernum==7){
    #number of subjects
    nsub <- as.numeric(scan(outfile,quiet=T,skip=3,nlines=1,what="character",comment.char="#"))
    npost <- ifelse(nsub>100,100,nsub)
    NPdims <- scan(outfile,quiet=T,skip=3,nlines=16+npost,what="character",comment.char="#")
    #number of active grid points for population
    nactve <- as.numeric(NPdims[2])
    #number of active grid points for each subject
    add <- npost+2
    nactvepost <- as.numeric(NPdims[3:add])
    #number of random parameters
    nvar <- as.numeric(NPdims[add+1])
    #number of constant fixed parameters
    nofix <- as.numeric(NPdims[add+2])
    #number of random fixed parameters
    nranfix <- as.numeric(NPdims[add+3])
    #number of dimensions (always 3 for mean, median, mode)
    ndim <- as.numeric(NPdims[add+4])
    #index of grid points
    indpts <- as.numeric(NPdims[add+5])
    #start cycle number
    icycst <- as.numeric(NPdims[add+6])
    #maximum cycles
    icycmax <- as.numeric(NPdims[add+7])
    #final cycle number
    icyctot <- as.numeric(NPdims[add+8])
    #convergence
    converge <- as.numeric(NPdims[add+9])
    #ODE tolerance
    ODEtol <- as.numeric(NPdims[add+10])
    #prior density
    prior <- NPdims[add+11]
    #assay error model
    ERRmod <- as.numeric(NPdims[add+12])
    #number of output equations
    numeqt <- as.numeric(NPdims[add+13])
    #number of drugs
    ndrug <- as.numeric(NPdims[add+14])
    #salt fraction for each drug
    salt <- scan(outfile,quiet=T,skip=19+npost,n=ndrug,comment.char="#")
    #number of dose events for each subjects
    ndose <- scan(outfile,quiet=T,skip=19+npost+ndrug,n=nsub,comment.char="#")
    #number of additional covariates
    ncov <- scan(outfile,quiet=T,skip=19+ndrug+npost+nsub,n=1,comment.char="#")
    #number of observations for each subject
    nobs <- scan(outfile,quiet=T,skip=20+ndrug+npost+nsub,n=nsub,comment.char="#")
    nobsmax <- max(nobs)
    #number of prediction times for each subject
    numt <- scan(outfile,quiet=T,skip=20+ndrug+npost+2*nsub,n=nsub,comment.char="#")
    
    #get table of contents
    toc <- scan(outfile,quiet=T,skip=20+ndrug+npost+3*nsub,n=28,comment.char="#")
    #get random parameter names
    par <- scan(outfile,what="character",n=nvar,skip=toc[1],quiet=T)
    #get constant fixed parameter names
    parfix <- if(nofix>0) scan(outfile,what="character",skip=toc[2],n=nofix,quiet=T)
    #get random fixed parameter names
    parranfix <- if(nranfix>0) scan(outfile,what="character",skip=toc[3],n=nranfix,quiet=T)
    #get initial ranges for random parameters
    ab <- matrix(as.numeric(sub("D","E",scan(outfile,n=nvar*2,skip=toc[4],quiet=T,what=""))),nrow=nvar,ncol=2,byrow=T)
    #get values for constant fixed parameters
    valfix <- if(nofix>0) scan(outfile,skip=toc[5],n=nofix,quiet=T)
    #get values for random fixed parameters
    valranfix <- if(nranfix>0) scan(outfile,skip=toc[6],n=nranfix,quiet=T)
    #get the covariate names
    if (ncov>0){
      covnames <- scan(outfile,what="character",skip=toc[7],n=ncov,quiet=T)
    } else {covnames <- NA}
    #get the population density
    corden <- matrix(as.numeric(sub("D","E",scan(outfile,skip=toc[8],n=nactve*(nvar+1),quiet=T,what=""))),nrow=nactve,ncol=nvar+1,byrow=T)
    #get the posterior densities
    postden <- array(dim=c(npost,max(nactvepost[1:npost]),nvar+1),
                     dimnames=list(subj=1:npost,
                                   nactvepost=1:max(nactvepost[1:npost]),
                                   density=c(par,"prob")))
    temp1 <-  as.numeric(sub("D","E",scan(outfile,skip=toc[9],n=sum(nactvepost[1:npost]*(nvar+1)),quiet=T,what="")))
    count <- 1
    cat("\nBaesian posterior densities for each subject.\n")
    flush.console()
    if(npost>1) pb <- txtProgressBar(min = 1, max = npost, style = 3)
    for (jsub in 1:npost){
      if(npost>1) setTxtProgressBar(pb,jsub)
      for (iactve in 1:nactvepost[jsub]){
        for (ivar in 1:(nvar+1)){
          postden[jsub,iactve,ivar]<-temp1[count]
          count <- count+1
        }
      }
    }
    count <- 1
    #get predicted
    pyjgx <- matrix(as.numeric(sub("D","E",scan(outfile,skip=toc[10],n=nsub*nactve,quiet=T,what=""))),nrow=nsub,ncol=nactve,byrow=T)
    
    #get population predictions at observation times
    temp1 <- as.numeric(sub("D","E",scan(outfile,skip=toc[11],n=numeqt*sum(nobs)*3,quiet=T,what="")))
    
    #get posterior predictions at observation times
    temp2 <-as.numeric(sub("D","E",scan(outfile,skip=toc[12],n=numeqt*sum(nobs)*3,quiet=T,what="")))
    
    #flag negative pop or post preds 
    if (any(temp1<0,na.rm=T) | any(temp2<0,na.rm=T)){
      negflag <- T
    }
    
    ypredpop <- array(dim=c(nsub,numeqt,nobsmax,3))
    ypredbay <- array(dim=c(nsub,numeqt,nobsmax,3))
    count <- 1
    cat("\nPopulation and posterior predictions at each subject's observation times.\n")
    flush.console()
    if(nsub>1) pb <- txtProgressBar(min = 1, max = nsub, style = 3)
    for (jsub in 1:nsub){
      if(nsub>1) setTxtProgressBar(pb,jsub)
      for (ieq in 1:numeqt){
        for (iobs in 1:nobs[jsub]){
          for (icen in 1:3){
            ypredpop[jsub,ieq,iobs,icen]<-temp1[count]
            ypredbay[jsub,ieq,iobs,icen]<-temp2[count]
            count <- count+1
          }
        }
      }
    }
    count <- 1
    #get prediction times for each subject
    temp <-as.numeric(sub("D","E",scan(outfile,skip=toc[13],n=sum(numt),quiet=T,what="")))
    ttpred <- matrix(nrow=nsub,ncol=max(numt))
    cat("\nPrediction times (tpred) for each subject.\n")
    flush.console()
    if(nsub>1) pb <- txtProgressBar(min = 1, max = nsub, style = 3)
    for (jsub in 1:nsub){
      if(nsub>1) setTxtProgressBar(pb,jsub)
      for (j in 1:numt[jsub]){
        ttpred[jsub,j] <- temp[count]
        count <- count+1
      }
    }
    count <- 1
    #get population predictions at each ttpred time
    temp <-as.numeric(sub("D","E",scan(outfile,skip=toc[14],n=numeqt*sum(numt)*3,quiet=T,what="")))
    if(any(temp<0,na.rm=T)){
      temp[temp<0] <- 0
      negflag <- T
    }
    ypredpopt <- array(dim=c(nsub,numeqt,max(numt),3))
    cat("\nPopulation predictions for each subject and tpred.\n")
    flush.console()
    if(nsub>1) pb <- txtProgressBar(min = 1, max = nsub, style = 3)
    for (jsub in 1:nsub){
      if(nsub>1) setTxtProgressBar(pb,jsub)
      for (ieq in 1:numeqt){
        for (j in 1:numt[jsub]){
          for (icen in 1:3){
            ypredpopt[jsub,ieq,j,icen]<-temp[count]
            count <- count+1
          }
        }
      }
    }
    dimnames(ypredpopt) <- list(id=1:nsub,outeq=1:numeqt,time=ttpred[which(numt==max(numt))[1],],icen=c("mean","median","mode"))
    count <- 1 
    #get mean, median and mode of posterior values for each parameter in each subject
    exx <- array(data=as.numeric(sub("D","E",scan(outfile,skip=toc[15],n=nsub*3*nvar,quiet=T,what=""))),dim=c(nvar,3,nsub))
    
    #CYCLE INFORMATION
    cat("\nCycle information.\n")
    flush.console()
    if (icyctot > 0){
      #get log-likelihoods
      ilog <- as.numeric(sub("D","E",scan(outfile,skip=toc[16],n=icyctot,quiet=T,what="")))
      #get aic-bic
      iic <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[17],n=icyctot*2,quiet=T,what=""))),nrow=icyctot,ncol=2,byrow=T)
      dimnames(iic) <- list(cycle=1:icyctot,type=c("AIC","BIC"))
      #get means
      imean <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[18],n=icyctot*nvar,quiet=T,what=""))),nrow=icyctot,ncol=nvar,byrow=T)
      dimnames(imean) <- list(cycle=1:icyctot,par=par)
      #get SDs
      if(nsub>1) {
        isd <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[19],n=icyctot*nvar,quiet=T,what=""))),nrow=icyctot,ncol=nvar,byrow=T)
      } else { isd <- matrix(NA,nrow=icyctot,ncol=nvar)}
      isd[isd==0] <- NA
      dimnames(isd) <- list(cycle=1:icyctot,par=par)
      #get additional information
      iaddl <- array(data=as.numeric(sub("D","E",scan(outfile,skip=toc[20],n=icyctot*nvar*12,quiet=T,what=""))),dim=c(12,nvar,icyctot))
      dimnames(iaddl)=list(info=c("mode","skew","kurt","2.5%","25%","50%","75%","97.5%","SD_IQR","SD_95","meanSD_IQR_95","%scaledInfo"),
                           par=par,cycle=1:icyctot)
      #get gamlam values
      igamlam <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[21],n=icyctot,quiet=T,what=""))),ncol=1,byrow=T)
      igamlam[igamlam==-99] <- 1
      
    } else {
      ilog <- NA
      iic <- NA
      imean <- NA
      isd <- NA
      iaddl <- NA
      igamlam <- NA
    }
    
    #BAYESIAN INFORMATION
    cat("\nBayesian posterior parameters.\n")
    flush.console()
    #get log-likelihoods
    blog <- as.numeric(sub("D","E",scan(outfile,skip=toc[22],n=nsub,quiet=T,what="")))
    #get means
    bmean <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[23],n=nsub*nvar,quiet=T,what=""))),nrow=nsub,ncol=nvar,byrow=T)
    dimnames(bmean) <- list(nsub=1:nsub,par=par)
    #get SDs
    bsd <- matrix(data=as.numeric(sub("D","E",scan(outfile,skip=toc[24],n=nsub*nvar,quiet=T,what=""))),nrow=nsub,ncol=nvar,byrow=T)
    dimnames(bsd) <- list(nsub=1:nsub,par=par)
    
    #get additional information
    baddl <- array(data=as.numeric(sub("D","E",scan(outfile,skip=toc[25],n=nsub*nvar*12,quiet=T,what=""))),dim=c(12,nvar,nsub))
    dimnames(baddl)=list(info=c("mode","skew","kurt","2.5%","25%","50%","75%","97.5%","SD_IQR","SD_95","meanSD_IQR_95","%scaledInfo"),
                         par=par,nsub=1:nsub)
    #SUBJECT DATA
    cat("\nSubject and covariate data.\n")
    flush.console()
    sdata <- matrix(scan(outfile,skip=toc[26],n=nsub*5,quiet=T,what="character"),nrow=nsub,ncol=5,byrow=T)
    sdata <- data.frame(id=checkID(sdata[,2]),nsub=1:nsub,age=as.numeric(sdata[,3]),
                        sex=sdata[,4],ht=as.numeric(sdata[,5]))
    #dose covariates
    dosecov <- cbind(rep(1:nsub,ndose),matrix(as.numeric(sub("D","E",scan(outfile,skip=toc[27],n=sum(ndose)*(2*ndrug+1+ncov),quiet=T,what=""))),nrow=sum(ndose),byrow=T))
    #outputs
    outputs <- data.frame(matrix(as.numeric(sub("D","E",scan(outfile,skip=toc[28],n=sum(nobs)*numeqt*8,quiet=T,what=""))),ncol=8,byrow=T))
    names(outputs) <- c("id","time","outeq","out","c0","c1","c2","c3")
    #data filename
    mdata <- scan(outfile,skip=toc[28]+sum(nobs)*numeqt,quiet=T,what="character",n=1)
    if(length(mdata)==0) mdata <- "NA"
    
    #summary
    cat("Output file:",outfile,"\n")
    cat("Random parameters:",par,"\n")
    if(nranfix==0){cat("There were no fixed but unknown parameters.\n")
    } else {cat("Fixed unknown parameters:",parranfix,"\n")}
    if(nofix==0){cat("There were no constant parameters.\n")
    } else {cat("Constant parameters:",parfix,"\n")}
    cat("Number of analyzed subjects:",nsub,"\n")
    cat("Number of output equations:",numeqt,"\n")
    cat("Additional covariates:",covnames,"\n")
    if (negflag){ cat("WARNING: There were negative or non-real pop/post predictions.\n")}
    coninterp <- switch(1+converge,"The run did not converge before the last cycle.","The run converged.","","WARNING: The run ended with a Hessian Error.")
    cat(coninterp,"\n")
    
    outlist <- list(nsub=nsub,nactve=nactve,nvar=nvar,nranfix=nranfix,nofix=nofix,par=par,parranfix=parranfix,parfix=parfix,covnames=covnames,ab=ab,valranfix=valranfix,valfix=valfix,ndim=ndim,
                    indpts=indpts,icycst=icycst,icycmax=icycmax,icyctot=icyctot,converge=converge,ODEtol=ODEtol,prior=prior,
                    ERRmod=ERRmod,numeqt=numeqt,ndrug=ndrug,salt=salt,ndose=ndose,ncov=ncov,
                    nobs=nobs,nobsmax=nobsmax,numt=numt,corden=corden,postden=postden,pyjgx=pyjgx,ypredpop=ypredpop,
                    ypredbay=ypredbay,ttpred=ttpred,exx=exx,ypredpopt=ypredpopt,ilog=ilog,
                    iic=iic,imean=imean,isd=isd,iaddl=iaddl,igamlam=igamlam,blog=blog,bmean=bmean,
                    bsd=bsd,baddl=baddl,sdata=sdata,dosecov=dosecov,outputs=outputs,negflag=negflag,mdata=mdata) 
    
  } #end version 1.8 (vernum==7)
  
  class(outlist) <- "NPAG"
  return(outlist)
}
