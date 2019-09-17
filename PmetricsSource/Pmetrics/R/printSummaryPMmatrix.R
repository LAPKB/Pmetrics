#' Print the summary of a Pmetrics PMmatrix object
#'
#' Summarize the raw data used for a Pmetrics run.
#'
#' @title Summarize Covariates and Bayesian Posterior Parameter Values
#' @method print summary.PMmatrix
#' @param x A summary.PMmatrix object made by \code{\link{summary.PMmatrix}} 
#' @return A formatted printing of a \emph{summary.PMmatrix} object
#' @author Michael Neely
#' @seealso \code{\link{summary.PMmatrix}}
#' @export

print.summary.PMmatrix <- function(x){
  # order of objects
  #   nsub
  #   ndrug
  #   numeqt
  #   nobsXouteq
  #   missObsXouteq
  #   ncov
  #   ndoseXid
  #   nobsXid
  #   doseXid 
  #   obsXid 
  #   formula 

  cat(paste("\nNumber of subjects:",x$nsub,"\n"))
  cat(paste("Number of inputs:",x$ndrug,"\n"))
  cat(paste("Number of outputs:",x$numeqt,"\n"))
  for(i in 1:x$numeqt){
    cat(paste("Total number of observations (outeq ",i,"): ",x$nobsXouteq[i],", with ",x$missObsXouteq[i]," (",sprintf("%.3f",x$missObsXouteq[i]/x$nobsXouteq[i]),"%) missing\n",sep=""))
    
  }
  cat(paste("Number of covariates:",x$ncov,"\n"))
  cat(paste("\nTHE FOLLOWING ARE MEAN (SD), MIN TO MAX\n",paste(rep("-",75),collapse=""),"\n",sep=""))
  cat("\nINPUTS\n")
  for(i in 1:x$ndrug){
    if(x$ndrug==1){
      cat(paste("Number of doses per subject (input ",i,"): ",sprintf("%.3f",mean(x$ndoseXid,na.rm=T))," (",sprintf("%.3f",sd(x$ndoseXid,na.rm=T)),"), ",sprintf("%.3f",min(x$ndoseXid,na.rm=T))," to ",sprintf("%.3f",max(x$ndoseXid,na.rm=T)),"\n",sep=""))
      cat(paste("Dose per subject (input ",i,"): ",sprintf("%.3f",mean(unlist(x$doseXid),na.rm=T))," (",sprintf("%.3f",sd(unlist(x$doseXid),na.rm=T)),"), ",sprintf("%.3f",min(unlist(x$doseXid),na.rm=T))," to ",sprintf("%.3f",max(unlist(x$doseXid),na.rm=T)),"\n",sep=""))
    } else {
      cat(paste("Number of doses per subject (input ",i,"): ",sprintf("%.3f",mean(x$ndoseXid[,i],na.rm=T))," (",sprintf("%.3f",sd(x$ndoseXid[,i],na.rm=T)),"), ",sprintf("%.3f",min(ndoseXid[,i],na.rm=T))," to ",sprintf("%.3f",max(x$ndoseXid[,i],na.rm=T)),"\n",sep=""))
      cat(paste("Dose (input ",i,"): ",sprintf("%.3f",mean(unlist(x$doseXid[,i]),na.rm=T))," (",sprintf("%.3f",sd(unlist(x$doseXid[,i]),na.rm=T)),"), ",sprintf("%.3f",min(unlist(x$doseXid[,i]),na.rm=T))," to ",sprintf("%.3f",max(unlist(x$doseXid[,i]),na.rm=T)),"\n",sep=""))
    }
  }
  cat("\nOUTPUTS\n")
  for(i in 1:x$numeqt){
    if(x$numeqt==1){
      nobs <- unlist(x$nobsXid)
      obs <- unlist(x$obsXid)
    } else {
      nobs <- unlist(x$nobsXid[,i])
      obs <- unlist(x$obsXid[,i])
    }
    obs <- obs[obs!=-99]
    cat(paste("Number of obs per subject (outeq ",i,"): ",sprintf("%.3f",mean(nobs,na.rm=T))," (",sprintf("%.3f",sd(nobs,na.rm=T)),"), ",sprintf("%.3f",min(nobs,na.rm=T))," to ",sprintf("%.3f",max(nobs,na.rm=T)),"\n",sep=""))
    cat(paste("Observation per subject (outeq ",i,"): ",sprintf("%.3f",mean(obs,na.rm=T))," (",sprintf("%.3f",sd(obs,na.rm=T)),"), ",sprintf("%.3f",min(obs,na.rm=T))," to ",sprintf("%.3f",max(obs,na.rm=T)),"\n",sep=""))
  }
  if(x$ncov>0){
    cat("\nCOVARIATES\n")
    for(i in 1:x$ncov){
      cat(paste(x$covnames[i],": ",sprintf("%.3f",mean(unlist(x$cov[[i]]),na.rm=T))," (",sprintf("%.3f",sd(unlist(x$cov[[i]]),na.rm=T)),"), ",sprintf("%.3f",min(unlist(x$cov[[i]]),na.rm=T))," to ",sprintf("%.3f",max(unlist(x$cov[[i]]),na.rm=T)),"\n",sep=""))
    }
  }
  
  if(!is.null(x$formula)){
    cat(paste("\nFormula\n",paste(rep("-",75),collapse=""),"\n",sep=""))
    print(x$formula)
  }
  cat(paste(paste(rep("-",75),collapse=""),"\nNote: See help(summary.PMmatrix) for accessing specific items by name.\n",sep=""))
  

} #end function
