#' Parses the output of the Pmetrics simulator
#'
#' For \code{file} specification \dQuote{?} will be matched by just a single numeral or character; \dQuote{*} will be
#' matched by any number of consecutive alphanumeric characters.  Examples include \code{file='simout1.txt,simout2.txt,simout3.txt'},
#' \code{file='simout?.txt'} and \code{file='sim*.txt'}.  All three will find the files simout1.txt,
#' simout2.txt, and simout3.txt in the working directory.  The second example would also find simout4.txt, etc.  The third
#' example would also find sim_1.txt if that existed.  Note that to combine simulator output files, the number of 
#' outputs and times of observations must be the same, although the numbers of simulated profiles may differ.  This
#' function is generally to allow the combination of profiles simulated from the same sampling template,
#' but differing by a covariate or dosing regimen.
#'
#' @title Parse Pmetrics Simulator Output
#'
#' @param file An output file or files of the simulator in the current working directory, or the full
#' pathname to the file.  To load and combine multiple outputs, specify files separated by commas
#' or using wild cards.  See details. 
#' @param silent Suppress messages
#' @return A list with five items, of class \emph{PMsim}
#' \item{obs }{An array of simulated observations with 3 dimensions: number of outputs,
#' number of simulated profiles, and number of simulated output times.  If more than one file
#' has been parsed and combined, the total number of profiles will be the sum of the profiles in each file.}
#' \item{parValues }{A datframe of the simulated parameter values, combined across files as necessary}
#' \item{totalSets}{The total number of parmeter sets simulated, which may be greater than the number of rows in \code{parValues} if some sets
#' were discarded for being outside specified limits.  For more than one file parsed, this will be a list, one
#' for each file.}
#' \item{totalMeans}{The means of each simulated parameter based on all profiles in a given file (even those discarded for exceeding limits).
#' For more than one file parsed, this will be a list, one for each file.}
#' \item{totalCov}{The covariances of the simulated parameter sets based on all profiles in a given file (even those discarded for exceeding limits).
#' For more than one file parsed, this will be a list, one for each file.}
#' A plot method exists in \code{\link{plot.PMsim}} for \emph{PMsim} objects.
#' @author Michael Neely
#' @seealso \code{\link{SIMrun}}

SIMparse <- function(file,silent=F){
  if (missing(file)){
    cat("Please provide filename of Pmetrics simulation output file(s).\n")
    return()
  }
  #separate files if more than one
  files <- unlist(strsplit(file,","))
  #remove leading and trailing spaces
  files <- sub("^[[:blank:]]*","",files)
  files <- sub("[[:blank:]]*$","",files)
  allfiles <- Sys.glob(files)
  
  if (length(allfiles)==0) {stop("No files found.\n")}
  
  strparse<-function(pattern,x) {
    match <- regexpr(pattern,x)
    start <- match[1]
    stop <- match[1]+attr(match,"match.length")-1
    return(substr(x,start,stop))
  }
  
  #initialize return objects
  all.obs <- NA
  all.parValues <- NA
  all.totalSets <- NA
  all.totalMeans <- NA
  all.totalCov <- NA
  badfiles <- 0
  for(thisfile in allfiles){  
    out <- readLines(thisfile)
    nsim <- as.numeric(strparse("[[:digit:]]+",out[grep(" THE NO. OF SIMULATED SUBJECTS",out)]))
    nout <- as.numeric(strparse("[[:digit:]]+",out[grep(" THE NO. OF OUTPUT EQUATIONS",out)]))
    i <- grep("CONTAIN THE SIMULATED OBSERVED$",out)
    nobs <- as.numeric(strparse("[[:digit:]]+",out[i+1]))
    times <- as.numeric(scan(thisfile,what="character",skip=i+6,n=nobs+1,quiet=T)[-1])
    concs <- array(dim=c(nout,nsim,nobs),dimnames=list(output=1:nout,subj=1:nsim,time=times))
    
    #get concentrations for each output
    
    j <- grep("OUTPUT EQUATION NO",out)+1
    j <- j[-(1:nout)]
    
    for (k in 1:nout){
      concs[k,,] <- matrix(scan(thisfile,n=nsim*(nobs+1),skip=j[k],quiet=T),ncol=nobs+1,byrow=T)[,-1]
    }
    
    #combine with previous if there
    if(all(is.na(all.obs))) {
      all.obs <- concs
    } else {
      all.nsim <- dim(all.obs)[2]+dim(concs)[2]
      if(!identical(times,as.numeric(dimnames(all.obs)$time))){
        cat(paste(thisfile,"has different observation times than previous files and could not be combined.\n"))
        badfiles <- c(badfiles,which(allfiles==thisfile))          
        next
      } else {temp <- array(dim=c(nout,all.nsim,nobs),dimnames=list(output=1:nout,subj=1:all.nsim,time=times))}
      for (k in 1:nout){
        temp[k,,] <- rbind(all.obs[k,,],concs[k,,])
      }
      
      all.obs <- temp
    }
    
    #get simulated parameter values
    
    i<-grep("PARAMETER VALUES FOR ALL THE SUBJECTS.",out)
    parNames <- scan(thisfile,what="character",skip=i+1,nlines=1,quiet=T)
    parValues <- matrix(scan(thisfile,skip=i+2,nlines=nsim,quiet=T),nrow=nsim,ncol=length(parNames),byrow=T)
    parValues <- data.frame(parValues)
    names(parValues) <- parNames
    names(parValues)[names(parValues)=="SUBJ."] <- "id"
    
    if(all(is.na(all.parValues))) {
      all.parValues <- parValues
    } else {
      if(ncol(parValues)!=ncol(all.parValues)){
        cat(paste(thisfile,"has different parameters than previous files and was not combined.\n"))
        badfiles <- c(badfiles,which(allfiles==thisfile))
        next
      } else {
        all.parValues <- rbind(all.parValues,parValues)
      }
    }
  }   
    #get means and covariances of entire simulated set, which will be the same for all templates
    
    i<-grep("BECAUSE OF PARAMETER BOUNDARY RESTRICTIONS",out)
    if(length(i)>0){
      totalSets <- as.numeric(scan(thisfile,what="character",skip=i,nlines=1,quiet=T,strip.white=T)[1])
      i<-grep("SAMPLE MEANS OF ALL PARAMETER DATA ARE",out)
      totalMeans <- as.numeric(scan(thisfile,what="character",skip=i,nlines=1,quiet=T,strip.white=T))
      i<-grep("SAMPLE COV. MATRIX OF ALL PARAMETER DATA IS",out)
      totalCov <- suppressWarnings(as.numeric(scan(thisfile,what="character",skip=i,nlines=length(totalMeans),quiet=T,strip.white=T)))
      NArows <- which(is.na(totalCov))
      NArows <- c(NArows,NArows+1)
      totalCov <- totalCov[-NArows]
      mat <- matrix(NA,nrow=length(totalMeans),ncol=length(totalMeans))
      mat[lower.tri(mat,diag=T)] <- totalCov
      mat[upper.tri(mat,diag=T)] <- totalCov
      totalCov <- t(mat)
    } else {
      totalSets <- nsim
      if(length(parValues)>2){
        totalMeans <- apply(parValues[,-1],2,mean)
        totalCov <- cov(parValues[,-1],method="pearson")
      } else {
        totalMeans <- parValues[1,2]
        totalCov <- NA
      }
    }
    
    names(totalMeans) <- parNames[-1]
    if(length(parValues)>2) dimnames(totalCov) <- list(parNames[-1],parNames[-1])
    
    if(all(is.na(all.totalSets))) {all.totalSets <- list(totalSets)} else {all.totalSets <- c(all.totalSets,list(totalSets))}
    if(all(is.na(all.totalMeans))) {all.totalMeans <- list(totalMeans)} else {all.totalMeans <- c(all.totalMeans,list(totalMeans))}
    if(all(is.na(all.totalCov))) {all.totalCov <- list(totalCov)} else {all.totalCov <- c(all.totalCov,list(totalCov))}
    
#  }  
  #return output  
  
  output <- list(obs=all.obs,parValues=all.parValues,totalSets=all.totalSets,totalMeans=all.totalMeans,totalCov=all.totalCov)
  class(output) <- c("PMsim","list")
  if(length(badfiles)==1) {goodfiles <- allfiles} else {goodfiles <- allfiles[-badfiles]}
  message <- switch(letters[1+length(goodfiles)],
                    a="\nNo files were successfully parsed.\n",
                    b=paste(paste("\nThe following file was successfully parsed:\n",paste(goodfiles,collapse="\n"),"\n",sep="")),
                    paste("\nThe following files were successfully parsed and combined:\n",paste(goodfiles,collapse="\n"),"\n",sep=""))
  if(!silent) cat(message)
  return(output)
  
}

