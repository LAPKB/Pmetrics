#' Parses the output of the Pmetrics simulator
#'
#' For \code{file} specification \dQuote{?} will be matched by just a single numeral or character; \dQuote{*} will be
#' matched by any number of consecutive alphanumeric characters.  Examples include \code{file='simout1.txt,simout2.txt,simout3.txt'},
#' \code{file='simout?.txt'} and \code{file='sim*.txt'}.All three will find the files simout1.txt,
#' simout2.txt, and simout3.txt in the working directory. The second example would also find simout4.txt, etc.  The third 
#' example would also find sim_1.txt if that existed. 
#' Note that to combine simulator output files, the numbers of simulated profiles may differ.
#' The number of outputs and times of observations also may differ, although combining these may lead to
#' strange plots since not all profiles have the same observations. 
#' For parallel execution, the function requires packages 'doParallel' and 'foreach'. If not installed, it will try to install, failing that it will run in serial mode. 
#'
#' @title Parse Pmetrics Simulator Output
#' @param file An output file or files of the simulator in the current working directory, or the full
#' pathname to the file.  To load and combine multiple outputs, specify files separated by commas
#' or using wild cards.  See details. 
#' @param include A vector of files to include in the parsing.  For example, if you used a wild
#' card in the \code{file} argument, such as \dQuote{simout?.txt}, which returned four files:
#' simout1.txt, simout2.txt, simout3.txt and simout4.txt, and you wished to only parse the first
#' and fourth file, specify \code{include=c(1,4)}.
#' @param exclude See the discussion for \code{include}, but this will exclude specified files.
#' @param combine Boolean parameter, default \code{False}, which specifies whether you wish to combine
#' the parsed files into a single PMsim object.  This can be useful for making visual predictive
#' checks, for example.  If \code{combine=F}, and multiple files are parsed, then the return object 
#' will be a list of PMsim objects, which can be plotted or otherwise accessed using standard list
#' referencing, e.g. simlist[[1]], simlist[[2]], etc.
#' @param silent Suppress messages
#' @param parallel Runs in parallel mode.  Defaults to true if multiple files are to be parsed, otherwise false.
#' Can be overridden by specifying \code{TRUE} or \code{FALSE}.
#' @return If one file is parsed or multiple files are parsed and combined, the return will be a list with five items, of class \emph{PMsim}.
#' If multiple files are parsed and not combined, then the return will be a list of \emph{PMsim} objects.
#' \item{obs }{An data frame of simulated observations with 4 columns: id, time, out, outeq.
#' \emph{id} is the number of the simulated subject, which will have a unique ending appended
#' if simulations are combined, such that \emph{id} will become x.y with x being the simulated profile
#' number and y being the simulation template number.  \emph{time} is the time of the simulated
#' output, \emph{out} of output equation number \emph{outeq}.}
#' \item{amt }{An data frame of simulated amounts with 4 columns: id, time, out, comp.
#' \emph{id} is the number of the simulated subject, which will have a unique ending appended
#' if simulations are combined, such that \emph{id} will become x.y with x being the simulated profile
#' number and y being the simulation template number.  \emph{time} is the time of the simulated
#' amount, \emph{out} in compartment  number \emph{comp}.}
#' \item{parValues }{A datframe of the simulated parameter values, combined across files as necessary}
#' \item{totalSets}{The total number of parmeter sets simulated, which may be greater than the number of rows in \code{parValues} if some sets
#' were discarded for being outside specified limits.  For more than one file parsed, this will the total number in all files.}
#' \item{totalMeans}{The means of each simulated parameter based on all profiles in a given file (even those discarded for exceeding limits).
#' For more than one file parsed, this will be the weighted averages for all simulations.}
#' \item{totalCov}{The covariances of the simulated parameter sets based on all profiles in a given file (even those discarded for exceeding limits).
#' For more than one file parsed, this will be the weighted averages for all simulations.}
#' A plot method exists in \code{\link{plot.PMsim}} for \emph{PMsim} objects.
#' @author Michael Neely
#' @seealso \code{\link{SIMrun}}

SIMparse <- function(file,include,exclude,combine=F,silent=F, parallel){
  processfile <- function(n) {
    out <- readLines(allfiles[n])
    nsim <- as.numeric(strparse("[[:digit:]]+",out[grep(" THE NO. OF SIMULATED SUBJECTS",out)]))
    nout <- as.numeric(strparse("[[:digit:]]+",out[grep(" THE NO. OF OUTPUT EQUATIONS",out)]))
    nobs <- as.numeric(strparse("[[:digit:]]+",out[grep(" VALUES FOR EACH OUTPUT EQUATION",out)]))
    i <- grep("CONTAIN THE SIMULATED OBSERVED$",out)
    times <- as.numeric(scan(allfiles[n],skip=i+6,n=nobs+1,what="character",quiet=T)[-1])
    
    
    #get compartment amounts and outeq concentrations for each output
    #places for compartment amounts
    amtlines <- grep("COMPARTMENT NO",out)+1
    #number of compartments
    ncomp <- length(amtlines)
    
    
    #places for observations
    obslines <- grep("OUTPUT EQUATION NO",out)+1
    #skip observed assay noise
    obslines <- obslines[-(1:nout)]
    
    #id is a block of 1:nsim repeated for each observation, all repeated for each compartment
    #time is a block of the times repeated for each subject, all repeated for each compartment
    #amt all amounts
    #outeq is a block of 1:nout, repeated for nsim*nobs
    if(ncomp>0){
      amt <- data.frame(id=rep(rep(1:nsim,each=nobs),ncomp),
                        time=rep(rep(times,nsim),ncomp),
                        out=c(unlist(sapply(1:ncomp,function(a) {
                          scan(allfiles[n],skip=amtlines[a],n=nsim*(nobs+1),quiet=T)[-(seq(0,(nsim-1)*(nobs+1),(nobs+1))+1)]
                        }))),
                        comp=rep(1:ncomp,each=nsim*nobs))
    } else {amt <- NA}
    
    #id is a block of 1:nsim repeated for each observation, all repeated for each output
    #time is a block of the times repeated for each subject, all repeated for each output
    #out is all observations
    #outeq is a block of 1:nout, repeated for nsim*nobs
    obs <- data.frame(id=rep(rep(1:nsim,each=nobs),nout),
                      time=rep(rep(times,nsim),nout),
                      out=c(unlist(sapply(1:nout,function(a) {
                        scan(allfiles[n],skip=obslines[a],n=nsim*(nobs+1),quiet=T)[-(seq(0,(nsim-1)*(nobs+1),(nobs+1))+1)]
                      }))),
                      outeq=rep(1:nout,each=nsim*nobs))
    obs$out[obs$out==-99] <- NA

    
    #get simulated parameter values
    
    i<-grep("PARAMETER VALUES FOR ALL THE SUBJECTS.",out)
    parNames <- unlist(strsplit(out[i+2]," +"))[-1]
    parValues <- t(sapply(strsplit(out[(i+3):(i+2+nsim)]," +"),
                          function(x) as.numeric(x[-1])))
    parValues <- data.frame(parValues)
    names(parValues) <- parNames
    names(parValues)[names(parValues)=="SUBJ."] <- "id"
    
    
    #get means and covariances of entire simulated set
    
    i<-grep("BECAUSE OF PARAMETER BOUNDARY RESTRICTIONS",out)
    if(length(i)>0){
      totalSets <- as.numeric(scan(allfiles[1],what="character",skip=i,nlines=1,quiet=T,strip.white=T)[1])
      i<-grep("SAMPLE MEANS OF ALL PARAMETER DATA ARE",out)
      totalMeans <- as.numeric(scan(allfiles[n],what="character",skip=i,nlines=1,quiet=T,strip.white=T))
      i<-grep("SAMPLE COV. MATRIX OF ALL PARAMETER DATA IS",out)
      totalCov <- suppressWarnings(as.numeric(scan(allfiles[n],what="character",skip=i,nlines=length(totalMeans),quiet=T,strip.white=T)))
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
    
    return(list(obs=obs,amt=amt,parValues=parValues,totalSets=totalSets,totalMeans=totalMeans,totalCov=totalCov))
  } # end of processfile function
  
  #  starttime <- proc.time()
  if (missing(file)){
    cat("Please provide filename of Pmetrics simulation output file(s).\n")
    return()
  }
  #extract pattern from strings
  strparse<-function(pattern,x) {
    match <- regexpr(pattern,x, perl = T) # perl=T required for the lookahead
    start <- match[1]
    stop <- match[1]+attr(match,"match.length")-1
    return(substr(x,start,stop))
  }
  
  #separate files if more than one
  files <- unlist(strsplit(file,","))
  #remove leading and trailing spaces
  files <- sub("^[[:blank:]]*","",files)
  files <- sub("[[:blank:]]*$","",files)

  #check that wildcard-generated list does not include outdated files 
  #if out-of-order files are found, the user has the option to include them, exclude them, or abort
  nfilepar <- length(files)
  tobeignored <- c()
  for (n in 1:nfilepar) { # cycle through individual filename parameters passed
    if (length(grep("\\?",files[[n]]))>0 | length(grep("\\*",files[[n]])) >0) {
      workfiles <- Sys.glob(files[[n]])
      worksimnum <- as.numeric(sapply(workfiles,function(x) strparse("([[:digit:]]+)(?!.*[[:digit:]])",x)))
      workfiles <- workfiles[order(worksimnum)]
      if(!missing(include)){
        workfiles <- workfiles[include]
      }
      if(!missing(exclude)){
        workfiles <- workfiles[-exclude]
      }
      nworkfiles <- length(workfiles)
      if (nworkfiles>1) for (n in 2:nworkfiles) {
        if (file.mtime(workfiles[n]) < file.mtime(workfiles[n-1])) {
          cat("\nFile ", workfiles[n], " is older than ", workfiles[n-1], ". Possible leftover from previous SIMrun?", sep = "")
          ans <- readline(cat("\nWhat would you like to do?\n1) include and continue\n2) ignore files after ", workfiles[n-1],"\n3) abort function", sep=""))
          if (ans == 1) {
            next
          } else if (ans==2) {
            tobeignored <- append(tobeignored, workfiles[n:nworkfiles])
            break
          } else stop("Function aborted.", call.=F)
        } # end of IF older
      } # end of check list of files generated by wildcard block
    } # end of IF wildcard used block
  } # end of filecheck cycle

  allfiles <- unique(Sys.glob(files)) # unique to exclude duplicates
  simnum <- as.numeric(sapply(allfiles,function(x) strparse("([[:digit:]]+)(?!.*[[:digit:]])",x)))
  # new reg exp matches the last number in filename rather than the first; "run5out1.txt" will now return 1 rather than 5
  allfiles <- allfiles[order(simnum)]
  if(!missing(include)){
    allfiles <- allfiles[include]
  }
  if(!missing(exclude)){
    allfiles <- allfiles[-exclude]
  }
  allfiles <- setdiff(allfiles, tobeignored) # delete files that are to be ignored from the date check
  
  nfiles <- length(allfiles)
  if (nfiles==0) {stop("No files found.\n")}
  if (missing(parallel)){
    if(nfiles>1){parallel <- T} else {parallel <- F}
  } 
  
  if (parallel){
    if (length(grep("doParallel", installed.packages()[, 1])) == 0) {
      install.packages("doParallel", repos = "http://cran.cnr.Berkeley.edu", dependencies = T)
    }
    if (length(grep("foreach", installed.packages()[, 1])) == 0) {
      install.packages("foreach", repos = "http://cran.cnr.Berkeley.edu", dependencies = T)
    }
    doParallel.installed <- require(doParallel, warn.conflicts = F, quietly = T)
    foreach.installed <- require(foreach, warn.conflicts = F, quietly = T)
    if (!doParallel.installed | !foreach.installed) {
      warning("Required package failed to be installed. SIMparse will run in serial mode.\n", call. = F, immediate. = !silent)
      parallel <- F
    } else {
      no_cores <- detectCores()
    }
  }

  #initialize return objects
  simlist <- list()
  if(!silent){
    cat(paste("\nProcessing ",nfiles," simulated data file(s)",sep=""))
    if (parallel) cat(" in parallel on ",no_cores, " cores.", sep = "")
    cat("\n")
    flush.console()
  }
  if(!silent & !parallel) {pb <- txtProgressBar(min = 0, max = nfiles, style = 3)}
  
  if (parallel) {
    cl <- makeCluster(no_cores)
    registerDoParallel(cl)
    simlist <- foreach(n = 1:nfiles, .verbose = F) %dopar% {processfile(n)}
    stopCluster(cl)
  } else {
    for (n in 1:nfiles) {
      simlist[[n]] <- processfile(n)
      if(!silent) {setTxtProgressBar(pb, n)}
    }
    if(!silent) {close(pb)}
  }
  
  #combine obs if requested
  if(combine & nfiles>1){
    if(!silent){
      cat("\nCombining files...\n")
      flush.console()
    }
    #make unique id values of "id.simnumber" for each set
    simlist <- lapply(seq_along(simlist),function(x) {simlist[[x]]$obs$id <- paste(simlist[[x]]$obs$id,sprintf("%02d",x),sep=".");simlist[[x]]})
    
    obs <- do.call(rbind,lapply(simlist,function(x) x$obs))
    amt <- do.call(rbind,lapply(simlist,function(x) x$amt))
    parValues <- do.call(rbind,lapply(simlist,function(x) x$parValues))
    totalSets <- sum(sapply(simlist,function(x) x$totalSets))
    totalMeans <- do.call(rbind,lapply(simlist,function(x) x$totalMeans*x$totalSets))
    totalMeans <- apply(totalMeans,2,function(x) sum(x)/totalSets)
    totalCov <- Reduce("+",lapply(simlist,function(x) x$totalCov*x$totalSets))/totalSets
    simlist <- list(obs=obs,amt=amt,parValues=parValues,totalSets=totalSets,totalMeans=totalMeans,totalCov=totalCov)
  }
  
  #return simlist   
  if(nfiles>1){ #more than one file
    if(combine) { #combined
      class(simlist) <- c("PMsim","list")
      message <- paste("\nThe following files were successfully parsed and combined: ",paste(allfiles,collapse=", "),"\n",sep="")
    } else {      #not combined
      simlist <- lapply(simlist,function(x) {class(x) <- c("PMsim","list");x})
      message <- paste("\nThe following files were successfully parsed as a list: ",paste(allfiles,collapse=", "),"\n",sep="")
    }
  } else {  #only one file
    simlist <- simlist[[1]]
    class(simlist) <- c("PMsim","list")
    message <- paste(paste("\nThe following file was successfully parsed: ",paste(allfiles,collapse=", "),"\n",sep=""))
  } 
  
  Rprof(NULL)
  #  runningtime <- proc.time()-starttime
  if(!silent) cat(message)
  #  if(!silent) cat(message,"Running time: ", runningtime[3], sep="")
  return(simlist)
  
}
