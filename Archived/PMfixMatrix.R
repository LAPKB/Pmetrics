#' This function will attempt to automatically fix some common errors in a .csv file or a data frame containing a
#' previously loaded .csv file (the output of \code{\link{PMreadMatrix}}
#' which would cause the run to fail.
#' 
#' @title Correct Errors in a Pmetrics .csv Matrix File 
#' @param x The name of a Pmetrics .csv matrix file in the current working directory,
#' the full path to one not in the current working directory, or a data.frame containing 
#' the output of a previous \code{\link{PMreadMatrix}} command.
#' @return \code{PMfixMatrix} returns an edited data frame and reports on the changes made.
#' @author Michael Neely
#' @seealso \code{\link{PMcheck}}

PMfixMatrix <- function(x){
  if(is.character(x)) {
    data <- tryCatch(suppressWarnings(PMreadMatrix(x,quiet=T)),error = function(e) return(invisible(e)))
  } else {data <- x}
  #if(!inherits(err,"PMerr")) stop("Please supply a Pmetrics error object made by PMcheck.\n")
  err <- PMcheck(x,quiet=T)
  report <- NA
  # 1 - fix first 14 columns
  if(length(grep("FAIL",err$colorder$msg))>0){
    fixedcols <- c("id","evid","time","dur","dose","addl","ii","input","out","outeq","c0","c1","c2","c3")
    t <- tolower(names(data))
    PMcols <- match(fixedcols,t)
    if(any(is.na(PMcols))) {
      misscols <- fixedcols[is.na(PMcols)]
      report <- c(report,paste("Cannot fix columns; the following are missing: ",paste(misscols,collapse="'', '"),".",sep=""))
    } else {
      covcols <- (1:ncol(data))[!(1:ncol(data)) %in% PMcols]
      data <- data[,c(PMcols,covcols)]
      report <- c(report,paste("Columns are now ordered appropriately."))      
    }
  }
  # 2 - Make sure ids and cols are 11 char or less
  if(length(grep("FAIL",err$maxchar$msg))>0){
    names(data) <- substr(names(data),1,11)
    report <- c(report,paste("Column names are all 11 characters or fewer."))    
  }
  # 3 - remove observations at time 0
  if(length(grep("FAIL",err$obsT0$msg))>0){
    data <- data[-err$obsT0$results,]
    report <- c(report,paste("Observations at time 0 have been removed."))    
    err <- PMcheck(data,quiet=T)
  }
  # 4 - check for NA observations (should be -99)
  if(length(grep("FAIL",err$obsMiss$msg))>0){
    data <- data[err$obsMiss$results,"out"] < -99
    report <- c(report,paste("Missing observations for evid=0 have been replaced with -99."))    
    err <- PMcheck(data,quiet=T)
  }
  # 5 - check for complete dose records
  if(length(grep("FAIL",err$doseComp$msg))>0){
    report <- c(report,paste("Dose records (evid=1 or evid=4) must have time, duration, dose and input; addl and ii should be '.' if not needed.  Run PMcheck and fix manually."))    
  }
  # 6 - check for complete observation records
  if(length(grep("FAIL",err$obsComp$msg))>0){
    report <- c(report,paste("Observation records (evid=0) must have time, out, and outeq. Run PMcheck and fix manually."))    
  }
  # 7 - flag evid!=1 as first event
  if(length(grep("FAIL",err$evid1$msg))>0){
    report <- c(report,paste("The first event for every subject must be a dose (evid=1). Run PMcheck and fix manually."))    
  }
  # 8 - insert dummy doses of 0 for those missing time=0 first events
  if(length(grep("FAIL",err$T0$msg))>0){
    T0 <- data[err$T0$results,]
    T0$time <- 0; T0$evid <- 1; T0$dose <- 0
    data <- rbind(data,T0)
    data <- data[order(data$id,data$time),]
    report <- c(report,paste("Subjects with first time > 0 have had a dummy dose of 0 inserted at time 0."))    
    err <- PMcheck(data,quiet=T)
  }
  #9 - remove trailing dose events
  if(length(grep("FAIL",err$obsLast$msg))>0){
    while(!is.na(err$obsLast$results[1])){
      data <- data[-err$obsLast$results,]
      err <- PMcheck(data,quiet=T)
    }
    report <- c(report,paste("Doses without following observations have been removed."))    
  }
  #10 - alert for missing covariate data
  if(length(grep("FAIL",err$covT0$msg))>0){
    report <- c(report,paste("All covariates must have values for each subject's first event.  Run PMcheck and fix manually."))    
  }
  #11 - change non-numeric to numeric covariates
  if(length(grep("FAIL",err$covNumeric$msg))>0){
    covcols <- which(names(data) %in% err$covNumeric$results)
    data[,covcols] <- lapply(covcols,function(x) as.numeric(factor(data[,x])))
    report <- c(report,paste("Non-numeric covariates have been converted to numeric."))    
  }
  
  print(report[-1])
  return(data)
}