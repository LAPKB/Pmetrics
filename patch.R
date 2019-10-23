#' \code{PMwrk2csv} will convert old style, single drug working copy files into
#'  a single .csv matrix file.
#'
#' This function will determine if the working copy files are old and convert them.
#' New, multi-drug working copy files will be ignored. IDs will be suffixed with
#' .1 to .9 for <10 subjects, .01 to .99 for <100 subjects and .001 to .999 for <1000 subjects,
#' as needed to ensure unique ID numbers.
#'
#' @title Convert Old .wrk Files to .csv Matrix File
#' @param prefix The alphabetic prefix of the working copy files to be converted,
#'  as a character vector.
#' @param ext The extension of the working copy files files, if it exists.
#'  Does not have to be specified.
#' @param nsub The number of subjects, or working copy files to read.
#' @return A new file will be created with the name equal to \code{prefix} and
#'  an extension of \dQuote{csv}.
#' @author Michael Neely

PMwrk2csv <- function(prefix,ext=NULL,nsub){
  if(!is.null(ext)){ext <- paste(".",ext,sep="")}
  new <- data.frame()
  oldFlag <- F
  for (i in 1:nsub){
    append <- 0
    fmtStr <- paste("%0",8-nchar(prefix),"d",sep="")
    filename <- paste(prefix,sprintf(fmtStr,i),ext,sep="")
    data <- readLines(filename)
    #clean up leading spaces and trailing CRs
    
    data <- gsub("^[[:blank:]]+","",data)
    data <- gsub("[[:space:]]+$","",data)
    
    
    if (length(grep("RATES",data)>0)){oldFlag <- T}
    idLine <- grep("CHART NUMBER",data)
    idMatch <- regexpr("[[:digit:]]+",data[idLine])
    id <- substr(data[idLine],start=idMatch,stop=idMatch+attr(idMatch,"match.length")-1)
    id2 <- paste(id,".",paste(rep(0,(nchar(nsub)-nchar(i))),collapse=""),i,sep="")
    id <- id2
    if (oldFlag){
      age <- as.numeric(data[8])
      male <- ifelse(data[9]=="M",1,0)
      height <- as.numeric(data[10])
      dosegap <- 5
      ratesLine <- grep("RATES",data)
      ratesMatch <- regexpr("[[:digit:]]+",data[ratesLine])
      rates <- as.numeric(substr(data[ratesLine],start=ratesMatch,stop=ratesMatch+attr(ratesMatch,"match.length")-1))
      bolusMatch <- regexpr("[[:digit:]]+",data[ratesLine+1])
      bolus <- as.numeric(substr(data[ratesLine+1],start=bolusMatch,stop=bolusMatch+attr(bolusMatch,"match.length")-1))
      dosesMatch <- regexpr("[[:digit:]]+",data[ratesLine+2])
      doses <- as.numeric(substr(data[ratesLine+2],start=dosesMatch,stop=dosesMatch+attr(dosesMatch,"match.length")-1))
      doseMatrixStart <- ratesLine+dosegap
      while(length(grep("[[:digit:]]",data[doseMatrixStart]))==0){
        dosegap <- dosegap+1
        doseMatrixStart <- ratesLine+dosegap
      }
      
      numoutLine <- ratesLine+dosegap+doses
      nobsLine <- numoutLine+1
      obsMatrixStart <- nobsLine+1
      dosesMatrix <- matrix(scan(file=filename,skip=doseMatrixStart-1,n=doses*(rates+bolus+1),quiet=T),nrow=doses,byrow=T)
      numout <- scan(file=filename,skip=numoutLine-1,n=1,quiet=T)
      nobs <- scan(file=filename,skip=nobsLine-1,n=1,quiet=T)
      obsMatrix <- matrix(scan(file=filename,skip=obsMatrixStart-1,n=nobs*(numout+1),quiet=T),nrow=nobs,byrow=T)
      #covariates
      if(rates>1){
        ncov <- rates-1
        covar <- vector("character",ncov)
        covLine <- grep("COVARIATE NAMES",data)  
        for (j in 1:(rates-1)){
          covMatch <- regexpr("[[:alpha:]]+",data[covLine+1+j])
          covar[j] <- substr(data[covLine+1+j],start=covMatch,stop=covMatch+attr(covMatch,"match.length")-1)
        }
      } else {ncov <- 0}
      assayLine <- grep("ASSAY COEFFICIENTS FOLLOW",data)
      if(length(assayLine)>0){
        assayError <- matrix(scan(file=filename,skip=assayLine,n=4*numout,quiet=T),nrow=numout,byrow=T)
      } else {
        assayError <- matrix(rep(NA,4*numout),nrow=numout,byrow=T)
      }
      #build matrix for subject i
      temp <- matrix(nrow=doses+(nobs*numout),ncol=14+ncov)
      
      #convert doses
      k <- 1
      for (j in 1:doses){
        if(rates>0 && dosesMatrix[j,2]!=0){ #iv dose
          dur <- dosesMatrix[j+1,1]-dosesMatrix[j,1]
          amt <- dosesMatrix[j,2]*dur
          temp[k,1] <- id #id
          temp[k,2] <- 1 #evid
          temp[k,3] <- dosesMatrix[j,1] #time
          temp[k,4] <- dur #dur
          temp[k,5] <- amt #dose
          temp[k,6] <- NA #addl
          temp[k,7] <- NA #ii
          temp[k,8] <- 1 #input
          temp[k,9] <- NA #out
          temp[k,10] <- NA #outeq
          temp[k,11] <- NA #C0
          temp[k,12] <- NA #C1
          temp[k,13] <- NA #C2
          temp[k,14] <- NA #C3          
          if(rates>1) temp[k,15:(13+rates)] <- dosesMatrix[j,3:(1+rates)] #covariates
          k <- k+1
        } else {
          if (bolus!=0){ #oral dose
            if(dosesMatrix[j,rates+2]!=0){
              dur <- 0
              amt <- dosesMatrix[j,rates+2]
              temp[k,1] <- id #id
              temp[k,2] <- 1 #evid
              temp[k,3] <- dosesMatrix[j,1] #time
              temp[k,4] <- dur #dur
              temp[k,5] <- amt #dose
              temp[k,6] <- NA #addl
              temp[k,7] <- NA #ii
              temp[k,8] <- 1 #input
              temp[k,9] <- NA #out
              temp[k,10] <- NA #outeq
              temp[k,11] <- NA #C0
              temp[k,12] <- NA #C1
              temp[k,13] <- NA #C2
              temp[k,14] <- NA #C3          
              if(rates>1) temp[k,15:(13+rates)] <- dosesMatrix[j,3:(1+rates)] #covariates
              k <- k+1
            }
          }
        }
      }
      #convert observations
      for (j in 1:numout){
        for (l in 1:nobs){
          temp[k,1] <- id #id
          temp[k,2] <- 0 #evid
          temp[k,3] <- obsMatrix[l,1] #time
          temp[k,4] <- NA #dur
          temp[k,5] <- NA #dose
          temp[k,6] <- NA #addl
          temp[k,7] <- NA #ii
          temp[k,8] <- 1 #input
          temp[k,9] <- obsMatrix[l,1+j] #out
          temp[k,10] <- j #outeq
          temp[k,11] <- assayError[j,1] #C0
          temp[k,12] <- assayError[j,2] #C1
          temp[k,13] <- assayError[j,3] #C2
          temp[k,14] <- assayError[j,4] #C3          
          if(rates>1) temp[k,15:(13+rates)] <- NA #covariates
          k <- k+1
        }
      }
      
      temp <- temp[!is.na(temp[,1]),] #remove any rows with missing id
      temp <- apply(temp,2,as.numeric) #ensure everything is numeric
      temp <- data.frame(temp) #convert to data.frame
      if(rates>1) {names(temp) <- c("id","evid","time","dur","dose","addl","ii","input","out","outeq","c0","c1","c2","c3",covar)
      } else {names(temp) <- c("id","evid","time","dur","dose","addl","ii","input","out","outeq","c0","c1","c2","c3")}
      temp <- temp[order(temp$time),]
      temp$age <- age
      temp$male <- male
      temp$height <- height
    } else {stop("This function is only for old working copy (single drug) files.\n")}
    new <- rbind(new,temp)
  }
  #write the new file
  PMwriteMatrix(data=new,filename=paste(prefix,".csv",sep=""),override=T)
  class(new) <- c("PMmatrix","data.frame")
  return(new)
}


