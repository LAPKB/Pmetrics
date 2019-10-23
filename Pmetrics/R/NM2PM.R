#' \code{NM2PM} will convert NONMEM .csv data files to Pmetrics csv data files.
#'
#' The format of NONMEM and Pmetrics data .csv files are similar, but not quite identical.
#' A major difference is that the order of the columns are fixed in Pmetrics (not including covariates),
#' while they are user-determined in NONMEM, and specified in a control (.ctl) file.
#' 
#' A list of other differences follows by data item.
#' \itemize{
#' \item ID This item is the same in both formats and is required.
#' \item EVID This is the same in both formats but is not required in NONMEM.  Doses have an EVID
#' of 1 and observations 0.  EVID=4 (dose/time reset) is the same in Pmetrics and NONMEM. 
#' EVID=2 (other event) and EVID=3 (dose reset) are not directly supported in Pmetrics, but if included
#' in a NONMEM file, will be converted into covariate values.  Specifically the value in the CMT variable will
#' be the covariate value for EVID=2, while for EVID=3, the covariate will be 1 at the time of the EVID=3 entry
#' and 0 othewise.  This allows for handling of these events in the Pmetrics model file using conditional statements.
#' \item DATE Pmetrics does not use dates, but will convert all NONMEM dates and times into relative times.
#' \item TIME Pmetrics uses relative times (as does NONMEM), but the NONMEM pre-processor will convert clock times
#' to relative times, as does \code{NM2PM}.
#' \item RATE NONMEM RATE items are converted by this function to Pmetrics DURation values.
#' \item AMT becomes DOSE in Pmetrics
#' \item ADDL is supported in both formats.  However, if NONMEM files contain an SS flag, it will be
#' incorporated as ADDL=-1 according to Pmetrics style.
#' \item II is the same in both formats.
#' \item INPUT in Pmetrics is similar to CMT in NONMEM for doses.  
#' \item DV in NONMEM becomes OUT in Pmetrics.  Ensure that the units of OUT are consistent with the
#' units of DOSE.
#' \item OUTEQ In Pmetrics, this is roughly equivalent to CMT in NONMEM for observation events.  
#' The lowest CMT value for any observation becomes OUTEQ=1; the next lowest becomes OUTEQ=2, etc.
#' \item SS Steady state dosing is incorporated into Pmetrics as ADDL=-1.
#' \item MDV Missing DV in NONMEM become OUT=-99 in Pmetrics.
#' \item Covariates These are copied from NONMEM to Pmetrics.  Note that Pmetrics does not allow
#' missing covariates at time 0 for each subject.
#' \item DROP Items marked as DROP in the NONMEM control file will not be included in the Pmetric data file.
#' }
#' It is strongly suggested to run \code{\link{PMcheck}} on the returned object for final adjusting.
#'
#' @title Convert NONMEM to Pmetrics Data Files
#' @param data The name and extension of a NONMEM data (e.g. .csv) file in the working directory, or the full path to a file.
#' @param ctl  The name and extension of a NONMEM control (e.g. .ctl) file in the working directory, or the full path to a file.
#' @return A Pmetrics style PMmatrix data.frame.
#' @author Michael Neely
#' @seealso \code{\link{PMcheck}}, \code{\link{PMwriteMatrix}}, \code{\link{PMwrk2csv}}

NM2PM <- function(data,ctl){
  
  if(length(grep("chron",installed.packages()[,1]))==0){
    install.packages("chron",repos="http://cran.cnr.Berkeley.edu",dependencies=T)
  }
  chron.installed <- require(chron)
  if(!chron.installed) stop("Error: connect to internet and re-run NM2PM to download and install chron package.\n")
  
  msg <- "NONMEM file conversion report:\n"
  #check data file name
  while(!file.exists(data)) {
    data <- readline(paste("The data file",shQuote(paste(getwd(),data)),"does not exist.\nEnter another filename or 'end' to quit: \n")) 
    if (tolower(data)=="end") {msg <- c(msg,"No data file specified.\n");cat(msg);return(invisible(NULL))}
  }
  
  #check ctl file name
  while(!file.exists(ctl)) {
    ctl <- readline(paste("The NONMEM control file",shQuote(paste(getwd(),ctl)),"does not exist.\nEnter another filename or 'end' to quit: \n")) 
    if (tolower(ctl)=="end") {msg <- c(msg,"No control file specified.\n");cat(msg);return(invisible(NULL))}
  }
  
  #get the data file contents
  dataDF <- read.table(data,header=T,na.strings=".",comment.char="#",sep=",",as.is=T)
  #C can also be a comment line in NONMEM, so find lines starting with C and remove
  Clines <- grep("^C",dataDF[,1])
  if(length(Clines)>0){
    dataDF <- dataDF[-Clines,]
  }
  numcol <- ncol(dataDF)
  
  #get the columns from ctl
  ctlTxt <- readLines(ctl)
  #find $INPUT line and read the items
  inputLine <- grep("\\$INPUT",ctlTxt)
  if(length(inputLine)>0){
    inputs <- scan(ctl,skip=inputLine-1,n=numcol+1,what="character",quiet=T,comment.char=";")[-1]
  } else {stop(paste("No \\$INPUT line found in ",ctl,"\n",sep=""))}
  
  #find  columns
  findCol <- function(item){
    found <- grep(paste("^",item,sep=""),inputs)
    if(length(found)==0){ 
      found <- grep(paste("^[[:alpha:]]{1,4} *= *",item,sep=""),inputs)
      if(length(found)==0) found <- NA
    }
    if(length(found)>1){ #we found multiple so narrow down
      found <- grep(paste("^",item,"$|^",item,"=|=",item,"$",sep=""),inputs)
    }
    return(found)
  }
  
  keys <- c("ID","EVID","DATE","TIME","RATE","AMT","ADDL","II","SS","DV","MDV","CMT")
  nkeys <- length(keys)
  
  colNums <- lapply(1:nkeys,function(x) findCol(keys[x]))
  names(colNums) <- keys
  
  #check for crucial missing 
  if(is.na(colNums$ID))   stop("There does not appear to be an ID column in the data.\n")
  if(is.na(colNums$TIME)) stop("There does not appear to be an TIME column in the data.\n")
  if(is.na(colNums$AMT)) {stop("There does not appear to be an AMT column in the data.\n")} else {dataDF[,colNums$AMT] <- as.numeric(dataDF[,colNums$AMT])}
  if(is.na(colNums$DV)) {stop("There does not appear to be a DV column in the data.\n")} else {dataDF[,colNums$DV] <- as.numeric(dataDF[,colNums$DV])}
  #missing EVID is recoverable
  if(is.na(colNums$EVID)){
    dataDF$EVID <- sapply(dataDF[,colNums$AMT],function(x) c(1,0)[1+as.numeric(is.na(x) || x==0)])
    colNums$EVID <- which(names(dataDF)=="EVID")
    inputs <- c(inputs,"EVID")
  } else {dataDF[,colNums$EVID] <- as.numeric(dataDF[,colNums$EVID])}
  #CMT is recoverable
  if(is.na(colNums$CMT)){
    dataDF$CMT <- ifelse(dataDF[,colNums$EVID]==0,1,NA)
    colNums$CMT <- which(names(dataDF)=="CMT")
    inputs <- c(inputs,"CMT")
  } else {dataDF[,colNums$CMT] <- as.numeric(dataDF[,colNums$CMT])}
  #RATE is recoverable
  if(is.na(colNums$RATE)) {
    msg <- c(msg,"There does not appear to be a RATE column in the data...setting DUR=0 (bolus).\n")
    dataDF$RATE <- ifelse(dataDF[,colNums$EVID]==0,NA,0)
    colNums$RATE <- which(names(dataDF)=="RATE")
  } else {dataDF[,colNums$RATE] <- as.numeric(dataDF[,colNums$RATE])}
  
  #figure out date format
  DATEchoices <- c("DATE","DAT1","DAT2","DAT3")
  DATEcol <- sapply(1:4,function(x) findCol(DATEchoices[x]))
  if(!all(is.na(DATEcol))){
    #get the date type
    dateType <- which(!is.na(DATEcol))
    DATEcol <- DATEcol[dateType]
    #get the separator
    dateSepPos <- unlist(gregexpr("[[:punct:]]",dataDF[1,DATEcol]))
    nDateItems <- length(dateSepPos)
    if(nDateItems>0){
      dateSep <- substr(dataDF[1,DATEcol],dateSepPos,dateSepPos)
      if(nDateItems==1){ #x/y
        dateFormat <- switch(dateType,
                             paste("m","d","y",sep=dateSep),
                             paste("d","m","y",sep=dateSep),
                             paste("m","d","y",sep=dateSep),
                             paste("d","m","y",sep=dateSep))
        dataDF[,DATEcol] <- paste(dataDF[,DATEcol],"00",dateSep)
        msg <- c(msg,"A date column was found and assumed to contain month and day only.\n")
      }
      if(nDateItems==2){ #x/y/z
        dateFormat <- switch(dateType,
                             paste("m","d","y",sep=dateSep),
                             paste("d","m","y",sep=dateSep),
                             paste("y","m","d",sep=dateSep),
                             paste("y","d","m",sep=dateSep))
        msg <- c(msg,paste("A date column was found with a format of ",dateFormat,".\n",sep=""))
      }
    } else {
      dataDF[,DATEcol] <- paste("01",dataDF[,DATEcol],"00","/")
      dateFormat <- "m/d/y"
      msg <- c(msg,"A date column was found and assumed to contain day only.\n")
    }  
  } else {dateFormat <- NULL} #no date column
  
  #figure out time
  TIMEcol <- colNums$TIME
  #conversion functions
  clock2Decimal <- function(time){
    parts <- unlist(strsplit(time,":"))
    return(as.numeric(parts[1])+as.numeric(parts[2])/60)
  }
  decimal2Clock <- function(time){
    time <- as.numeric(time)
    return(paste(sprintf("%02i",floor(time)),sprintf("%02i",round(60*(time-floor(time)),0)),sep=":"))
  }
  #look for TIME column
  if(!is.na(TIMEcol)){
    colons <- grep(":",dataDF[,TIMEcol])
    if(length(colons)>0){ #clock format times present
      if(is.null(dateFormat)){ # no date so convert any hh:mm to h.h
        dataDF[colons,TIMEcol] <- sapply(dataDF[colons,TIMEcol],clock2Decimal)
        clockTime <- F
      } else { #there is a date so convert any h.h to hh:mm
        if(length(colons)<nrow(dataDF)) {dataDF[-colons,TIMEcol] <- sapply(dataDF[-colons,TIMEcol],decimal2Clock)}
        clockTime <- T
        timeFormat="h:m"
      }
    } else { #there were no colons, so all times are decimal
      if(is.null(dateFormat)){ #no date format so indicated that times are already decimal
        clockTime <- F
      } else { #there was a date format, so convert all h.h to hh:mm
        #dataDF[,TIMEcol] <- sapply(dataDF[,TIMEcol],decimal2Clock)  
        dataDF[,TIMEcol] <- as.numeric(dataDF[,TIMEcol])/24
        dt <- chron(dataDF[,DATEcol],dataDF[,TIMEcol])
        dataDF[,TIMEcol] <- paste(sprintf("%02i",hours(dt)),":",sprintf("%02i",minutes(dt)),sep="")
        dataDF[,DATEcol] <- dates(dt)
        clockTime <- T
      }
    }
  } #end time block
  
  #make relative times if not already there
  if(clockTime){
    relTime <- PMmatrixRelTime(dataDF,idCol=colNums$ID,dateCol=DATEcol,timeCol=TIMEcol,evidCol=colNums$EVID,
                               format=c(dateFormat,"h:m"))
    dataDF[,TIMEcol] <- relTime$relTime
    msg <- c(msg,"Times and dates converted to relative decimal hours.\n")
  }
  
  #remove DROP columns from data and inputs
  drops <- grep("DROP",inputs)
  if(length(drops)>0){
    dataDF <- dataDF[,-drops]
    inputs <- inputs[-drops]
    numcol <- ncol(dataDF)
    msg <- c(msg,"DROP columns removed.\n")
    #recount columns
    colNums <- lapply(1:nkeys,function(x) findCol(keys[x]))
    names(colNums) <- keys
  }
  
  
  #find the covariates
  covCol <- which(!(1:numcol) %in% colNums)
  ncov <- length(covCol)
  if(ncov>0) {
    covNames <- inputs[covCol]
    if(ncov==1){
      dataDF[,covCol] <- as.numeric(dataDF[,covCol])
    } else {
      dataDF[,covCol] <- apply(dataDF[,covCol],2,as.numeric)
    }
  }
  
  
  #make covariate for evid=2 or 3
  evid2 <- which(dataDF[,colNums$EVID]==2)
  evid3 <- which(dataDF[,colNums$EVID]==3)
  if(length(evid2)>0){
    dataDF$evid2 <- 0
    dataDF$evid2[evid2] <- dataDF[evid2,colNums$CMT]
    msg <- c(msg,"EVID = 2 is not implemented in Pmetrics directly.\nAn EVID2 covariate column has been added which contains the CMT values.\nUse it to conditionally control compartments in your model file.\n")
  }
  if(length(evid3)>0){
    dataDF$evid3 <- 0
    dataDF$evid3[evid3] <- 1
    msg <- c(msg,"EVID = 3 is not implemented in Pmetrics directly.\nAn EVID3 covariate column has been added with 0 for EVID not equal to 3, and 1 for EVID 3.\nUse it to conditionally reset compartments in your model file.\n")
  }
  
  
  #start to build Pmetrics file
  
  #create dur column
  if(!is.na(colNums$RATE)){
    dur <- round(dataDF[,colNums$AMT]/dataDF[,colNums$RATE],3)
  } else {
    dur <- 0
  }
  #create ADDL and II columns
  if(!is.na(colNums$ADDL)){
    addl <- dataDF[,colNums$ADDL]
  } else {
    addl <- NA
  }
  if(!is.na(colNums$II)){
    ii <- dataDF[,colNums$II]
  } else {
    ii <- NA
  }
  
  PMdata <- data.frame(id=dataDF[,colNums$ID],
                       evid=dataDF[,colNums$EVID],
                       time=dataDF[,colNums$TIME],
                       dur=dur,
                       dose=dataDF[,colNums$AMT],
                       addl=addl,
                       ii=ii,
                       input=ifelse(dataDF[,colNums$EVID]==0,NA,dataDF[,colNums$CMT]),
                       out=ifelse(dataDF[,colNums$EVID]==0,
                                  dataDF[,colNums$DV],NA),
                       outeq=ifelse(dataDF[,colNums$EVID]==0,
                                    dataDF[,colNums$CMT],NA),
                       c0=NA,c1=NA,c2=NA,c3=NA
                       
  )
  #fix EVID=2 or EVID=3
  evid23 <- c(evid2,evid3)
  if(length(evid23)>0){
    PMdata$evid[evid23] <- 1
    PMdata$dur[evid23] <- 0
    PMdata$dose[evid23] <- 0
    PMdata$addl[evid23] <- NA
    PMdata$ii[evid23] <- NA
    PMdata$input[evid23] <- 1
    PMdata$out[evid23] <- NA
    PMdata$outeq[evid23] <- NA
  }
  #fix SS/ADDL
  if(!is.na(colNums$SS)){
    ss <- which(dataDF[,colNums$SS]>=1)
    if(length(ss)>0){
      PMdata$addl[ss] <- -1
      msg <- c(msg,"SS=1 doses set to ADDL=-1.\n")
    }
  }
  
  #fix DUR if missing on doses (presumed to be 0)
  missingDur <- which(is.na(PMdata$dur[PMdata$evid==1]))
  if(length(missingDur)>0){
    PMdata$dur[PMdata$evid==1][missingDur] <- 0
    msg <- c(msg,"Missing DUR for EVID=1 dose events was set to 0.\n")
  }
  #fix DUR on non-doses
  PMdata$dur[PMdata$evid!=1] <- NA
  #fix DUR that are INF due to 0 rate
  PMdata$dur[PMdata$dur==Inf] <- 0
  
  #fix CMT to be outeq for observations and input for doses
  cmtObs <- unique(PMdata$outeq[PMdata$evid==0])
  cmtDoses <- unique(PMdata$input[PMdata$evid!=0])
  
  outeqFac <- factor(PMdata$outeq[PMdata$evid==0],labels=1:length(cmtObs))
  PMdata$outeq[PMdata$evid==0] <- as.numeric(as.character(outeqFac))
  
  inputFac <- factor(PMdata$input[PMdata$evid!=0],labels=1:length(cmtDoses))
  PMdata$input[PMdata$evid!=0] <- as.numeric(as.character(inputFac))
  
  msg <- c(msg,"CMT changed to OUTEQ for observations and INPUT for doses; ensure that your model file is correct.\n")
  
  #fix MDV
  if(!is.na(colNums$MDV)){
    mdv <- which(dataDF[dataDF[,colNums$EVID]==0,colNums$MDV]==1)
    if(length(mdv)>0){
      PMdata$out[PMdata$evid==0][mdv] <- -99
      msg <- c(msg,"MDV=1 observations set to OUT=-99.\n")
    }
  }
  
  #add covariates
  if(ncov>0){
    PMdata[,15:(14+ncov)] <- dataDF[,covCol]
    names(PMdata)[15:(14+ncov)] <- covNames
    msg <- c(msg,paste("The following covariates were found: ",paste(covNames,collapse=", "),".\n",sep=""))
  }
  if(length(evid2)>0) PMdata$evid2 <- dataDF$evid2
  if(length(evid3)>0) PMdata$evid3 <- dataDF$evid3
  
  #print the report to screen
  cat(msg)
  
  class(PMdata) <- c("PMmatrix","data.frame")
  return(PMdata)  
  
} #end function
