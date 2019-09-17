#' Non-compartmental analysis
#'
#' Performs a non-compartmental analysis from observed concentrations in the raw data
#' file or from an individual Bayesian posterior predicted 
#' time-observation profiles (PMpost object) generated automatically after an NPAG run 
#' by the \code{\link{makePost}} command and loaded with \code{\link{PMload}}.  
#' 
#' If concentrations from multiple dose intervals are included in the \code{start}-\code{end} time interval,
#' \code{makeNCA} will superpose the concentrations using the time after dose.  An error will be generated if
#' different doses are within this interval as superposition would no longer be valid.
#' 
#' A minimum of 5 concentrations must be available to perform NCA for any given subject.  Fewer than this will
#' suppress all results for that subject.  
#'
#' @param x Data to analyze.  This can be specified in a number of ways.  
#' \itemize{
#' \item It can be the run number, e.g. 3, that has been previously loaded with \code{\link{PMload}}.
#' Either the mdata file from the run (NPAG or IT2B) can be used (default) or
#' the post object can be used (NPAG only) by specifying \code{postPred = T} below.  If \code{x} is a run number that corresponds 
#' to both an NPAG and IT2B run which have been previously loaded into memory with \code{PMload}, the NPAG run will be used. 
#' \item It can be the run number of a run that has \emph{not} been previously loaded with \code{\link{PMload}}.
#' In this case, the current working directory should be the Runs folder as \code{makeNCA} will call \code{PMload}. 
#' \item It can be the specific name of an mdata.x file already loaded into memory with \code{\link{PMload}}, e.g. mdata.3.  Note
#' that quotation marks are not necessary since mdata.3 is an object, not a label/character vector.
#' \item Finally, it can be the name of a Pmetrics data file in the current working directory, which will be loaded with
#' \code{\link{PMreadMatrix}} and analyzed, e.g. \dQuote{data.csv}.  In this case, quotation marks are reqired, because \code{x}
#' is now a character vector specifying the filename of the file to load.
#' }
#' @param postPred Boolean switch to use the posterior predictions rather than the observed. 
#' concentrations.  Default is \code{FALSE}. Ignored if an IT2B run is
#' used to supply the raw data file.
#' @param include A vector of subject IDs to include in the NCA, e.g. c(1:3,5,15)
#' @param exclude A vector of subject IDs to exclude in the NCA, e.g. c(4,6:14,16:20). When \code{postPred} is \code{TRUE}, any subject(s) excluded from the IT2B/NPAG run will be excluded as well.
#' @param input The number of the input (e.g. drug) to analyze; default 1.
#' @param icen If \code{postPred} is \code{TRUE}, use predictions based on median or mean of each
#' subject's Bayesian posterior parameter distribution.  Default is "median", but could be "mean".
#' @param outeq The number of the output equation to analyze; default 1
#' @param block The number of the observation block within subjects, with each block delimited by EVID=4 in the data file; default 1
#' @param start The beginning of the time interval to look for doses and observations, e.g. 120.  It can be
#' a vector to allow for individual start times per subject, e.g. c(120,120,144,168).  If the length of \code{start}
#' is less than the number of subjects, the last value will be recycled as needed.  If the \code{start} time is not 0 (default), 
#' then it is assumed that steady state (multiple dose) conditions apply. 
#' @param end Analogous to \code{start}, set this equal to the end of the dosing interval. It too can be a vector, with the last value
#' recycled as necessary.  Default is \code{Inf}, i.e. all data used.
#' @param first Alternative way to specify time interval for NCA by choosing dose number, e.g. 1 or 3.  May be a numeric vector, like \code{start} and \code{end},
#' e.g. c(1,1,1,3,1,...) to allow for individualization by subject.  The last value will be recycled to ensure length equal to the
#' number of subjects.  Default is \code{NA}, which means \code{start} will be used.
#' @param last The complement to \code{first}, specifying the last dose to end the time interval.  If \code{NA},
#' which is the default, then the maximum time per subject will be the upper bound of the time interval.
#' Like \code{first}, \code{last} can be a vector, with the last value recycled as necessary.  Use \code{NA} in the vector
#' to signify maximum time for that subject.
#' @param terminal Number of observations to use for terminal curve fitting (i.e. to estimate \emph{k}).  Default is 3.
#' @return A dataframe of class \emph{PMnca} with columns
#'  \item{id }{Subject identification}
#'  \item{auc }{Area under the time-observation curve, using the trapezoidal approximation, from time 0 until the second dose, 
#' or if only one dose, until the last observation}
#'  \item{aumc }{Area under the first moment curve}
#'  \item{k }{Slope by least-squares linear regression of the final 3 log-transformed observations vs. time.  
#'  If the final 3 concentrations are not decreasing such that linear regression results in a positive slope,
#'  this value and all others that depend on \code{k} will be suppressed.}
#'  \item{auclast }{Area under the curve from the time of the last observation to infinity, calculated as [Final obs]/k.
#'  This value will be suppressed if start != 0.}
#'  \item{aumclast }{Area under the first moment curve from the time of the last observation to infinity.
#'  This value will be suppressed if start!=0.}
#'  \item{aucinf }{Area under the curve from time 0 to infinity, caluculated as auc + auclast}
#'  \item{aumcinf }{Area under the first moment curve from time 0 to infinity}
#'  \item{mrt }{Mean residence time, calculated as 1/k}
#'  \item{cmax }{Maximum predicted concentration after the first dose}
#'  \item{tmax }{Time to cmax}
#'  \item{cl }{Clearance, calculated as dose/aucinf}
#'  \item{vdss }{Volume of distribution at steady state, calculated as cl*mrt}
#'  \item{thalf }{Half life of elimination, calculated as ln(2)/k}
#'  \item{dose }{Dose for each subject}
#' @author Michael Neely

makeNCA <- function(x,postPred=F,include,exclude,input=1,icen="median",outeq=1,block=1,
                    start=0,end=Inf,first=NA,last=NA,terminal=3){
  
  
  if(length(grep("plyr",installed.packages()[,1]))==0){
    install.packages("plyr",repos="http://cran.cnr.Berkeley.edu",dependencies=T)
  }
  plyr.installed <- require(plyr)
  if(!plyr.installed) stop("Error: connect to internet and re-run makeOP to download and install plyr package.\n")
  
  
  if(!missing(x)) { #x specified, so load corresponding object
    if(is.numeric(x)){ #run number
      ITdataName <- paste("ITdata.",x,sep="")
      NPdataName <- paste("NPdata.",x,sep="")
      mdataName <- paste("mdata.",x,sep="")
      postName <- paste("post.",x,sep="")
      
      
      if(length(ls(pattern=NPdataName))>0 | length(ls(pattern=ITdataName))>0){ #objects are already loaded
        data <- tryCatch(get(NPdataName),error=function(e) -1)
        if(data[1]==-1){data <- get(ITdataName)}
        mdata <- get(mdataName)
        post <- tryCatch(get(postName),error=function(e) -1)
        if(length(post)==1){post <- NA}  
      } else { #objects not loaded, try to load
        loadOK=PMload(x)
        if(loadOK){ #load was ok
          data <- tryCatch(get(NPdataName),error=function(e) -1)
          if(data[1]==-1){data <- get(ITdataName)}
          mdata <- get(mdataName)
          post <- tryCatch(get(postName),error=function(e) -1)
          if(length(post)==1){post <- NA}
        } else {stop("Please ensure that current working directory is your Runs folder and run exists.\n")}
      }
    } else {# end is numeric x
      if(inherits(x,"PMmatrix")){
        mdata <- x #mdata file specified directly
      } else {mdata <- PMreadMatrix(x)} #x must be a character vector of file to load
      #make dummy data object to correctly handle below
      data <- 1
      class(data) <- "IT2B"
    } #end is not numeric x
    
  } else { #end if !missing x
    stop("Please specify an object for NCA.\n")
  } #end x is missing
  
  #filter by time for each subject
  timeFilter <- function(x,startTimes,endTimes){
    whichtime <<- whichtime + 1
    x <- x[x$time>=startTimes[whichtime] & x$time <=endTimes[whichtime],]
    return(x)
  }
  
  #function to filter mdata by time interval
  mdataFilter <- function(mdata,start,end,first,last){
    
    nsub <- length(unique(mdata$id))
    doseTimes <- by(mdata$time[mdata$evid!=0],mdata$id[mdata$evid!=0],function(x) x)
    
    
    if(!is.na(first)){
      if(length(first) < nsub) first <- c(first,rep(tail(first,1),nsub))
      startTimes <- sapply(1:length(doseTimes),function(x) doseTimes[[x]][first[x]])
      
      if(all(is.na(last))){ #missing last
        endTimes <- tapply(mdata$time[mdata$evid==0],mdata$id[mdata$evid==0],max)
      } else { #last supplied
        if(length(last) < nsub) last <- c(last,rep(tail(last,1),nsub)) #ensure last is long enough
        maxTimes <- tapply(mdata$time[mdata$evid==0],mdata$id[mdata$evid==0],max)
        endTimes <- sapply(1:nsub,function(x) ifelse(is.na(last[x]),maxTimes[x],doseTimes[[x]][last[x]]))
      }
      
    } else {#end !is.na first
      
      #ok first was NA so use start and end, making sure long enough
      if(length(start) < nsub) {start <- c(start,rep(tail(start,1),nsub))[1:nsub]}
      if(length(end) < nsub) {end <- c(end,rep(tail(end,1),nsub))[1:nsub]}
      startTimes <- start
      endTimes <- end
    }
    
    #set missing endTimes to Inf, which means go to the end of the 
    #record for that subject
    endTimes[is.na(endTimes)] <- Inf
    
    #however missing startTimes mean that the subject has no records
    #which meet the criteria, so we have to remove them
    if(any(is.na(startTimes))){
      missingStart <- which(is.na(startTimes))
      mdata <- mdata[!mdata$id %in% unique(mdata$id)[missingStart],]
      startTimes <- startTimes[-missingStart]
      endTimes <- endTimes[-missingStart]
      cat(paste("The following subjects had no concentrations within the time interval: ",paste(missingStart,collapse=", "),"\n",sep=""))
    }
    
    whichtime <<- 0
    mdata2 <- ddply(mdata,.(id),timeFilter,startTimes,endTimes)
    return(list(mdata2,startTimes,endTimes))
  }
  
  #function to convert mdata into data frame of of ID, time, conc, prev dose
  conv_mdata <- function(mdata,input,outeq,block,start,end,first,last){
    #first, figure out time interval
    filterResults <- mdataFilter(mdata,start,end,first,last)
    mdata2 <- filterResults[[1]]
    startTimes <- filterResults[[2]]
    endTimes <- filterResults[[3]]
    #now filter to input, outeq, and block
    otherInputs <- which(mdata2$evid!=0 & mdata2$input!=input)
    if(length(otherInputs)>0) {mdata2 <- mdata2[-otherInputs,]}
    otherOuteq <- which(mdata2$evid==0 & mdata2$outeq!=outeq)
    if(length(otherOuteq)>0) {mdata2 <- mdata2[-otherOuteq,]}
    mdata2 <- mdata2[!is.na(mdata2$id),]
    mdata2 <- makePMmatrixBlock(mdata2)
    mdata2 <- mdata2[mdata2$block==block,]
    mdata2 <- mdata2[order(mdata2$id,mdata2$time,-mdata2$evid),]
    
    #then calculate time after dose and previous dose
    for(i in 1:nrow(mdata2)){
      if(mdata2$evid[i]!=0){
        doseTime <- mdata2$time[i]
        prevDose <- mdata2$dose[i]
      }
      mdata2$tad[i] <- mdata2$time[i] - doseTime
      mdata2$prevDose[i] <- prevDose
    }
    #now make the final form
    mdata2 <- mdata2[mdata2$evid==0,]
    if(nrow(mdata2)==0) stop("Your PMmatrix object has no observations in the interval.\n")
    mdata3 <- data.frame(id=mdata2$id,tad=mdata2$tad,out=mdata2$out,dose=mdata2$prevDose)
    mdata3 <- mdata3[order(mdata3$id,mdata3$tad),]
    dupTimes <- which(duplicated(mdata3[,1:2])) #remove duplicated times
    if(length(dupTimes)>0){
      cat("WARNING: Duplicate times after dose within subjects removed. Consider smaller time interval.\n")
      mdata3 <- mdata3[-dupTimes,]
    }
    return(list(mdata3,startTimes,endTimes))
    
  }
  
  #function to convert post into data frame of of ID, time, conc, prev dose
  conv_post <- function(post,mdata,include,exclude,input,outeq,block,icen,start,end,first,last){
    #first, figure out time interval
    
    filterResults <- mdataFilter(mdata,start,end,first,last)
    mdata2 <- filterResults[[1]]
    startTimes <- filterResults[[2]]
    endTimes <- filterResults[[3]]
    
    #now filter post by time
    whichtime <<- 0
    post2 <- ddply(post,.(id),timeFilter,startTimes,endTimes)
    #then, filter by outeq, block, and icen
    otherOuteq <- which(post2$outeq!=outeq)
    if(length(otherOuteq)>0) {post2 <- post2[-otherOuteq,]}
    post2 <- post2[post2$block==block,]
    post2 <- post2[post2$icen==icen,]
    #rename pred column to out
    names(post2)[4] <- "out"
    #add evid and dummy dose
    post2$evid <- 0
    post2$dose <- NA
    post3 <- post2[,c("id","evid","time","out","dose")]
    #now get dose times from mdata
    otherInputs <- which(mdata2$evid!=0 & mdata2$input!=input)
    if(length(otherInputs)>0) {mdata2 <- mdata2[-otherInputs,]}
    otherOuteq <- which(mdata2$evid==0 & mdata2$outeq!=outeq)
    if(length(otherOuteq)>0) {mdata2 <- mdata2[-otherOuteq,]}
    mdata2 <- mdata2[!is.na(mdata2$id),]
    mdata2 <- makePMmatrixBlock(mdata2)
    mdata2 <- mdata2[mdata2$block==block,]
    mdata3 <- mdata2[mdata2$evid==1,c("id","evid","time","out","dose")]
    #combine
    post4 <- rbind(post3,mdata3)
    post4 <- post4[order(post4$id,post4$time,-post4$evid),]
    
    #then calculate time after dose and previous dose
    for(i in 1:nrow(post4)){
      if(post4$evid[i]!=0){
        doseTime <- post4$time[i]
        prevDose <- post4$dose[i]
      }
      post4$tad[i] <- post4$time[i] - doseTime
      post4$prevDose[i] <- prevDose
    }
    #now make the final form
    post5 <- post4[post4$evid==0,]
    post6 <- data.frame(id=post5$id,tad=post5$tad,out=post5$out,dose=post5$prevDose)
    post6 <- post6[order(post6$id,post6$tad),]
    dupTimes <- which(duplicated(post6[,1:2])) #remove duplicated times
    if(length(dupTimes)>0){
      cat("WARNING: Duplicate times after dose within subjects removed. Consider smaller time interval.\n")
      post6 <- post6[-dupTimes,]
    }
    return(list(post6,startTimes,endTimes))
    
  }
  
  #choose ultimate data source and get into data frame 
  if(inherits(data,"IT2B")){ #using IT2B to get mdata
    if(!missing(include)){
      mdata <- subset(mdata,sub("[[:space:]]+","",as.character(mdata$id)) %in% as.character(include))
    } 
    if(!missing(exclude)){
      mdata <- subset(mdata,!sub("[[:space:]]+","",as.character(mdata$id)) %in% as.character(exclude))
    }       
    dataList <- conv_mdata(mdata,input=input,outeq=outeq,block=block,start=start,end=end,first=first,last=last)
  }
  if(inherits(data,"NPAG")){ #using NPAG
    
    if(!missing(include)){
      mdata <- subset(mdata,sub("[[:space:]]+","",as.character(mdata$id)) %in% as.character(include))
      post <- subset(post,sub("[[:space:]]+","",as.character(post$id)) %in% as.character(include))
    } 

    if(!missing(exclude)){
      mdata <- subset(mdata,!sub("[[:space:]]+","",as.character(mdata$id)) %in% as.character(exclude))
      post <- subset(post,!sub("[[:space:]]+","",as.character(post$id)) %in% as.character(exclude))
      if (length(unique(mdata$id))==0) stop("No subjects left to analyze.", call. = F)
    }

    if(postPred){
      # check if any subjects are still missing from the post object after include/exclude are done, exclude them from the analysis and warn user
      missing_from_post <- setdiff(unique(mdata$id), unique(post$id)) #there should never be more in post than in mdata
      if (length(missing_from_post)!=0){
        warning("Subjects No. ", paste(missing_from_post, collapse = ", "),  " are missing from the post object and were excluded.", call. = F)
        mdata <- subset(mdata,!sub("[[:space:]]+","",as.character(mdata$id)) %in% as.character(missing_from_post))
        post <- subset(post,!sub("[[:space:]]+","",as.character(post$id)) %in% as.character(missing_from_post))
        if (length(unique(mdata$id))==0) stop("No subjects left to analyze.", call. = F)
      }
      dataList <- conv_post(post=post,mdata=mdata,input=input,outeq=outeq,block=block,icen=icen,start=start,end=end,first=first,last=last)
    } else {      
      dataList <- conv_mdata(mdata,input=input,outeq=outeq,block=block,start=start,end=end,first=first,last=last)
    }
  }
  
  
  
  
  ### DO THE NCA
  
  #make dataframe
  wrkData <- dataList[[1]]
  startTimes <- dataList[[2]]
  endTimes <- dataList[[3]]
  nsub <- length(unique(wrkData$id))
  NCA <- matrix(NA,nrow=nsub,ncol=14)
  
  #cycle through each subject
  
  for(i in 1:nsub){
    temp <- wrkData[wrkData$id==unique(wrkData$id)[i],]
    temp <- temp[temp$out!=-99,]
    #error checking
    if(!all(temp$dose==temp$dose[1])){
      cat(paste("Subject",temp$id[1],"does not have the same doses before all concentrations that were superimposed; shorten the time interval.\n"))
      next
    }
    if(nrow(temp)==0){
      cat(paste("Subject",temp$id[1],"has no events in the time interval.\n"))
      next
    }
    if(all(temp$tad==0)){
      cat(paste("Subject",temp$id[1],"does not have any observations within the time interval.\n"))
      next
    }
    #end error checking
    
    NCA[i,10] <- max(temp$out) #cmax
    NCA[i,11] <- temp$tad[which(temp$out==NCA[i,10])][1]  #tmax
    NCA[i,2] <- makeAUC(temp,out~tad,icen=icen,outeq=outeq,block=block)[,2] #auc
    temp2 <- data.frame(id=temp$id,tad=temp$tad,out=temp$tad*temp$out)
    NCA[i,3] <- makeAUC(temp2,out~tad,icen=icen,outeq=outeq,block=block)[,2] #aumc
    
    if(nrow(temp)>=5){
      temp <- tail(temp,terminal)
      temp <- temp[temp$out>0,]
      k <- tryCatch(-coef(lm(log(temp$out)~temp$tad))[2],error=function(e) -1)
      if(k<=0){
        cat(paste("Subject",temp$id[1],"does not have decreasing terminal",terminal,"concentrations; some NCA values cannot be computed.\n"))
        NCA[i,4:6] <- NA
      } else {
        NCA[i,4] <-  k
        NCA[i,5] <- ifelse(start==0,temp$out[3]/k,NA) #auclast
        NCA[i,6] <- ifelse(start==0,temp$out[3]*temp$tad[3]/k + temp$out[3]/k^2,NA) #aumclast
      }
    } else {
      cat(paste("Subject",temp$id[1],"does not have 5 observations; some NCA values cannot be computed.\n"))
      NCA[i,4:6] <- NA
    }
    
  }
  
  NCA <- data.frame(NCA)
  names(NCA) <- c("id","auc","aumc","k","auclast","aumclast","aucinf","aumcinf","mrt","cmax","tmax","cl","vdss","thalf")
  

  NCA$id <- unique(wrkData$id)
  NCA$aucinf <- ifelse(startTimes==0,NCA$auc + NCA$auclast,NA)
  NCA$aumcinf <- ifelse(startTimes==0,NCA$aumc + NCA$aumclast,NA)
  NCA$mrt <- ifelse(startTimes==0,NCA$aumcinf/NCA$aucinf,NCA$aumc/NCA$auc)
  
  NCA$dose <- wrkData$dose[!duplicated(wrkData$id)]
  
  NCA$cl <- ifelse(startTimes==0,NCA$dose/NCA$aucinf,NCA$dose/NCA$auc)
  NCA$vdss <- NCA$cl*NCA$mrt
  NCA$thalf <- ifelse(startTimes==0,log(2)/NCA$k,NCA$mrt/1.44)
  class(NCA) <- c("PMnca","data.frame")
  return(NCA)
}


