#' Individual Bayesian posterior predictions at short intervals
#'
#' Returns the Bayesian posterior predictions at short intervals specified during the NPAG run,
#' up to 12 minutes.  These results are contained separately from the main output of NPAG, in the
#' PRTBxxxx file.
#'
#' @param run The number of the folder that contains the relevant run.  If missing will be
#' set to current working directory.
#' @param NPdata Optional name of NPdata object if run is missing.
#' @return A dataframe of class \emph{PMpost} with columns:
#' \item{id}{ Subject id}
#' \item{time}{ Time of predictions in decimal hours}
#' \item{icen}{ Prediction based on mean or median of Bayesian posterior parameter distribution}
#' \item{pred}{ Predicted output for each outeq}
#' \item{outeq}{ Output equation number}
#' \item{block}{ Observation blocks within subjects as defined by EVID=4 dosing events}
#' @author Michael Neely

makePost <- function(run,NPdata) {
  #require(utils)
  if(length(grep("reshape2",installed.packages()[,1]))==0){
    install.packages("reshape2",repos="http://cran.cnr.Berkeley.edu",dependencies=T)
  }
  reshape2.installed <- require(reshape2)
  if(!reshape2.installed) stop("Error: connect to internet and re-run makePost to download and install reshape2 package.\n")
  
  #get data
  if (missing(run)){ #look in current wd
    run <- "."
    predfile <- paste(run,"PRTB0001",sep="/")
    
  } else { #look in run folder/outputs
    if (!file.exists(as.character(run))) stop(paste(run," not found in the current working directory.\n",sep=""))
    predfile <- paste(run,"outputs/PRTB0001",sep="/")
  } 
  if(run !=".") { #run specified, so load corresponding objects
    PMload(run)
    NPdata <- get(paste("NPdata.",run,sep=""))
  }
  
  #read PRTB file  
  preddata <- readLines(predfile)
  
  
  predLines <- grep("[[:space:]]*TIMES[[:space:]]+PREDICTED VALUES",preddata)
  
  if(length(predLines)>0){ #we are dealing with the old format
    #get prediction type
    icen <- tolower(tail(strsplit(preddata[2]," ")[[1]],1))
    #remove final "s"
    icen <- substr(icen, 1,nchar(icen)-1) 
    outLines <- grep("[[:print:]]+OUTPUT EQUATION NO[[:print:]]+[[:digit:]]+",preddata)
    nsub <- length(predLines)
    nout <- length(outLines)/nsub
    
    #verify that predfile and NPdata have same number of subjects
    
    if (NPdata$nsub != nsub){
      cat("The number of subjects in the NPAG input matrix file\ndoes not equal the number in the NPAG output files.\n")
      return()
    }
    
    #get number of output equations
    nout <- NPdata$numeqt
    #get number of prediction points per subject
    npred <- NPdata$numt
    #get all post predicted values 
    predLines <- predLines + 1 #offset to NPdata
    
    post <- data.frame(id=rep(NPdata$sdata$id,npred))
    postPred <- matrix(scan(predfile,skip=predLines[1],nlines=npred[1],quiet=T),ncol=nout+1,byrow=T)
    if (nsub > 1){
      pb <- txtProgressBar(min = 1, max = nsub, style = 3)
      cat("\nObtaining posterior predicted time-observation profiles for each subject.\n")
      flush.console()
      for (i in 2:nsub){
        temp <- matrix(scan(predfile,skip=predLines[i],nlines=npred[i],quiet=T),ncol=nout+1,byrow=T)
        postPred <- rbind(postPred,temp)
        setTxtProgressBar(pb,i)
        
      }
    }
    post$time <- postPred[,1]
    post <- cbind(post,postPred[,-1])
    names(post)<-c("id","time",paste("pred",1:nout,sep=""))
    post$id <- factor(as.character(post$id),levels=unique(post$id))
    post$time <- unlist(tapply(post$time,post$id,function(x) x-x[1]))
    blocks <- tapply(post$time,post$id,function(x) sum(x==0))
    blocks2 <- unlist(mapply(function(x) 1:x,blocks))
    time0 <- c(which(post$time==0),nrow(post))
    blocks3 <- rep(blocks2,times=diff(time0))
    post$block <- c(blocks3,tail(blocks3,1))
    
    #transform into new format
    post <- melt(post,id.vars=c("id","time","block"),variable.name="outeq",value.name="pred")
    post$icen <- icen
    post$outeq <- as.numeric(sub("pred","",post$outeq))
    post <- post[,c("id","time","icen","pred","outeq","block")]
    #sort by icen, id, outeq
    post <- post[order(post$icen,post$id,post$outeq),]
    
  } else { #we are dealing with the new format
    predLines <- grep("BASED, IN ORDER, ON THE POSTERIOR MEANS, MEDIANS, AND MODES:",preddata)
    raw <- list()
    if(NPdata$nsub>1) pb <- txtProgressBar(min = 1, max = NPdata$nsub, style = 3)
    cat("\nObtaining posterior predicted time-observation profiles for each subject.\n")
    flush.console()
    for(i in 1:NPdata$nsub){
      if(NPdata$nsub>1) setTxtProgressBar(pb,i)
      raw[[i]] <- data.frame(matrix(scan(predfile,skip=predLines[i]+1,nlines=NPdata$numt[i],quiet=T),ncol=1+3*NPdata$numeqt,byrow=T))
      names(raw[[i]]) <- c("time",unlist(lapply(1:NPdata$numeqt,function(x) paste(c("mean","median","mode"),x,sep=""))))
      raw[[i]]$id <- NPdata$sdata$id[i]
    }
    totalRaw <- do.call(rbind,raw)
    
    post <- melt(totalRaw,id.vars=c("id","time"),variable.name="icen",value.name="pred")
    post$outeq <- rep(1:NPdata$numeq,each=3*sum(NPdata$numt))
    levels(post$icen) <- rep(c("mean","median","mode"),NPdata$numeqt)

    
    
    
    #count 0 times per subject, icen, and outeq - should be at least 1 for each
    blocks <- tapply(post$time,list(post$id,post$icen,post$outeq),function(x) sum(x==0))
    
    blocks2 <- unlist(mapply(function(x) 1:x,blocks))
    time0 <- c(which(post$time==0),1+nrow(post))
    blocks3 <- rep(blocks2,times=diff(time0))
    post$block <- blocks3
    
    #suppress mode for NPAG
    post <- post[post$icen!="mode",]
  }
  
  #add predictions at observed times
  op <- makeOP(NPdata)
  opPost <- op[op$pred.type=="post",]
  post <- rbind(post,opPost[,c("id","time","icen","pred","outeq","block")])
  
  #remove duplicates
  dupTime <- which(duplicated(post[,c("id","time","icen","outeq","block")]))
  if(length(dupTime)>0) post <- post[-dupTime,]
  
  #sort by icen, id, time, outeq
  post <- post[order(post$icen,post$id,post$time,post$outeq),]
  
 
  
  
  class(post) <- c("PMpost","data.frame")
  
  return(post)
  
}

