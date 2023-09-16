#' @title Individual Bayesian posterior predictions at short intervals
#' @description
#' `r lifecycle::badge("stable")`
#' 
#' Returns the Bayesian posterior predictions at short intervals specified during the NPAG run,
#' up to 12 minutes.  These results are contained separately from the main output of NPAG, in the
#' PRTBxxxx file.
#' @details Predictions are calculated using the median (default) or mean of the 
#' Bayesian posterior parameter values and each subject's dosing, sampling, and covariate
#' (if relevant) information.
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
#' @examples
#' \dontrun{
#' post <- makePost(run = 1)
#' #would work if there is a complete run in folder 1
#' }
#' 
#' @export

makePost <- function(run,NPdata) {
  #get data
  if (missing(run)){ #look in current wd
    run <- "."
    predfile <- paste(run,"PRTB0001",sep="/")
    
  } else { #look in run folder/outputs
    if (!file.exists(as.character(run))) stop(paste(run," not found in the current working directory.\n",sep=""))
    predfile <- paste(run,"outputs/PRTB0001",sep="/")
  } 
  
  if(run !=".") { #run specified, so load corresponding objects
    res <- PM_load(run)
    NPdata <- res$NPdata
  }
  
  #read PRTB file  
  preddata <- readLines(predfile)
  
  
  predLines <- grep("[[:space:]]*TIMES[[:space:]]+PREDICTED VALUES",preddata)
  
  if(length(predLines)>0){ #we are dealing with the old format
    stop("Rerun NPAG to generate current format.\n")
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
    
    #add blocks
    blocks <- totalRaw %>% 
      group_by(.data$id) %>%
      filter(.data$time == 0) %>%
      transmute(blocknum = row_number())

    
    totalRaw$block <- NA
    totalRaw$block[totalRaw$time==0] <- blocks$blocknum
    totalRaw <- fill(totalRaw,block)
    
    
    post <- totalRaw %>% 
      pivot_longer(cols = c(-time, -id, -block),names_to="icen",values_to="pred") %>%
      arrange(.data$id,.data$icen,.data$block,.data$time) %>%
      extract(icen,into="outeq",regex="([[:digit:]])+$", remove = F, convert = T) %>%
      separate(icen,into = c("icen",NA), sep = "[[:digit:]]+") %>%
      select(.data$id,.data$time,.data$icen,.data$outeq,.data$pred,.data$block) %>% 
      filter(.data$icen!="mode") #suppress mode
    

  }
  
  #add predictions at observed times
  op <- makeOP(NPdata)
  opPost <- op[op$pred.type=="post",]
  post <- rbind(post,opPost[,c("id","time","icen","outeq","pred","block")])
  
  #remove duplicates
  dupTime <- which(duplicated(post[,c("id","time","icen","outeq","block")]))
  if(length(dupTime)>0) post <- post[-dupTime,]
  
  #sort by icen, id, block, time, outeq
  post <- post[order(post$icen,post$id,post$block,post$time,post$outeq),]
  
  #assign class
  class(post) <- c("PMpost","data.frame")
  
  return(post)
  
}
