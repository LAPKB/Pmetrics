#' @title Individual Bayesian population prior predictions at short intervals
#' @description
#' `r lifecycle::badge("stable")`
#' 
#' Returns the Bayesian population prior predictions at short intervals specified during the NPAG run,
#' up to 12 minutes.
#' @details Predictions are calculated using the median (default) or mean of the 
#' Bayesian prior parameter values and each subject's dosing, sampling, and covariate
#' (if relevant) information.
#'
#' @param run The number of the folder that contains the relevant run.  If missing, \code{NPdata} will be used.
#' @param NPdata Optional name of NPdata object if run is missing.
#' @return A dataframe of class \emph{PMpop} with columns:
#' \item{id}{ Subject id}
#' \item{time}{ Time of predictions in decimal hours}
#' \item{icen}{ Prediction based on mean or median of Bayesian posterior parameter distribution}
#' \item{pred}{ Predicted output for each outeq}
#' \item{outeq}{ Output equation number}
#' \item{block}{ Observation blocks within subjects as defined by EVID=4 dosing events}
#' @author Michael Neely
#' @examples
#' pop <- makePop(NPdata = NPex$NPdata)
#' @export

makePop <- function(run, NPdata) {
  #require(utils)
  #checkRequiredPackages("reshape2") 
  #get data
  if (!missing(run)){ #run specified, so load corresponding objects
    res <- PM_load(run)
    NPdata <- res$NPdata
  }
  
  #check for old ypredpopt object and update dimnames
  if(is.null(dimnames(NPdata$ypredpopt))){
    dimnames(NPdata$ypredpopt) <- list(id=1:NPdata$nsub,outeq=1:NPdata$numeqt,time=NPdata$ttpred[which(NPdata$numt==max(NPdata$numt))[1],],icen=c("mean","median","mode")) 
  }
  #pop <- melt(NPdata$ypredpopt,value.name="pred")
  
  pop <- NPdata$ypredpopt %>% 
    as.tbl_cube(met_name="pred") %>%
    dplyr::as_tibble() %>%
    filter(!is.na(.data$pred)) %>%
    select(.data$id,.data$time,.data$icen,.data$pred,.data$outeq) %>%
    arrange(.data$icen,.data$id,.data$outeq)
  

  pop$id <- rep(unlist(lapply(1:NPdata$nsub,function(x) rep(NPdata$sdata$id[x],times=NPdata$numeqt*NPdata$numt[x]))),3)
  
  #count 0 times per subject, icen, and outeq - should be at least 1 for each
  blocks <- tapply(pop$time,list(pop$id,pop$icen,pop$outeq),function(x) sum(x==0))
  
  blocks2 <- unlist(mapply(function(x) 1:x,blocks))
  time0 <- c(which(pop$time==0),1+nrow(pop))
  blocks3 <- rep(blocks2,times=diff(time0))
  pop$block <- blocks3
  
  #suppress mode for NPAG
  pop <- pop[pop$icen!="mode",]
  
  #add predictions at observed times
  op <- makeOP(NPdata)
  opPop <- op[op$pred.type=="pop",]
  pop <- rbind(pop,opPop[,c("id","time","icen","pred","outeq","block")])
  
  #remove duplicates
  dupTime <- which(duplicated(pop[,c("id","time","icen","outeq","block")]))
  if(length(dupTime)>0) pop <- pop[-dupTime,]
  
  #sort by icen, id, time, outeq
  pop <- pop[order(pop$icen,pop$id,pop$time,pop$outeq),]
  
  class(pop) <- c("PMpop","data.frame")
 
  return(pop)

}

