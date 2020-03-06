#' Generates an observed vs. predicted data.frame from an \emph{NPAG} or \emph{IT2B} object
#'
#' \code{makeOP} will parse the output of \code{\link{NPparse}} or \code{\link{ITparse}} to generate a
#' data.frame suitable for analysis and plotting of observed vs. population or
#' or individual predicted outputs.
#'
#' @title Generated observed vs. predicted data
#' @param data A suitable data object of the \emph{NPAG} or \emph{IT2B} class (see \code{\link{NPparse}} or \code{\link{ITparse}}).
#' @return The output of \code{makeOP} is a data frame of class \emph{PMop}, which has a population and posterior
#' prediction object (also class \emph{PMop}) for each output equation.  Each of these has 13 columns:
#' \item{id }{subject identification}
#' \item{time }{observation time in relative hours}
#' \item{obs }{observation}
#' \item{pred }{prediction}
#' \item{pred.type }{Population predictions based on Bayesian prior parameter value distribution,
#' or individual predictions based on Bayesian posterior parameter value distributions}
#' \item{icen }{Predictions based on mean or median of Bayesian \code{pred.type} parameter values}
#' \item{outeq }{output equation number}
#' \item{block }{dosing block number for each subject, as defined by dose resets (evid=4).}
#' \item{obsSD }{standard deviation of the observation based on the assay error polynomial}
#' \item{d }{prediction error, \code{pred}-\code{obs}}
#' \item{ds }{squared prediction error}
#' \item{wd }{weighted prediction error, which is the prediction error divided by the \code{obsSD}}
#' \item{wds }{weighted squared prediction error}
#' A plot method exists in \code{\link{plot}} for \emph{PMop} objects.
#' @author Michael Neely
#' @seealso \code{\link{NPparse}}, \code{\link{ITparse}}, \code{\link{plot.PMop}}, \code{\link{summary.PMop}}
#' @examples
#' data(PMex1)
#' op <- makeOP(NPdata.1)
#' op
#' names(op)
#' summary(op)
#' @export




makeOP <- function(data){
  
  #checkRequiredPackages("reshape2")
  if(!inherits(data,"NPAG") & !inherits(data,"IT2B")) stop(paste("Use NPparse() or ITparse() to generate a Pmetrics NPAG or IT2B object.\n"))
  
  #subsidiary function
  makeOPwrk <- function(data,icen,pred.type,outeq){
    #create data frame of observations by subject and output, including times
    vernum <- 1+as.numeric(ncol(data$outputs)==8)
    icen.index <- switch(icen,mean=1,median=2,mode=3,2) #default is median
    if(vernum>1){data$outputs <- data$outputs[data$outputs[,3]==outeq,]}
    obspred <- data.frame(id=rep(data$sdata$id,times=data$nobs),time=data$outputs[,2])
    obspred$obs <- switch(1+as.numeric(vernum>1),data$outputs[,outeq+2],data$outputs[,4])
    if(pred.type=="post"){pred <- c(t(data$ypredbay[,outeq,,icen.index]))} else {pred <- c(t(data$ypredpop[,outeq,,icen.index]))}
    na.only <- which(is.na(pred) & !is.nan(pred))
    if(length(na.only)>0) {obspred$pred <- pred[-na.only]} else {obspred$pred <- pred}
    obspred$pred.type <- pred.type
    obspred$icen <- icen
    obspred$outeq <- outeq
    obspred$block <- 1
    
    
    #add block numbers for dose resets
    
    for (i in unique(obspred$id)){
      reset.row <- which(obspred$time[obspred$id==i]==0 & obspred$obs[obspred$id==i]==-99)
      reset.blocks <- c(1,reset.row,length(obspred$time[obspred$id==i]))
      for (k in 1:(length(reset.blocks)-1)){
        obspred$block[obspred$id==i][reset.blocks[k]:reset.blocks[k+1]] <- rep(k,reset.blocks[k+1]-reset.blocks[k]+1)
      }
      obspred$obs[obspred$id==i][reset.row] <- obspred$pred[obspred$id==i][reset.row] <- 0
    }
    obspred$obs[obspred$obs == -99] <- NA
    obspred$pred[obspred$pred == -99] <- NA
    
    if(vernum>1){
      c0 <- c(data$outputs[,5])
      c1 <- c(data$outputs[,6])
      c2 <- c(data$outputs[,7])
      c3 <- c(data$outputs[,8])
      
      obspred$obsSD <- c0+c1*obspred$obs+c2*obspred$obs**2+c3*obspred$obs**3
      
      if(data$ERRmod==2){  #SD*gamma
        obspred$obsSD <- obspred$obsSD* rep(tail(data$igamlam,1),length(obspred$obsSD))
      }
      if(data$ERRmod==3){ #SD+lambda
        obspred$obsSD <- obspred$obsSD+ rep(tail(data$igamlam,1),length(obspred$obsSD))
      }
      if(data$ERRmod==4){ #gamma
        obspred$obsSD <- rep(tail(data$igamlam,1),length(obspred$obsSD))
      }
      obspred$d <- obspred$pred-obspred$obs
      obspred$ds <- (obspred$pred-obspred$obs)**2
      obspred$wd <- (obspred$pred-obspred$obs)/obspred$obsSD
      obspred$wds <- ((obspred$pred-obspred$obs)/obspred$obsSD)**2
      
    } 
    class(obspred)=c("PMop","data.frame")
    return(obspred)  
  } #end makeOPwrk()
  
  #here's the code to make the OP object
  op <- list()
  j <- 1
  for (i in 1:data$numeqt){
    op[[j]] <- makeOPwrk(data,icen="mean",pred.type="pop",outeq=i)
    op[[j+1]] <- makeOPwrk(data,icen="mean",pred.type="post",outeq=i)
    op[[j+2]] <- makeOPwrk(data,icen="median",pred.type="pop",outeq=i)
    op[[j+3]] <- makeOPwrk(data,icen="median",pred.type="post",outeq=i)
    j <- j+4

  }
  allOp <- do.call(rbind,op)
  return(allOp)
}

