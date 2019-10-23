#' Calculates AUC from a variety of inputs
#'
#' \code{makeAUC} will calculate the area under the time concentration curve using the
#' trapezoidal approximation from a variety of inputs.  If a PMpost, PMop, or PMsim object is specified, 
#' \code{formula} is not required.  AUCs from PMop objects are based on observations.
#' For AUCs based on predictions, use a PMpost object.
#'
#' @title Calculation of AUCs
#' @param formula A formula of the form \code{obs ~ time}.  This is only required with data that is not of class PMpop, PMpost, PMop or PMsim.
#' @param data A suitable data object of the \emph{PMpop} class (see \code{\link{makePop}}),
#' \emph{PMpost} class (see \code{\link{makePost}}),
#' \emph{PMop} class (see \code{\link{makeOP}}),
#' the \emph{PMsim} class (see \code{\link{SIMparse}}), or some other suitable dataframe
#' with at least time/observation columns referred to by \code{formula}, with an \dQuote{id} column (so named) if necessary.
#' @param include A vector of subject IDs to include in the AUC calculations, e.g. c(1:3,5,15)
#' @param exclude A vector of subject IDs to exclude in the AUC calculations, e.g. c(4,6:14,16:20)
#' @param start Specify the time to begin AUC calculations. Default is 0.
#' @param end Specify the time to end AUC calculations so that AUC is calculated
#' from \code{start} to \code{end}.  Default for end is the maximum observation
#' time for each subject.  Subjects with insufficient data for a specified interval will have
#' AUC calculated for the available data, not to exceed the specified interval.
#' @param icen Only relevant for PMpost or PMpop objects which have predictions based on median or mean of each
#' subject's Bayesian posterior parameter distribution.  Default is "median", but could be "mean".
#' @param outeq Specify which output equation is to be used.  Default is 1.
#' @param block Specify which observation block (separated by EVID=4) is to be used for each subject.  Default is 1.
#' @param method Default is "linear" for AUC trapezoidal calculation.  Any other value will result in
#' linear up, log down.
#' @return The output of \code{makeAUC} is a dataframe of class \emph{PMauc},
#' which has 2 columns:
#' \item{id }{subject identification}
#' \item{tau }{AUC from \code{start} to \code{end}}
#' @author Michael Neely
#' @seealso \code{\link{makeOP}}, \code{\link{SIMparse}}
#' @examples
#' data(PMex1)
#' op <- makeOP(NPdata.1)
#' makeAUC(op)

makeAUC <- function(data,formula,include,exclude,start=0,end=Inf,icen="median",outeq=1,block=1,method="linear"){
  
  #auc function
  trapAUC <- function(time, conc, id = rep(1, length(time)), addZero, method) {
    notMiss <- !is.na(conc) & !is.na(time) & !is.na(id)
    if(!any(notMiss)) stop("Missing values not allowed")
    innerAuc <- function(df, obs, addZero, method) {
      if (addZero & !any(df$X == 0)) df <- rbind(data.frame(X=0, Y=0), df)
      N <- nrow(df)
      if(N <= 1) return(data.frame(NA, NA))
      #(t_i - t_i-1)
      diffTimes <- diff(df$X)
      #(C_i + C_i-1)
      sumConc <- df$Y[-1] + df$Y[-N]
      #(C_i - C_i-1)
      diffConc <- diff(df$Y)
      #log(C_i/C_i-1)
      logConc <- log(df$Y[-1]/df$Y[-N])
      #tmax
      tmax <- which(df$Y==max(df$Y))
      
      #auc
      if(method=="linear"){
        auc <- 0.5 * sum(diffTimes * sumConc)
      } else {
        auc <- sum(
          #linear up
          0.5 * sum ( diffTimes[1:tmax] * sumConc[1:tmax]),
          #log down
          sum(diffTimes[(tmax+1):(N-1)] * diffConc[(tmax+1):(N-1)]/logConc[(tmax+1):(N-1)])
        )
      }
      
      sumScaleConc <- df$Y[-1] * df$X[-1] + df$Y[-N] * df$X[-N]
      aumc <- 0.5 * sum( diffTimes * sumScaleConc)
      c(AUC=auc, AUMC=aumc)
    }
    dataDf <- data.frame(X=time, Y=conc, ID=id)
    dataDf <- dataDf[order(dataDf$ID, dataDf$X),]
    if(start==0) {addZero <- T} else {addZero <- F}
    byOut <- by(dataDf[,-3], dataDf$ID, innerAuc, addZero = addZero, method=method)
    pars <- data.frame(do.call("rbind", byOut))
    names(pars) <- c("AUC", "AUMC")
    outNames <- dimnames(byOut)[[1]]
    if(is.numeric(id)) outNames <- as.numeric(outNames)
    outDf <- data.frame(ID = outNames, pars)
    return(outDf)
  }
  
  #handle objects
  if(missing(data)) stop("Please supply a data object.\n")
  if (inherits(data,"PMsim")){
    data2 <- data$obs
    #filter by outeq
    data2 <- data2[data2$outeq==outeq,]
    #filter by include/exclude
    if(!missing(include)) data2 <- data2[data2$id %in% include,]
    if(!missing(exclude)) data2 <- data2[-data2$id %in% exclude,]
    #filter by start and end times
    data2 <- data2[data2$time>=start & data2$time<=end,]
#     
#     if(!missing(include)) data2 <- data2[outeq,include,]
#     if(!missing(exclude)) data2 <- data2[outeq,-exclude,]
#     if (length(dim(data2))==3) data2<-data2[outeq,,]
#     if(is.null(dim(data2))) {data2=matrix(data2,nrow=1,dimnames=list(1,names(data2)))} 
#     time.all <- as.numeric(unlist(dimnames(data2)[2]))
#     time.all <- time.all[time.all>=start & time.all<=end]
    
    simAUC <- data.frame(id=unique(data2$id))
    simAUC$tau <- by(data2,data2$id,function(x) unlist(trapAUC(x$time,x$out,method=method))[2])
#     for (i in 1:nrow(data2)){
#       conc <- data2[i,1:length(time.all)]
#       simAUC$tau[i] <- unlist(trapAUC(time.all,conc,method=method))[2]
#     }
    class(simAUC) <- c("PMauc","data.frame")
    return(simAUC)
  }
  if (inherits(data,"PMpost") | inherits(data,"PMpop")){
    data2 <- data[data$block==block,]
    if(!missing(include)) data2 <- data2[data2$id %in% include,]
    if(!missing(exclude)) data2 <- data2[!data2$id %in% exclude,]
    data2 <- data2[data2$outeq==outeq,]
    data2 <- data2[data2$icen==icen,]
    data2 <- data2[data2$time>=start & data2$time<=end,]
    fitAUC <- data.frame(id=unique(data2$id),tau=NA)
    for (i in 1:length(unique(data2$id))){
      temp <- data2[data2$id==unique(data2$id)[i],]
      if(nrow(temp)>0) fitAUC$tau[i] <- unlist(trapAUC(temp$time,temp$pred,method=method))[2]
    }
    class(fitAUC) <- c("PMauc","data.frame")
    return(fitAUC)
  }
  if (inherits(data,"PMop")){
    if(inherits(data,"list")) {data2 <- data[[2*outeq-1]]} else {data2 <- data}
    data2 <- data2[data2$block==block,]
    if(!missing(include)) data2 <- data2[data2$id %in% include,]
    if(!missing(exclude)) data2 <- data2[!data2$id %in% exclude,]
    data2 <- data2[,1:3]
    data2 <- data2[!is.na(data2$obs),]
    data2 <- data2[data2$time>=start & data2$time<=end,]
    fitAUC <- data.frame(id=unique(data2$id),tau=NA)
    for (i in 1:length(unique(data2$id))){
      temp <- data2[data2$id==unique(data2$id)[i],]
      if(nrow(temp)>0) fitAUC$tau[i] <- unlist(trapAUC(temp[,2],temp[,3],method=method))[2]
    }
    class(fitAUC) <- c("PMauc","data.frame")
    return(fitAUC)
  }      
  
  if (!inherits(data,c("PMsim","PMpost","PMop"))){
    
    if (missing(formula)) stop("\nPlease supply a formula of form 'out~time' for objects other than class PMsim, PMpost or PMop.")
    
    y.name <- as.character(attr(terms(formula),"variables")[2])
    x.name <- as.character(attr(terms(formula),"variables")[3])
    
    if (length(grep(y.name,names(data)))==0) {cat(paste("\n'",y.name,"' is not a variable in the data.",sep=""));return()}
    if (length(grep(x.name,names(data)))==0) {cat(paste("\n'",x.name,"' is not a variable in the data.",sep=""));return()}
    if(is.null(data$id)) data$id <- 1
    
    data2 <- data[data[x.name]>=start & data[x.name]<=end,]
    data2 <- data2[,c("id",x.name,y.name)]
    if(!missing(include)) data2 <- data2[data2$id %in% include,]
    if(!missing(exclude)) data2 <- data2[!data2$id %in% exclude,]
    fitAUC <- data.frame(id=unique(data2$id),tau=NA)
    for (i in 1:length(unique(data2$id))){
      temp <- data2[data2$id==unique(data2$id)[i],]
      fitAUC$tau[i] <- unlist(trapAUC(temp[,2],temp[,3],method=method))[2]
    }
    class(fitAUC) <- c("PMauc","data.frame")
    return(fitAUC)
  }
  
}

