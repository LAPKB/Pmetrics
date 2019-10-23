#' Compare NPAG or IT2B runs
#' 
#' Objects can be specified separated by commas, e.g. PMcompare(1,2,3) followed by
#' any arguments you wish to \code{\link{plot.PMop}}, \code{\link{mtsknn.eq}}. P-values are based on comparison using the nearest neighbors
#' approach if all models are non-parametrics.  Models may only be compared on parameters that are included
#' in the first model.  The P-value is the comparison between each model and the first model in
#' the list.  Missing P-values are when a model has no parameter names in common with the first
#' model, and for the first model compared to itself, or when models from IT2B runs are included.  Significant P-values indicate that the null
#' hypothesis should be rejected, i.e. the joint distributions between the two compared models are 
#' significantly different.
#'
#' @title Compare NPAG or IT2B runs
#' @param x The run number of the first object you wish to compare. This should be a folder in your
#' working directory. To avoid confusion, this function does not use objects
#' already loaded with \code{\link{PMload}}.
#' This will serve as the reference output for P-value testing (see details).
#' @param y The run number of the second object to compare.
#' @param \dots Additional run numbers to compare.  See details.  Also, parameters to be passed to \code{\link{plot.PMop}} 
#' if \code{plot} is true as well as to \code{\link{mtsknn.eq}}.  Order does not matter.
#' @param icen Can be either "median" for the predictions based on medians of \code{pred.type} parameter value
#' distributions, or "mean".  Default is "median".#' @param outeq Number of the output equation to compare; default is 1
#' @param plot Boolean operator selecting whether to generate observed vs. predicted plots for each data object
#' as in \code{\link{plot.PMop}}
#' @return A data frame with the following objects for each model to analyze:
#'  \item{run }{The run number of the data}
#'  \item{type }{NPAG or IT2B data}
#'  \item{nsub }{Number of subjects in the model}
#'  \item{nvar }{Number of random parameters in the model}
#'  \item{par }{Names of random parameters}
#'  \item{cycles }{Number of cycles run}
#'  \item{converge }{Boolean value if convergence occurred.}
#'  \item{ll }{Final cycle -2*Log-likelihood }
#'  \item{aic }{Final cycle Akaike Information Criterion}
#'  \item{bic }{Final cycle Bayesian (Schwartz) Information Criterion }
#'  \item{popBias }{Bias, or mean weighted prediction error of predictions based on population parameters minus observations}
#'  \item{popImp }{Imprecision, or bias-adjusted mean weighted squared error of predictions based on population parameters minus observations }
#'  \item{popPerRMSE}{Percent root mean squared error of predictions based on population parameters minus observations}
#'  \item{postBias }{Bias, or mean weighted prediction error of predictions - observations  based on posterior parameters}
#'  \item{postImp }{Imprecision, or bias-adjusted mean weighted squared error of predictions - observations based on posterior parameters}
#'  \item{postPerRMSE}{Percent root mean squared error of predictions based on posterior parameters minus observations}
#'  \item{pval }{P-value for each model compared to the first. See details.}
#' @author Michael Neely
#' @seealso \code{\link{PMload}}, \code{\link{plot.PMop}}, \code{\link{mtsknn.eq}}


PMcompare <- function (x,y,...,icen="median",outeq=1,plot=F){
  if(missing(x) | missing(y)) stop("You must specify at least two run numbers for PMcompare.\n")
  if(inherits(x,c("NPdata","ITdata"))) stop("You should specify your objects by run number.  See help.\n")
  
  #parse dots
  arglist <- list(...)
  namesPlot <- names(formals(plot.PMop))
  namesMTSKNN <- names(formals(mtsknn.eq))
  #get the args to plot.PMop and set defaults if missing
  plotArgs <- which(names(arglist) %in% namesPlot)
  argsPlot <- arglist[plotArgs]
  if (!"cex.stat" %in% names(argsPlot)) argsPlot$cex.stat <- 0.8
  if (!"x.stat" %in% names(argsPlot)) argsPlot$x.stat <- 0.5
  #get the args to mtsknn.eq and set defaults if missing
  MTSKNNargs <- which(names(arglist) %in% namesMTSKNN)
  argsMTSKNN <- arglist[MTSKNNargs]
  if (!"k" %in% names(argsMTSKNN)) argsMTSKNN$k <- 3
  if (!"print" %in% names(argsMTSKNN)) argsMTSKNN$print <- FALSE
  #get the others if there and assume that they are PMdata objects for now
  if((length(arglist) - length(c(plotArgs,MTSKNNargs)))>0){
    if(length(c(plotArgs,MTSKNNargs))==0) {argsPM <- 1:length(arglist)} else {argsPM <- (1:length(arglist))[-c(plotArgs,MTSKNNargs)]}
  } else {argsPM <- NULL}
  
  if(length(argsPM)==0) obj <- list(x,y)
  if(length(argsPM)>=1) obj <- c(list(x,y),arglist[argsPM])
  
  #get each obj
  nobj <- length(obj)
  allObj <- list()
  for(thisobj in 1:nobj){
    #find  objects
    if(!file.exists(as.character(obj[thisobj]))){
      cat(paste(obj[thisobj]," is not a folder in the current working directory.\n",sep=""))
    } else {
      ITfile <- list.files(paste(obj[thisobj],"outputs",sep="/"),pattern="IT2Bout.Rdata",full.names=T)
      NPfile <- list.files(paste(obj[thisobj],"outputs",sep="/"),pattern="NPAGout.Rdata",full.names=T)
      load(c(ITfile,NPfile))
      if(length(ITfile)>0) {allObj[[thisobj]] <- IT2Bout$ITdata} else {allObj[[thisobj]] <- NPAGout$NPdata}
      
    } 
  }
  
  
  
  
  objClass <- mapply(class, allObj)
  #check for non-Pmetrics data objects and remove them if found
  yesPM <- which(objClass %in% c("NPAG","IT2B"))
  allObj <- allObj[yesPM]
  objClass <- objClass[yesPM]
  
  
  #check for zero cycle objects
  cycles <- unlist(sapply(allObj,function(x) x$icyctot))
  if(any(cycles==0)) stop(paste("Do not include 0-cycle runs: item(s) ",paste(which(cycles==0),collapse=", "),"\n",sep=""))
  op <- apply(mapply(makeOP, allObj),2,function(x) {data.frame(x)})
  op <- lapply(op,function(x) {class(x) <- c("PMop","data.frame");x})
  if (plot) {
    if (!"resid" %in% names(argsPlot)) {
      if (nobj <= 3) {
        par(mfrow = c(nobj, 2))
      }
      else {
        par(mfrow = c(3, 2))
        devAskNewPage(ask = T)
      }
      for (i in 1:length(op)) {
        do.call(plot.PMop, args=c(list(x=op[[i]], pred.type="pop",icen=icen,outeq=outeq,                        
                                       main = paste("Model",i, "Population")),argsPlot))
        do.call(plot.PMop, args=c(list(x=op[[i]], pred.type="post",icen=icen,outeq=outeq, 
                                       main = paste("Model",i, "Posterior")),argsPlot))
      }
    }
    else {
      devAskNewPage(ask = T)
      for (i in 1:length(op)) {
        do.call(plot.PMop,args=c(list(x=op[[i]], pred.type="post",icen=icen,outeq=outeq,
                                      main = paste("Model",i)),argsPlot))
      }
    }
    par(mfrow = c(1, 1))
    devAskNewPage(ask = F)
  }
  
  
  #get summaries of op for outeq
  sumobjPop <- mapply(summary.PMop,op,MoreArgs=list(outeq=outeq,pred.type="pop",icen=icen),SIMPLIFY=F)
  sumobjPost <- mapply(summary.PMop,op,MoreArgs=list(outeq=outeq,pred.type="post",icen=icen),SIMPLIFY=F)
  
    
  popBias <- sapply(sumobjPop,function(x) ifelse(is.na(x$pe[1]),NA,x$pe$mwpe))
  postBias <- sapply(sumobjPost,function(x) ifelse(is.na(x$pe[1]),NA,x$pe$mwpe))
  popImp <- sapply(sumobjPop,function(x) ifelse(is.na(x$pe[1]),NA,x$pe$bamwspe))
  postImp <- sapply(sumobjPost,function(x) ifelse(is.na(x$pe[1]),NA,x$pe$bamwspe))
  popPercent_RMSE <- sapply(sumobjPop,function(x) ifelse(is.na(x$pe[1]),NA,x$pe$percent_rmse))
  postPercent_RMSE <- sapply(sumobjPost,function(x) ifelse(is.na(x$pe[1]),NA,x$pe$percent_rmse))
  
  #if all NPAG, calculate nearest neighbors p-value compared to first
  if(all(sapply(allObj,function(x) inherits(x,"NPAG")))){
    #get population points
    final <- mapply(makeFinal, allObj)
    #find intersecting parameters
    popPointsRef <- final[,1]$popPoints
    namesRef <- names(popPointsRef)
    popPointsOther <- lapply(2:nobj,function(x) final[,x]$popPoints)
    t <- sapply(2:nobj,function(x) {
      thisPopPoints <- popPointsOther[[x-1]]
      namesThis <- names(thisPopPoints)
      intersect <- namesRef[namesRef %in% namesThis]
      if(length(intersect)>0){
        popPoints1 <- popPointsRef[,intersect]
        popPoints2 <- thisPopPoints[,intersect]
        t <- do.call(mtsknn.eq,args=c(list(x=popPoints1,y=popPoints2),argsMTSKNN))$pval
      } else {t <- NA}
      signif(t,3)
    })
    
    t <- c(NA,t)
  } else {t <- NA}
  
  results <- data.frame(run=unlist(obj),
                        type = objClass, 
                        nsub = mapply(function(x) x$nsub, allObj), 
                        nvar = mapply(function(x) x$nvar, allObj), 
                        par = mapply(function(x) paste(x$par,collapse = " "), allObj),
                        converge = mapply(function(x) x$converge==1, allObj), 
                        ll = mapply(function(x) -2*x$ilog[length(x$ilog)], allObj), 
                        aic = mapply(function(x) tail(x$iic[,1], 1), allObj), 
                        bic = mapply(function(x) tail(x$iic[,2], 1), allObj), 
                        popBias = popBias,
                        popImp = popImp,
                        popPer_RMSE = popPercent_RMSE,
                        postBias = postBias,
                        postImp = postImp,
                        postPer_RMSE = postPercent_RMSE,
                        pval = t)
  names(results)[7] <- "-2*LL"
  results[,7:15] <- format(results[,7:15],digits=4)
  row.names(results) <- 1:nobj
  results
}
