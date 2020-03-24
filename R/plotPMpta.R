#' Plots PMpta objects
#'
#' This function will plot the percent target attainment for objects made with the \code{\link{makePTA}} function.
#' For the legend, defaults that are different that the standard are:
#' \itemize{
#'   \item x Default \dQuote{topright}
#'   \item legend Default will be the labeled regimen names supplied during \code{\link{makePTA}}, 
#'   or if missing, \dQuote{Regimen 1, Regimen 2,...Regimen n}, where \emph{n} is the number of 
#'   regimens in the PMpta object.  
#'   This default can be overridden by a supplied character vector of regimen names.
#'   \item col The color of each Regimen plot as specified by the default color scheme or \code{col}
#'   \item pch The plotting character for each Regimen plot as specified by the default plotting characters or \code{pch}
#'   \item lty The line type of each Regimen plot as specified by the default line types or \code{lty}
#'   \item bg Default \dQuote{white}
#' }
#'
#' @title Plot PMpta Percent Target Attainment objects
#' @method plot PMpta
#' @param x The name of an \emph{PMpta} data object read by \code{\link{makePTA}}
#' @param include A vector of simulations (regimens) to include in the plot, e.g. c(1,3)
#' @param exclude A vector of simulations (regimens) in the plot, e.g. c(2,4:6)
#' @param plot.type Character vector controlling type of plot.  
#' Default is \dQuote{pta}, which plots proportion with success on the y-axis and target on the x-axis.
#' The other choice is \dQuote{pdi}, which plots the median pdi (pharmacodynamic index), e.g. AUC/MIC, on the
#' y-axis, and target on the x-axis. 
#' @param log Boolean operator to plot x-axis in logarithmic scale; the default is \code{True}
#' @param pch Vector of integers which control the plotting symbol for each regimen curve; the default is 1:nsim.  NA results in no symbol.
#' Use 0 for open square, 1 for open circle, 2 for open triangle, 3 for cross, 4 for X, or 5 for a diamond.
#' Other alternatives are \dQuote{*} for asterisks, \dQuote{.} for tiny dots, or \dQuote{+} for a smaller,
#' bolder cross.  These plotting symbols are standard for R (see \code{\link{par}}).
#' @param grid Either a boolean operator to plot a reference grid, or a list with elements x and y,
#' each of which is a vector specifying the native coordinates to plot grid lines; the default is \code{False}.
#' For example, grid=list(x=seq(0,24,2),y=1:10).  Defaults for missing x or y will be calculated by \code{\link{axTicks}}.
#' @param xlab Label for the x axis.  Default is \dQuote{MIC}
#' @param ylab Label for the y axis.  Default is \dQuote{Proportion with success}
#' @param col A vector of color names to be used for each regimen plotted.  If the
#' length of \code{col} is too short, values will be recycled.
#' @param lty A vector of line types to be used for each regimen plotted.  If the
#' length of \code{lty} is too short, values will be recycled.
#' @param lwd Line width, with default of 4.
#' @param legend Either a boolean operator or a list of parameters to be supplied to the \code{\link{legend}}
#' function (see its documentation).  If \code{False}, a legend will not be plotted.
#' If \code{True} (the default), the default legend parameters will be used, as documented in that function, with exceptions
#' as noted in \emph{Details}.
#' @param ci Confidence interval around curves on \code{pdi} plot, on scale of 0 to 1. Default is 0.9.
#' @param out Direct output to a PDF, EPS or image file.  Format is a named list whose first argument, 
#' \code{type} is one of the following character vectors: \dQuote{pdf}, \dQuote{eps} (maps to \code{postscript}),
#' \dQuote{\code{png}}, \dQuote{\code{tiff}}, \dQuote{\code{jpeg}}, or \dQuote{\code{bmp}}.  Other named items in the list
#' are the arguments to each graphic device. PDF and EPS are vector images acceptable to most journals
#' in a very small file size, with scalable (i.e. infinite) resolution.  The others are raster images which may be very
#' large files at publication quality dots per inch (DPI), e.g. 800 or 1200. Default value is \code{NA} which means the 
#' output will go to the current graphic device (usually the monitor). For example, to output an eps file,
#' out=list(\dQuote{eps}) will generate a 7x7 inch (default) graphic.
#' @param \dots Other parameters as found in \code{\link{plot.default}}.
#' @return Plots the object.
#' @author Michael Neely
#' @seealso \code{\link{makePTA}}, \code{\link{plot}}, \code{\link{par}}, \code{\link{axis}}
#' @export


plot.PMpta <- function(x,include,exclude,plot.type="pta",log=T,pch,
                       grid,xlab,ylab,col,lty,lwd=4,
                       legend=T,ci=0.9,out=NA,...){
  
  #choose output
  if(inherits(out,"list")){
    if(out$type=="eps") {setEPS();out$type <- "postscript"}
    if(length(out)>1) {do.call(out$type,args=out[-1])} else {do.call(out$type,list())}
  }
  
  if(!inherits(x,"PMpta")) stop("Please supply a PMpta object made by makePTA().\n")
  
  #check input 
  simnum <- 1:max(x$outcome$simnum)
  if(!missing(include)){
    if(any(include>max(simnum))){
      stop(paste("PMpta object does not have ",max(simnum)," simulations.\n",sep=""))
    }else{simnum <- simnum[include]}
  } 
  if(!missing(exclude)){
    if(any(exclude>max(simnum))){
      stop(paste("PMpta object does not have ",max(simnum)," simulations.\n",sep=""))
    }else{simnum <- simnum[-exclude]}  } 
  
  #choose xlab as Target if targets were set or Regimen if targets were simulated
  simTarg <- 1+as.numeric(attr(x,"simTarg")) #1 if missing or set, 2 if random
  if(length(simTarg)==0) simTarg <- 1
  if(missing(xlab)) xlab <- switch(simTarg,"Target","Regimen")
  if(missing(ylab)) ylab <- switch(plot.type,pdi="Pharmacodynamic Index",pta="Proportion with success","Proportion with success")
  if(simTarg==1){
    logscale <- c("","x")[1+as.numeric(log)]
  } else {logscale <- ""}
  nsim <- length(simnum)
  if(missing(pch)) {pch <- 1:nsim} else {pch <- rep(pch,nsim)}
  if(missing(col)) {col <- rep(c("black","red","blue","green","purple","orange"),nsim)} else {col <- rep(col,nsim)}
  if(missing(lty)) {lty <- 1:nsim} else {lty <- rep(lty,nsim)}     
  if(class(legend)=="list"){
    legend$plot<- T
    if(is.null(legend$x)) legend$x <- "topright"
    if(is.null(legend$bg)) legend$bg <- "white"
    if(is.null(legend$col)) legend$col <- col
    if(is.null(legend$pch)) legend$pch <- pch
    if(is.null(legend$lty)) legend$lty <- lty
    if(is.null(legend$legend)){
      legendText <- attr(x,"simlabels")
      if(is.null(legendText)) legendText <- paste("Regimen",simnum)
      legend$legend <- legendText
    } 
    
  } else {
    if(legend){
      legendText <- attr(x,"simlabels")
      if(is.null(legendText)) legendText <- paste("Regimen",simnum)
      legend <- list(plot=T,x="topright",bg="white",col=col,lty=lty,pch=pch,legend=legendText)} else {legend <- list(plot=F)}
  }
  
  if(plot.type=="pdi"){ #pdi plot

    
    if(simTarg==1){ #set targets
      pdi.median <- tapply(x$results$pdi,list(x$results$target,x$results$simnum),median,na.rm=T)
      pdi.lower <- tapply(x$results$pdi,list(x$results$target,x$results$simnum),quantile,probs=0.5-ci/2,na.rm=T)
      pdi.upper <- tapply(x$results$pdi,list(x$results$target,x$results$simnum),quantile,probs=0.5+ci/2,na.rm=T)
      targets <- as.numeric(row.names(pdi.median))
      plot(x=range(targets),y=range(c(pdi.lower[,simnum],pdi.upper[,simnum]),na.rm=T),type="n",xlab=xlab,ylab=ylab,log=logscale,xaxt="n",...)
      axis(side=1,at=targets,labels=targets,lwd=1,...)
    } else { #random targets
      pdi.median <- tapply(x$results$pdi,x$results$simnum,median,na.rm=T)
      pdi.lower <- tapply(x$results$pdi,x$results$simnum,quantile,probs=0.5-ci/2,na.rm=T)
      pdi.upper <- tapply(x$results$pdi,x$results$simnum,quantile,probs=0.5+ci/2,na.rm=T)
      plot(x=range(1:nsim),y=range(c(pdi.lower[simnum],pdi.upper[simnum]),na.rm=T),type="n",xlab=xlab,ylab=ylab,log=logscale,xaxt="n",...)
      axisLabels <- attr(x,"simlabels")
      if(is.null(axisLabels)) axisLabels <- paste("Regimen",simnum)
      axis(side=1,at=1:nsim,labels=axisLabels,lwd=1,...)
    }
    
    #make grid if necessary
    if(missing(grid)){
      grid <- list(x=NA,y=NA)
    } else {
      if(inherits(grid,"logical")){
        if(grid){
          grid <- list(x=targets,y=axTicks(2))
        } else {
          grid <- list(x=NA,y=NA)
        }
      }
      if(inherits(grid,"list")){
        if(is.null(grid$x)) grid$x <- targets
        if(is.null(grid$y)) grid$y <- axTicks(2)
      }
    }
    abline(v=grid$x,lty=1,col="lightgray")
    abline(h=grid$y,lty=1,col="lightgray")
    
    if(simTarg==1){ #set targets
      if(ci>0){
        for(i in simnum){
          if(ci>0) polygon(x=c(targets,rev(targets)),
                           y=c(pdi.upper[,i],rev(pdi.lower[,i])),
                           col=rgb(red=col2rgb(col[i])[1,1],
                                   green=col2rgb(col[i])[2,1],
                                   blue=col2rgb(col[i])[3,1],
                                   alpha=50,maxColorValue=255),border=NA)        
        }
      }
      for(i in 1:nsim){
        lines(x=targets,y=pdi.median[,simnum[i]],type="o",lty=lty[i],lwd=lwd,col=col[i],pch=pch[i],...)
      }
      if(legend$plot) do.call("legend",legend)
    } else { #random targets
      for(i in 1:nsim){
        points(x=i,y=pdi.median[simnum[i]])
        arrows(x0=i,y0=pdi.lower[simnum[i]],
               x1=i,y1=pdi.upper[simnum[i]],angle=90,code=3,
               lwd=1) #draw error bars
      }
    }
    
  }else{ #pta plot
    
    temp <- x$outcome[x$outcome$simnum %in% simnum,]
    if(simTarg==1){ #set targets
      plot(prop.success~target,temp,type="n",xlab=xlab,ylab=ylab,log=logscale,xaxt="n",...)
      axis(side=1,at=unique(temp$target),lwd=1,...)
      #make grid if necessary
      if(missing(grid)){
        grid <- list(x=NA,y=NA)
      } else {
        if(inherits(grid,"logical")){
          if(grid){
            grid <- list(x=unique(temp$target),y=axTicks(2))
          } else {
            grid <- list(x=NA,y=NA)
          }
        }
        if(inherits(grid,"list")){
          if(is.null(grid$x)) grid$x <- unique(temp$target)
          if(is.null(grid$y)) grid$y <- axTicks(2)
        }
      }
      abline(v=grid$x,lty=1,col="lightgray")
      abline(h=grid$y,lty=1,col="lightgray")
      
      #draw plot
      for(i in 1:nsim){
        lines(prop.success~target,temp[temp$simnum==simnum[i],],type="o",lty=lty[i],lwd=lwd,col=col[i],pch=pch[i],...)   
      }
      #legend
      if(legend$plot) do.call("legend",legend)
      
    } else { #random targets
      plot(prop.success~simnum,temp,type="n",xlab=xlab,ylab=ylab,log=logscale,xaxt="n",...)
      axisLabels <- attr(x,"simlabels")
      if(is.null(axisLabels)) axisLabels <- paste("Regimen",simnum)
      axis(side=1,at=1:nsim,labels=axisLabels,lwd=1,...) 
      #make grid if necessary
      if(missing(grid)){
        grid <- list(x=NA,y=NA)
      } else {
        if(inherits(grid,"logical")){
          if(grid){
            grid <- list(x=simnum,y=axTicks(2))
          } else {
            grid <- list(x=NA,y=NA)
          }
        }
        if(inherits(grid,"list")){
          if(is.null(grid$x)) grid$x <- simnum
          if(is.null(grid$y)) grid$y <- axTicks(2)
        }
      }
      abline(v=grid$x,lty=1,col="lightgray")
      abline(h=grid$y,lty=1,col="lightgray")
      
      #draw plot
      lines(prop.success~simnum,temp,type="o",...)
      
    }
  }
  
  
  #close device if necessary
  if(inherits(out,"list")) dev.off()
  
}
