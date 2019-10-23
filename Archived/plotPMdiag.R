#' Plot a Pmetrics Simulation Diagnostic Object
#'
#' Includes qqnorm prediction discrepancy (pd) plot, pd histogram, pd vs. time, and pd vs. prediction 
#'
#' Plot a PMdiag object made by \code{\link{PMdiag}}.
#'
#' @title Plot a Pmetrics Simulation Diagnostic Object
#' @method plot PMdiag
#' @param x A PMdiag object made by \code{\link{PMdiag}}.
#' @param out Direct output to a PDF, EPS or image file.  Format is a named list whose first argument, 
#' \code{type} is one of the following character vectors: \dQuote{pdf}, \dQuote{eps} (maps to \code{postscript}),
#' \dQuote{\code{png}}, \dQuote{\code{tiff}}, \dQuote{\code{jpeg}}, or \dQuote{\code{bmp}}.  Other named items in the list
#' are the arguments to each graphic device. PDF and EPS are vector images acceptable to most journals
#' in a very small file size, with scalable (i.e. infinite) resolution.  The others are raster images which may be very
#' large files at publication quality dots per inch (DPI), e.g. 800 or 1200. Default value is \code{NA} which means the 
#' output will go to the current graphic device (usually the monitor). For example, to output an eps file,
#' out=list(\dQuote{eps}) will generate a 7x7 inch (default) graphic.
#' @param \dots Other parameters which are not necessary.
#' @return A plot.
#' @author Michael Neely
#' @seealso \code{\link{PMdiag}}
#' @S3method plot PMdiag
#' @references 
#' Brendel K, Comets E, Laffont CM, Laveille C, Mentre F (2006) Metrics for external model evaluation with an application to the population pharmacokinetics of gliclazide. \emph{Pharmaceutical Research}, 23:2036-49
#' 
#' Mentre F, Escolano S (2006) Prediction discrepancies for the evaluation of nonlinear mixed-effects models. \emph{J Pharmacokinet Pharmacodyn}, 33:345-67
#'
#' Wang DD, Zhang S. (2011) Standardized Visual Predictive Check Versus Visual Predictive Check for Model Evaluation. \emph{J Clin Pharmacol}, epub ahead of print



plot.PMdiag <- function (x,out=NA,...)
{
  #     plot.npde <- F  #disable npde for now  
  #     plot.SVPC <- F  #disable SVPC for now in favor of pd
  
  #choose output
  if(inherits(out,"list")){
    if(out$type=="eps") {setEPS();out$type <- "postscript"}
    if(length(out)>1) {do.call(out$type,args=out[-1])} else {do.call(out$type,list())}
  }
  
  plot.pd <- T
  pd <- x$pd
  #npde <- x$npde 
  xobs <- x$obsdat$time
  ypred <- x$ypred
  PIJ <- x$obsdat$PIJ
  
  
  
  #    breaks <- 40
  #    par(mfrow = c(2, 2))
  
  # #npde
  # if(plot.npde){    
  #     qqnorm(sort(npde), xlab = "Sample quantiles (npde)", ylab = "Theoretical Quantiles", 
  #         cex.lab = 1.5, main = "Q-Q plot versus N(0,1) for npde")
  #     qqline(sort(npde))
  #     
  #     xh <- hist(npde, breaks = breaks, xlab = "npde", main = "Distribution of npde", 
  #         cex.lab = 1.5)
  #     xpl <- min(npde) + c(0:100)/100 * (max(npde) - min(npde))
  #     ypl <- dnorm(xpl)
  #     ypl <- ypl/max(ypl) * max(xh$counts)
  #     lines(xpl, ypl, lwd = 2)
  #     
  #     plot(xobs, npde, xlab = "Time", ylab = "npde", cex.lab = 1.5)
  #     abline(h = 0, lty = 2)
  #     x1 <- qnorm(0.05)
  #     abline(h = x1, lty = 3)
  #     abline(h = (-x1), lty = 3)
  #     
  #     plot(ypred, npde, xlab = "Predicted", ylab = "npde", cex.lab = 1.5)
  #     abline(h = 0, lty = 2)
  #     abline(h = x1, lty = 3)
  #     abline(h = (-x1), lty = 3)
  # }   
  # #SVPC
  # 
  # if(plot.SVPC){    
  #     plot(y=PIJ,x=xobs, type="n", xlab = "Time", ylab = "SVPC Pij", cex.lab=1.5, ylim=c(0,1))
  #     points (y=PIJ,x=xobs, cex=1, col=1)    
  #     abline(h = 0.05,lty=3)
  #     abline(h = 0.95,lty=3)
  #     abline(h = 0.5,lty=2)
  # }
  #     
  #pd
  if(plot.pd){
    nclass <- 10
    par(mfrow = c(2, 2))
    samp <- sort(pd)
    ndat <- length(samp)
    theo <- c(1:ndat)/ndat
    qqplot(samp, theo, xlab = "Sample quantiles (pd)", ylab = "Theoretical Quantiles", 
           cex.lab = 1.2, main = "Q-Q plot versus U(0,1) for pd")
    segments(0, 0, 1, 1)
    xh <- hist(pd, nclass = nclass, xlab = "pd", main = "", cex.lab = 1.2)
    abline(h = ndat/nclass, lty = 2, lwd = 2)
    plot(xobs, pd, xlab = "Time", ylab = "pd", cex.lab = 1.2)
    abline(h = 0.5, lty = 2)
    abline(h = 0.05, lty = 3)
    abline(h = 0.95, lty = 3)
    plot(ypred, pd, xlab = "Predicted Observation", ylab = "pd", cex.lab = 1.2)
    abline(h = 0.5, lty = 2)
    abline(h = 0.05, lty = 3)
    abline(h = 0.95, lty = 3)
    par(mfrow = c(1, 1))
  }
  #close device if necessary
  if(inherits(out,"list")) dev.off()
  
}

