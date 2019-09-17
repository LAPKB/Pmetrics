#' Plots PMvalid objects
#'
#' Plot arguments which may be passed on to the npde plot function via the \dots argument include the following.  
#' Full documentation is available at http://www.npde.biostat.fr.
#' \itemize{
#' \item \code{plot.type}  Control the type of plot. The default is \dQuote{default}. 
#' \itemize{
#' \item \code{default} Combines 4 plots below: QQ-plot, hist, x.scatter, pred.scatter
#' \item \code{data} Plots the observed data in the dataset
#' \item \code{x.scatter} Scatterplot of the npde versus the predictor X (e.g. time)
#' \item \code{pred.scatter} Scatterplot of the npde versus the population predicted values
#' \item \code{vpc} Plots a Visual Predictive Check
#' \item \code{ecdf} Empirical distribution function of the npde (optionally pd or npd)
#' \item \code{hist} Histogram of the npde (optionally pd or npd)
#' \item \code{qqplot} QQ-plot of the npde versus its theoretical distribution (optionally pd or npd)
#' }
#' \item \code{frame.plot}  If TRUE, a box is drawn around the current plot. Default is TRUE.
#' \item \code{xlog}	If TRUE, x axis will be log scale. Default FALSE.
#' \item \code{ylog}	If TRUE, y axis will be log scale. Default FALSE.
#' \item \code{ilist}	List of subject numbers to include in the individual plots. Default is 1:N.
#' \item \code{box}	If TRUE, boxplots are produced instead of scatterplots. Default is FALSE.
#' \item \code{pch.pobs}	Plot character for observations. Default 20 (dot).
#' \item \code{col.pobs}	Color for observations. Default is steelblue4.
#' \item \code{col.lobs}	Color for lines joining observations. Default is steelblue4.
#' \item \code{lty.lobs}	Type for lines joining observations. Default is 1 (solid).
#' \item \code{lwd.lobs}	Width for lines joining observations. Default is 1.
#' \item \code{col.abline}	Color of the horizontal/vertical lines added to the plots. Default is DarkBlue.
#' \item \code{lty.abline}	Type of the lines added to the plots. Default is 2 (dashed).
#' \item \code{wd.abline}	Width of the lines added to the plots. Default is 2.
#' \item \code{col.fillpi}	Color used to fill histograms and prediction bands. Default is slategray1.
#' \item \code{col.fillmed}	Color used to fill prediction band on the median (VPC, npde). Default is pink.
#' \item \code{col.lmed}	Color used to plot the predicted median (VPC, npde). Default is indianred4.
#' \item \code{col.lpi}	Color used to plot lower and upper quantiles. Default is slategrey4.
#' \item \code{lty.lmed}	Line type used to plot the predicted median (VPC, npde). Default is 2 (dashed).
#' \item \code{lty.lpi}	Line type used to plot lower and upper quantiles. Default is 2 (dashed).
#' \item \code{lwd.lmed}	Line width used to plot the predicted median (VPC, npde). Default is 1.
#' \item \code{lwd.lpi}	Line width used to plot lower and upper quantiles. Default is 1.
#' \item \code{bands}	Whether prediction intervals should be plotted. Default is TRUE.
#' \item \code{approx.pi}	If TRUE, samples from N (0, 1) are used to plot prediction intervals, while if FALSE, prediction bands are obtained using npde computed for the simulated data. Default is TRUE.
#' \item \code{vpc.method}	Method used to bin points (one of \dQuote{equal}, \dQuote{width}, \dQuote{user} or \dQuote{optimal}); at least the first two letters of the method need to be specified. Default is \dQuote{equal}.
#' \item \code{vpc.bin}	Number of binning intervals. Default is 10.
#' \item \code{vpc.interval}	Size of interval. Default is 0.95.
#' \item \code{vpc.breaks}	Vector of breaks used with user-defined breaks when vpc.method=\dQuote{user}). Default is NULL.
#' \item \code{vpc.extreme}	Can be set to a vector of 2 values to fine-tune the behaviour of the binning algorithm at the boundaries; specifying c(0.01,0.99) with 
#' the \dQuote{equal} binning method and vpc.bin=10 will create 2 extreme bands containing 1 X-interval, then divide the region 
#' within the two bands into the remaining 8 intervals each containing the same number of data; in this case the intervals will all be equal except for the two 
#' extreme intervals, the size of which is fixed by the user; complete fine-tuning can be obtained by setting the breaks with the vpc.method=\dQuote{user}. Default is NULL.
#' \item \code{pi.size}	Width of the prediction interval on the quantiles. Default is 0.95.
#' \item \code{vpc.lambda}	Value of lambda used to select the optimal number of bins through a penalised criterion. Default is 0.3.
#' \item \code{vpc.beta}	Value of beta used to compute the variance-based criterion (Jopt,beta(I)) in the clustering algorithm. Default is 0.2.
#' \item \code{bands.rep}	Number of simulated datasets used to compute prediction bands. Default is 200.
#' }
#'
#' @title Plot Pmetrics Validation objects
#' @method plot PMvalid
#' @param x The name of an \emph{PMvalid} list object made by \code{\link{makeValid}}
#' @param \dots Other non-standard and standard R graphical parameters to pass to plot.NpdeObject (see details). 
#' @return Plots the object.
#' @author Michael Neely
#' @seealso \code{\link{makeValid}}, \code{\link{plot}}, \code{\link{autonpde}}, \code{\link{par}}
#' @export

plot.PMvalid <- function(x,type="vpc",...){
  
  if(length(grep("ggplot2",installed.packages()[,1]))==0){
    install.packages("ggplot2",repos="http://cran.cnr.Berkeley.edu",dependencies=T)
  }
  ggplot2.installed <- require(ggplot2,quietly=T,warn.conflicts=F)
  if(!ggplot2.installed) stop("Package ggplot2 not installed.")
  
  if(type=="vpc"){
    p <- with(x,ggplot(mapping=aes(x=med2$time,
                                      y=unlist(lapply(quant_Obs,function(x) x[3])))) +
                 geom_line(col="blue",lty=2,lwd=1) + scale_x_continuous(breaks=floor(med2$time)) +
                 geom_polygon(aes(x=time,y=value),data=upperDF,fill="dodgerblue",alpha=0.25) +
                 geom_polygon(aes(x=time,y=value),data=medDF,fill="lightpink",alpha=0.25) +
                 geom_polygon(aes(x=time,y=value),data=lowerDF,fill="dodgerblue",alpha=0.25) +
                 geom_line(aes(x=med2$time,
                               y=unlist(lapply(quant_Obs,function(x) x[2]))),col="red",lty=1,lwd=1) +
                 geom_line(aes(x=med2$time,
                               y=unlist(lapply(quant_Obs,function(x) x[1]))),col="blue",lty=2,lwd=1) + 
                 geom_point(aes(x=time,y=obs),data=tempDF,col="black",pch=1) + scale_y_log10() +
                 xlab("Time (h)") + ylab("Concentration mg/L") 
    )
    print(p)
  }
  if(type=="pcvpc"){
    p <- with(x,ggplot(mapping=aes(x=med2$time,
                                   y=unlist(lapply(quant_pcObs,function(x) x[3])))) +
                geom_polygon(aes(x=time,y=value),data=upperDF,fill="dodgerblue",alpha=0.25) +
                geom_polygon(aes(x=time,y=value),data=medDF,fill="lightpink",alpha=0.25) +
                geom_polygon(aes(x=time,y=value),data=lowerDF,fill="dodgerblue",alpha=0.25) +
                geom_line(col="blue",lty=2,lwd=1) +
                geom_line(aes(x=med2$time,
                              y=unlist(lapply(quant_pcObs,function(x) x[2]))),col="red",lty=1,lwd=1) +
                geom_line(aes(x=med2$time,
                              y=unlist(lapply(quant_pcObs,function(x) x[1]))),col="blue",lty=2,lwd=1) + 
                geom_point(aes(x=time,y=pcObs),data=tempDF,col="black",pch=1) + scale_y_log10() +
                xlab("Time (h)") + ylab("Concentration mg/L") 
    )
    print(p)
  }
  if(type=="npde"){
    plot(x$npde,...)
    par(mfrow=c(1,1))
  }

}