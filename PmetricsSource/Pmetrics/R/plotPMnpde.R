#' Plots PMnpde objects
#'
#' This function is wrapper around the plot.NpdeObjects invisible method of Comets et al
#' in the npde package for R.  Full documentation is available at 
#' http://www.npde.biostat.fr. 
#' 
#' Plot arguments which may be passed on to the npde plot function via the \dots argument include:
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
#' @title Plot Pmetrics normalized prediction distribution errors
#' @method plot PMnpde
#' @param x The name of an \emph{PMnpde} list object made by \code{\link{makeNPDE}}
#' @param outeq Plot the NPDE or VPC for which output equation.  Default is 1.
#' @param \dots Other non-standard and standard R graphical parameters to pass to plot.NpdeObject (see details). 
#' @return Plots the object.
#' @author Michael Neely
#' @seealso \code{\link{makeNPDE}}, \code{\link{plot}}, \code{\link{autonpde}}, \code{\link{par}}
#' @export


plot.PMnpde <- function(x,outeq=1,...){
  
  if(length(x)<outeq) stop(paste("There is no output equation ",outeq,".\n",sep=""))
  data <- x[[outeq]]
  if(length(data)==0) stop(paste("NPDE for output ",outeq," was not calculated when makeNPDE was run.\n",sep=""))

  plot(data,...)  
  
}
