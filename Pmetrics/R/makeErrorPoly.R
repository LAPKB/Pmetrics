#' This function plots first, second, and third order polynomial functions fitted
#' to pairs of observations and associated standard deviations for a given output assay.
#' In this way, the standard deviation associated with any observation may be calculated and
#' used to appropriately weight that observation in the model building process.  Observations
#' are weighted by the reciprocal of the variance, or squared standard deviation.
#'
#' @title Assay error polynomial coefficients
#' @param obs A vector of observations
#' @param sd A vector of standard deviations obtained from repeated measurements at each
#' observation in \code{obs}
#' @param data A Pmetrics data file.  From this, the maximum and mininimum observations will be retrieved.
#' This is useful to ensure that calculated standard deviations are not negative
#' at any observation in the dataset.  If not specified, the default is the maximum \emph{obs}.
#' @param outeq The output equation in \emph{data}.  Default is 1.
#' @param col Color of the data points. Default is red.
#' @param cex Relative size of the data points.  Default is 3. See \code{\link{par}}.
#' @param pch Ploting symbol.  Default is \dQuote{+}.  See \code{\link{par}}.
#' @param lcol Color of the fitted polynomial lines.  Default is blue.
#' @param lwd Width of the lines. Default is 2.
#' @param ref Add a reference line at SD 0 to help evaluate that all fitted SDs are >0.  Default is true.
#' @param legend Boolean argument to plot legend.  Default is \code{TRUE}.
#' @param \dots Other plotting parameters as in \code{\link{plot.default}} and \code{\link{par}}
#' @return A plot of the measured observations and fitted polynomial curves and a list with the
#' first, second, and third order coefficients
#' @author Michael Neely
#' @examples
#' makeErrorPoly(obs=c(0,5,50,100,250,500,1000),sd=c(1,0.4,4.5,12,34,60,190))

makeErrorPoly <- function(obs,sd,data,outeq=1,col="red",cex=3,pch="+",lcol="blue",lwd=2,ref=T,legend=T,...){
  
  if(length(obs)<2 | length(sd)<2 | length(obs)!=length(sd)) stop("Minimum of 2 obs/sd pairs required and vectors must be of equal length.\n")
  if(missing(data)) {maxobs <- max(obs)} else {maxobs <- max(data$out[data$evid==0 & data$outeq==outeq],na.rm=T)}
  
  arglist <- list(...)
  
  
  lm.1 <- lm(sd~obs)
  lm.2 <- lm(sd~obs + I(obs**2))
  lm.3 <- lm(sd~obs + I(obs**2) + I(obs**3))
  coef.1 <- coef(lm.1)
  coef.2 <- coef(lm.2)
  if(any(is.na(coef.2))) {bad.2 <- T} else {bad.2 <- F}
  coef.3 <- coef(lm.3)
  if(any(is.na(coef.3))) {bad.3 <- T} else {bad.3 <- F}
  
  
  
  r.1 <- round(summary(lm.1)$r.squared,5)
  if(!bad.2) {r.2 <- round(summary(lm.2)$r.squared,5)} else {r.2 <- "NA"}
  if(!bad.3) {r.3 <- round(summary(lm.3)$r.squared,5)} else {r.3 <- "NA"}
  
  
  newConc <- 0:ceiling(maxobs)
  
  predict.1 <- predict(lm.1,data.frame(obs=newConc))
  if(!bad.2) {predict.2 <- predict(lm.2,data.frame(obs=newConc))} else {predict.2 <- predict.1}
  if(!bad.3) {predict.3 <- predict(lm.3,data.frame(obs=newConc))} else {predict.3 <- predict.1}
  
  ylim <- c(min(c(predict.1,predict.2,predict.3,sd[which(obs<=maxobs)])),max(c(predict.1,predict.2,predict.3,sd[which(obs<=maxobs)])))
  if(length(arglist)>0){
    if(!"ylim" %in% names(arglist)) {arglist <- list(arglist,ylim)}
  } else {arglist <- list(ylim=ylim)}
  par(mar=c(5,5,4,2)+0.1)
  do.call(plot,args=c(list(y=sd,x=obs,type="n",xlim=range(newConc),
                           xlab="Concentration",ylab="Standard Deviation",
                           cex.lab=1.5,font=2),arglist))
  points(sd~obs,col=col,pch=pch,cex=cex)
  lines(x=newConc,y=predict.1,lty=1,lwd=lwd,col=lcol)
  if(!bad.2) lines(x=newConc,y=predict.2,lty=2,lwd=lwd,col=lcol)
  if(!bad.3) lines(x=newConc,y=predict.3,lty=3,lwd=lwd,col=lcol)
  if(ref) abline(h=0,lty=2)
  if(legend){
    legend(x="topleft",legend=c(as.expression(substitute(paste(1^{st}," degree ",R^2," = ", r.1),list(r.1=r.1))),
                                as.expression(substitute(paste(2^{nd}," degree ",R^2," = ", r.2),list(r.2=r.2))),
                                as.expression(substitute(paste(3^{rd}," degree ",R^2," = ", r.3),list(r.3=r.3)))),
           lty=c(1,2,3),bty="n",lwd=lwd,col=lcol)
  }
  if(bad.2) cat("Note: Second and third degree polynomials could not be fitted to the data.\n")
  if(!bad.2 & bad.3) cat("Note: A third degree polynomial could not be fitted to the data.\n")
  second <- c(NA,NA,NA) ; second[!bad.2] <- coef.2
  third <- c(NA,NA,NA,NA) ; third[!bad.3] <- coef.3
  
  names(coef.1) <- c("C0","C1")
  names(second) <- c("C0","C1","C2")
  names(third) <- c("C0","C1","C2","C3")
  
  return(list(first=coef.1,second=second,third=third))
  
}



