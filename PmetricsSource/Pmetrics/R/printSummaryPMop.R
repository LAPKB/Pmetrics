#' Print a Pmetrics Observed vs. Predicted Summary Object
#'
#' Print a summary of observations, predictions and errors in a summary.PMop object made by \code{\link{summary.PMop}}.
#'
#' @title Print Summary of Observations and Predictions
#' @method print summary.PMop
#' @param x A summary.PMop object made by \code{\link{summary.PMop}}.
#' @param digits Integer, used for number of digits to print.
#' @param \dots Other parameters which are not necessary.
#' @return A printed object.
#' @author Michael Neely
#' @seealso \code{\link{summary.PMop}}
#' @export

print.summary.PMop <- function(x, digits=max(3,getOption("digits")-3),...){
  
  printSumWrk <- function(data,dataname){
    cat(paste("\n",dataname,"\n",sep=""))
    print(data$sumstat)
    cat("\n\n")
    cat(paste("Mean prediction error:",round(data$pe$mpe,digits),"\n"))
    cat(paste("Mean weighted prediction error (bias): ",round(data$pe$mwpe,digits)," (P=",round(data$wtd.t$p.value,digits)," different than 0)\n",sep=""))
    cat(paste("Mean squared prediction error:",round(data$pe$mspe,digits),"\n"))
    cat(paste("Root mean squared error (RMSE):",round(data$pe$rmse,digits),"\n"))
    cat(paste("Percent root mean squared error (%RMSE):",round(data$pe$percent_rmse,digits),"\n"))
    cat(paste("Mean weighed squared prediction error:",round(data$pe$mwspe,digits),"\n"))
    cat(paste("Bias-adjusted mean squared prediction error:",round(data$pe$bamspe,digits),"\n"))
    cat(paste("Bias-adjusted mean weighted squared prediction error (imprecision):",round(data$pe$bamwspe,digits),"\n\n"))
  }
  #function to make summary
  if(inherits(x[[1]],"data.frame")){ #we just have one 
    if(!is.na(x$pe[1])) {printSumWrk(x,"")} else {cat("NA\n")}
    
  } else {
    if(all(unlist(sapply(x,is.na)))){cat("No observations.\n")
    } else {    
      for(i in 1:length(x)){
        if(!is.na(x[[i]][1])) {printSumWrk(x[[i]],paste("$",names(x)[i],sep=""))} else {cat("NA\n")}
      }
    }
    
  }
  
}

