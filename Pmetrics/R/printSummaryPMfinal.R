#' Print a Pmetrics Final Summary Object
#'
#' Print a summary of parameter medians and MAWD, with point estimates and credibilty intervals
#' from a summary.PMfinal object made by \code{\link{summary.PMfinal}}.
#'
#' @title Print Summary of Parameter Values and Credibility Intervals
#' @method print summary.PMfinal
#' @param x A summary.PMfinal object made by \code{\link{summary.PMfinal}}.
#' @param digits Integer, used for number of digits to print.
#' @param \dots Other parameters which are not necessary.
#' @return A printed object.
#' @author Michael Neely
#' @seealso \code{\link{summary.PMfial}}
#' @export

print.summary.PMfinal <- function(x, digits=max(3,getOption("digits")-3),...){
  
  cat(paste("\nWeighted Medians (", 100*(max(x$percentile)-min(x$percentile)),
            "% credibility interval)\n",sep=""))  
  for(i in 1:(ncol(x)-2)){
    cat(paste(colnames(x[,i]),": ",round(x[2,i],digits)," (",round(x[1,i],digits)," - ",round(x[3,i],digits),")\n",sep=""))
  }
  
  cat(paste("\nMedian Absolute Weighed Differences (similar to variance) (", 100*(max(x$percentile)-min(x$percentile)),
            "% credibility interval)\n",sep=""))
  
  for(i in 1:(ncol(x)-2)){
    cat(paste(colnames(x[,i]),": ",round(x[5,i],digits)," (",round(x[4,i],digits)," - ",round(x[6,i],digits),")\n",sep=""))
  }
  
 

}

