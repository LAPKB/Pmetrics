#' Print a Pmetrics Error Object
#'
#' Print the errors in a Pmetrics data file or PMmatrix object.
#'
#' @title Print Data Errors
#' @method print PMerr
#' @param x A PMerr object made by \code{\link{PMcheckMatrix}}.
#' @param \dots Other parameters which are not necessary.
#' @return A printed object.
#' @author Michael Neely
#' @seealso \code{\link{PMcheckMatrix}}
#' @export

print.PMerr <- function(x,...){
  cat("\n")
  okay <- sapply(x,function(x) is.na(x$results[1]))
  if(all(okay)){
    cat("No data errors found.\n")
  } else {
    x <- x[!okay]
    for(i in 1:length(x)){
      cat(paste("\n(",i,") ",x[[i]]$msg," See the 'errors.xlsx' file in current directory.\n",paste(x[[i]]$results,collapse=", "),"\n\n",sep=""))
    }
  }
}
