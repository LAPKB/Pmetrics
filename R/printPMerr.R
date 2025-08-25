#' @title Print Data Errors
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Print the errors in a Pmetrics data file or [PM_data] object.
#'
#' @method print PMerr
#' @param x A PMerr object made by validation in [PM_data].
#' @param ... Not used.
#' @return A printed object.
#' @author Michael Neely
#' @seealso [PM_data]
#' @export

print.PMerr <- function(x, ...) {
  cat("\n")
  okay <- sapply(x, function(x) is.na(x$results[1]))
  if (all(okay)) {
    cat("No data errors found.\n")
  } else {
    x <- x[!okay]
    for (i in 1:length(x)) {
      cat(paste("\n(", i, ") ", x[[i]]$msg, " See the 'errors.xlsx' file in current directory.\n", paste(x[[i]]$results, collapse = ", "), "\n\n", sep = ""))
    }
  }
}
