#' @title Read a Pmetrics data file
#' @description
#' `r lifecycle::badge("superseded")`
#' 
#' Reads a Pmetrics .csv matrix input file into R.
#' This function is largely superseded as the function is called automatically
#' when data are intialized as a [PM_data] object with `PM_data$new()`. There
#' is rarely need to call `PMreadMatrix()` directly any longer.
#' 
#' @details
#' As of Pmetrics version 2, the structure of a valid .csv file relaxed.  
#' Minimal required columns are id, time, dose, and out. This function is now included
#' as part of the [PM_data] R6 object to create new `PM_data` objects. Users should
#' rarely have a need to call `PMreadMatrix` as a standalone function unless they
#' continue to use Pmetrics in its legacy mode (versions < 2.0). Note that support 
#' for legacy Pmetrics will eventually wither as the package evolves.
#' 
#' There are a number of other options for columns in the data input.  Details can
#' be found in the [documentation](https://lapkb.github.io/Pmetrics/articles/data.html).
#'
#' @param file The name of the file to be loaded, including the full path if not
#'  in the current working directory (check with [getwd]).
#' @param sep Delimiter between columns, which is a comma by default, but can be changed with
#' [setPMoptions].
#' @param dec Decimal separator, which is a period by default, but can be changed with
#' [setPMoptions].
#' @param quiet Default is `FALSE`.  If `TRUE`, there will be no report to
#'  the console on the contents of file.
#' @param \dots Other parameters to be passed to [readr::read_delim()]
#' @return `PMreadMatrix` returns a data frame of class "PMmatrix". 
#'  If the file is successfully read and `quiet=F`,
#'  the column headers will be reported to the console as a validation check.
#'  Note that this function converts the column headers in the
#'  `file` from upper to lowercase for convenient referencing in R.
#
#' @author Michael Neely 
#' @seealso [PMwriteMatrix], [PMcheck], and [plot.PM_data]


PMreadMatrix <- function(file,
                         sep = getPMoptions("sep"),
                         dec = getPMoptions("dec"),
                         quiet = F,...){
  #get data
  if (missing(file)){
    warning("Please provide filename of Pmetrics data file.\n")
    return(invisible())
  }
  if (!file.exists(file)){
    warning(paste("The file ",sQuote(file)," was not found in the current working directory:\n",getwd(),"\n",sep=""))
    return(invisible(NULL))
  }
  
  #read the first line to understand the format
  headers <- scan(file, what = "character", quiet = T, nlines = 1, 
                  sep = sep, dec = dec, strip.white = T) 
  if(grepl(",",headers)[1]){stop("Your .csv delimiter is not a comma. Use setPMoptions(sep = \";\"), for example.")}
  headers <- headers[headers !=""]
  skip <- ifelse(grepl("POPDATA .*", headers[1]), 1, 0) #0 if current, 1 if legacy

  
  
  args1 <- list(file = file, delim = sep, col_names = T, na=".", 
               locale = readr::locale(decimal_mark = dec),
               skip = skip, show_col_types = F, progress = F, num_threads = 1)
  args2 <- list(...)
  
  args <- modifyList(args1, args2)

  if(quiet){
    data <- suppressWarnings(purrr::exec(readr::read_delim, !!!args))
  } else {
    data <- purrr::exec(readr::read_delim, !!!args)
  }
  
  #remove commented headers and lines
  if(grepl("#",names(data)[1])){
    names(data)[1] <- sub("#","",names(data)[1])
  }
  comments <- grep("#",t(data[,1]))
  if(length(comments) > 0){
    data <- data[-comments, ]
  }
  
  names(data) <- tolower(names(data))
  
  if(!quiet){
    cat(paste("The file",sQuote(file),"contains these columns:\n",sep=" "))
    cat(paste(names(data), collapse = ", "))
    cat("\n")
  }
  
  attr(data,"legacy") <- ifelse(skip == 1, T, F) #if skip = 1, set attribute to TRUE
  class(data) <- c("PMmatrix","data.frame")
  return(data)
}

