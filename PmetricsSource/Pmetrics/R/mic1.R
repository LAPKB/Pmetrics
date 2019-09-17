#' Example MIC data
#'
#' This data frame contains MIC data for cefepime against E. coli.  It was obtained 
#' from the EUCAST website at \url{http://mic.eucast.org}.  Select the organism
#' or drug, and then select the desired row of the resulting table to see
#' a histogram (top) and table (bottom) of MIC distributions.
#' 
#' Copy the table into excel, save as a .csv file, and read into R using
#' \code{\link{read.csv}}.  Then use \code{\link{makePTAtarget}}.
#'
#' @name mic1
#' @docType data
#' @title Example MIC data
#' @usage mic1
#' @format An R data frame containing example MIC distribution data in two columns:
#' \itemize{
#' \item mic Minimum inhibitory concentration
#' \item n Number of organisms with the given MIC
#' }  
#' @author Michael Neely
#' @keywords datasets
NULL


