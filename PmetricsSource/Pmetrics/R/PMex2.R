#' Exmaple dataset from an IT2B run.
#'
#' The run consisted of a model with an absorptive compartment and a central compartment.
#' There were 4 parameters in the model: lag time of absorption (Tlag1),
#' rate constant of absorption (Ka), volume (V) and rate constatn of elmination (Ke).
#' Parameters were log transformed.  There were 20 subjects in the dataset.  The run was
#' 20 cycles long and did converge.
#' 
#' The input files for this run (ex.csv and model.txt) can be downloaded as a zip file from 
#' \url{http://www.lapk.org/Pmetrics_install.php#examples}.
#'
#' @name PMex2
#' @docType data
#' @title Example IT2B Output
#' @usage PMex2
#' @format An R data file containing the output generated at the end of a successful IT2B run.
#' \itemize{
#' \item ITdata.1 made by \code{\link{ITparse}}
#' \item final.1 made by  \code{\link{makeFinal}}
#' \item cycle.1 made by \code{\link{makeCycle}}
#' \item op.1 made by \code{\link{makeOP}}
#' \item cov.1 made by \code{\link{makeCov}}
#' \item mdata.1 the original data file as read by \code{\link{PMreadMatrix}}
#' } 
#' @author Michael Neely
#' @keywords datasets
NULL

