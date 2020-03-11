#' Centers for Disease Control Pediatric and Adolescent Growth Data Table
#'
#' Coefficients to calculate sex-specific percentiles of length, weight and head cicumference data 
#' in children from 0 to 18 years.  Downloaded and combined from http://www.cdc.gov/growthcharts/data_tables.htm.
#' Used with the \code{qgrowth} function to generate height and weight percentiles for the purposes of simulation.
#'
#' @name growth
#' @docType data
#' @title CDC Pediatric and Adolescent Growth Data Table
#' @usage growth
#' @format A data frame with the following 9 columns: KNOT (integer age in months); A, B1, B2, B3 (coefficients for calculating
#' percentiles), SEX, AGE, PERCENTILE, and CHART (length x age, wt x age, wt x length, hc x age, or ht x age).
#' @author Michael Neely
#' @keywords datasets

NULL

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

#' Example dataset from an NPAG run.
#'
#' The run consisted of a model with an absorptive compartment and a central compartment.
#' There were 4 parameters in the model: lag time of absorption (Tlag1),
#' rate constant of absorption (Ka), volume (V) and rate constatn of elmination (Ke).
#' Parameters were log transformed.  There were 20 subjects in the dataset.  The run was
#' 100 cycles long and did not converge.
#' 
#' The input files for this run (ex.csv and model.txt) can be downloaded as a zip file from 
#' \url{http://www.lapk.org/Pmetrics_install.php#examples}.
#'
#' @name PMex1
#' @docType data
#' @title Example NPAG Output
#' @usage PMex1
#' @format An R data file containing the output generated at the end of a successful NPAG run.
#' \itemize{
#' \item NPdata.1 made by \code{\link{NPparse}}
#' \item final.1 made by  \code{\link{makeFinal}}
#' \item cycle.1 made by \code{\link{makeCycle}}
#' \item op.1 made by \code{\link{makeOP}}
#' \item cov.1 made by \code{\link{makeCov}}
#' \item pop.1 made by \code{\link{makePop}}
#' \item post.1 made by \code{\link{makePost}}
#' \item mdata.1 the original data file as read by \code{\link{PMreadMatrix}}
#' }  
#' @author Michael Neely
#' @keywords datasets

NULL

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

#' Example dataset for an NPAG run, which has been corrupted with errors.
#'
#' Errors include missing covariate on first line for subject 1, alphanumeric covariate for
#' gender, and trailing dose for subject 1.
#' 
#'
#' @name PMex3
#' @docType data
#' @title Pmetrics data file with errors
#' @usage PMex3
#' @format badData is a PMmatrix object as read by \code{\link{PMreadMatrix}}
#' @author Michael Neely
#' @keywords datasets

NULL







