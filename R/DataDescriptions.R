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

"growth"

#' Example MIC data
#'
#' This data frame contains MIC data for vancomycin against S. aureus.  It was obtained 
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

"mic1"

#' Example output from an NPAG run.
#'
#' The run consisted of a model with an absorptive compartment and a central compartment.
#' There were 4 parameters in the model: lag time of absorption (Tlag1),
#' rate constant of absorption (Ka), volume (V) and rate constant of elmination (Ke).
#' There were 20 subjects in the dataset.  The run was
#' 100 cycles long and did not converge.
#'
#' @name NPdata.1
#' @docType data
#' @title Example NPAG Output
#' @usage NPdata.1
#' @format An R data frame made by \code{\link{NPparse}} at the end of a successful NPAG run.
#' @author Michael Neely
#' @keywords datasets
#' 
"NPdata.1"
#' 
#' Example output from an NPAG run.
#'
#' The run consisted of a model with an absorptive compartment and a central compartment.
#' There were 4 parameters in the model: lag time of absorption (Tlag1),
#' rate constant of absorption (Ka), volume (V) and rate constant of elmination (Ke).
#' There were 20 subjects in the dataset.  The run was
#' 100 cycles long and did not converge.
#'
#' @name final.1
#' @docType data
#' @title Example NPAG Output
#' @usage final.1
#' @format An R data frame made by \code{\link{makeFinal}} at the end of a successful NPAG run.
#' @author Michael Neely
#' @keywords datasets
#' 
"final.1"
#' 
#' Example output from an NPAG run.
#'
#' The run consisted of a model with an absorptive compartment and a central compartment.
#' There were 4 parameters in the model: lag time of absorption (Tlag1),
#' rate constant of absorption (Ka), volume (V) and rate constant of elmination (Ke).
#' There were 20 subjects in the dataset.  The run was
#' 100 cycles long and did not converge.
#'
#' @name cycle.1
#' @docType data
#' @title Example NPAG Output
#' @usage cycle.1
#' @format An R data frame made by \code{\link{makeCycle}} at the end of a successful NPAG run.
#' @author Michael Neely
#' @keywords datasets
#' 
"cycle.1"
#' 
#' Example output from an NPAG run.
#'
#' The run consisted of a model with an absorptive compartment and a central compartment.
#' There were 4 parameters in the model: lag time of absorption (Tlag1),
#' rate constant of absorption (Ka), volume (V) and rate constant of elmination (Ke).
#' There were 20 subjects in the dataset.  The run was
#' 100 cycles long and did not converge.
#'
#' @name op.1
#' @docType data
#' @title Example NPAG Output
#' @usage op.1
#' @format An R data frame made by \code{\link{makeOP}} at the end of a successful NPAG run.
#' @author Michael Neely
#' @keywords datasets
#' 
"op.1"
#' 
#' Example output from an NPAG run.
#'
#' The run consisted of a model with an absorptive compartment and a central compartment.
#' There were 4 parameters in the model: lag time of absorption (Tlag1),
#' rate constant of absorption (Ka), volume (V) and rate constant of elmination (Ke).
#' There were 20 subjects in the dataset.  The run was
#' 100 cycles long and did not converge.
#'
#' @name cov.1
#' @docType data
#' @title Example NPAG Output
#' @usage cov.1
#' @format An R data frame made by \code{\link{makeCov}} at the end of a successful NPAG run.
#' @author Michael Neely
#' @keywords datasets
#' 
"cov.1"
#' 
#' Example output from an NPAG run.
#'
#' The run consisted of a model with an absorptive compartment and a central compartment.
#' There were 4 parameters in the model: lag time of absorption (Tlag1),
#' rate constant of absorption (Ka), volume (V) and rate constant of elmination (Ke).
#' There were 20 subjects in the dataset.  The run was
#' 100 cycles long and did not converge.
#'
#' @name pop.1
#' @docType data
#' @title Example NPAG Output
#' @usage pop.1
#' @format An R data frame made by \code{\link{makePop}} at the end of a successful NPAG run.
#' @author Michael Neely
#' @keywords datasets
#' 
"pop.1"
#' 
#' Example output from an NPAG run.
#'
#' The run consisted of a model with an absorptive compartment and a central compartment.
#' There were 4 parameters in the model: lag time of absorption (Tlag1),
#' rate constant of absorption (Ka), volume (V) and rate constant of elmination (Ke).
#' There were 20 subjects in the dataset.  The run was
#' 100 cycles long and did not converge.
#'
#' @name post.1
#' @docType data
#' @title Example NPAG Output
#' @usage post.1
#' @format An R data frame made by \code{\link{makePost}} at the end of a successful NPAG run.
#' @author Michael Neely
#' @keywords datasets
#' 
"post.1"
#' 
#' Example output from an NPAG run.
#'
#' The run consisted of a model with an absorptive compartment and a central compartment.
#' There were 4 parameters in the model: lag time of absorption (Tlag1),
#' rate constant of absorption (Ka), volume (V) and rate constant of elmination (Ke).
#' There were 20 subjects in the dataset.  The run was
#' 100 cycles long and did not converge.
#'
#' @name mdata.1
#' @docType data
#' @title Example NPAG Output
#' @usage mdata.1
#' @format The original data file as read by \code{\link{PMreadMatrix}} at the end of a successful NPAG run.
#' @author Michael Neely
#' @keywords datasets
#' 
"mdata.1"



#' Example output from an IT2B run.
#'
#' The run consisted of a model with an absorptive compartment and a central compartment.
#' There were 4 parameters in the model: lag time of absorption (Tlag1),
#' rate constant of absorption (Ka), volume (V) and rate constant of elmination (Ke).
#' There were 20 subjects in the dataset.  The run was
#' 100 cycles long and did not converge.
#'
#' @name ITdata.2
#' @docType data
#' @title Example IT2B Output
#' @usage ITdata.2
#' @format An R data frame made by \code{\link{NPparse}} at the end of a successful IT2B run.
#' @author Michael Neely
#' @keywords datasets
#' 
"ITdata.2"
#' 
#' Example output from an IT2B run.
#'
#' The run consisted of a model with an absorptive compartment and a central compartment.
#' There were 4 parameters in the model: lag time of absorption (Tlag1),
#' rate constant of absorption (Ka), volume (V) and rate constant of elmination (Ke).
#' There were 20 subjects in the dataset.  The run was
#' 100 cycles long and did not converge.
#'
#' @name final.2
#' @docType data
#' @title Example IT2B Output
#' @usage final.2
#' @format An R data frame made by \code{\link{makeFinal}} at the end of a successful IT2B run.
#' @author Michael Neely
#' @keywords datasets
#' 
"final.2"
#' 
#' Example output from an IT2B run.
#'
#' The run consisted of a model with an absorptive compartment and a central compartment.
#' There were 4 parameters in the model: lag time of absorption (Tlag1),
#' rate constant of absorption (Ka), volume (V) and rate constant of elmination (Ke).
#' There were 20 subjects in the dataset.  The run was
#' 100 cycles long and did not converge.
#'
#' @name cycle.2
#' @docType data
#' @title Example IT2B Output
#' @usage cycle.2
#' @format An R data frame made by \code{\link{makeCycle}} at the end of a successful IT2B run.
#' @author Michael Neely
#' @keywords datasets
#' 
"cycle.2"
#' 
#' Example output from an IT2B run.
#'
#' The run consisted of a model with an absorptive compartment and a central compartment.
#' There were 4 parameters in the model: lag time of absorption (Tlag1),
#' rate constant of absorption (Ka), volume (V) and rate constant of elmination (Ke).
#' There were 20 subjects in the dataset.  The run was
#' 100 cycles long and did not converge.
#'
#' @name op.2
#' @docType data
#' @title Example IT2B Output
#' @usage op.2
#' @format An R data frame made by \code{\link{makeOP}} at the end of a successful IT2B run.
#' @author Michael Neely
#' @keywords datasets
#' 
"op.2"
#' 
#' Example output from an IT2B run.
#'
#' The run consisted of a model with an absorptive compartment and a central compartment.
#' There were 4 parameters in the model: lag time of absorption (Tlag1),
#' rate constant of absorption (Ka), volume (V) and rate constant of elmination (Ke).
#' There were 20 subjects in the dataset.  The run was
#' 100 cycles long and did not converge.
#'
#' @name cov.2
#' @docType data
#' @title Example IT2B Output
#' @usage cov.2
#' @format An R data frame made by \code{\link{makeCov}} at the end of a successful IT2B run.
#' @author Michael Neely
#' @keywords datasets
#' 
"cov.2"
#' 
#' Example output from an IT2B run.
#'
#' The run consisted of a model with an absorptive compartment and a central compartment.
#' There were 4 parameters in the model: lag time of absorption (Tlag1),
#' rate constant of absorption (Ka), volume (V) and rate constant of elmination (Ke).
#' There were 20 subjects in the dataset.  The run was
#' 100 cycles long and did not converge.
#'
#' @name mdata.2
#' @docType data
#' @title Example IT2B Output
#' @usage mdata.2
#' @format The original data file as read by \code{\link{PMreadMatrix}} at the end of a successful IT2B run.
#' @author Michael Neely
#' @keywords datasets
#' 
"mdata.2"

#' Example dataset for an NPAG/IT2B run, which has been corrupted with errors.
#'
#' Errors include missing covariate on first line for subject 1, alphanumeric covariate for
#' gender, and trailing dose for subject 1.
#' 
#'
#' @name badCSV
#' @docType data
#' @title Pmetrics data file with errors
#' @usage badCSV
#' @format badData is a PMmatrix object as read by \code{\link{PMreadMatrix}}
#' @author Michael Neely
#' @keywords datasets

"badCSV"



#' Example model for an NPAG/IT2B run. There are 4 parameters in the model: lag time of absorption (Tlag1),
#' rate constant of absorption (Ka), volume (V) and rate constant of elmination (Ke).
#' There are 5 covariates: weight in kg (WT), whether from Africa or not (AFRICA), age in years (AGE), 
#' 1 for male (GENDER), and height in cm (HEIGHT).  There is one output equation, and the model uses
#' gamma plus an error polynomial derived from the assay. 
#'

#'
#' @name model
#' @docType data
#' @title Pmetrics model file
#' @usage model
#' @format model is a PMmatrix model.txt file.  See user manual for details on model format.
#' @author Michael Neely
#' @keywords datasets

"model"





