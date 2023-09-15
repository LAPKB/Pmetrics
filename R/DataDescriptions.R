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

#' Centers for Disease Control Pediatric and Adolescent BMI Table
#'
#' Coefficients to calculate sex-specific BMI z-scores and percentiles.  
#' Downloaded from [](https://www.cdc.gov/nccdphp/dnpa/growthcharts/resources/biv-cutoffs.pdf).
#' Tables were last updated in 2000, based on data through 1994. Definitions of overweight
#' and obese come from these data, based on BMI percentile >=85 for overweight and >=95 
#' for obese. See [ger_bmi] for percentiles based on more modern NHANES data.
#' 
#' @name cdc_bmi
#' @docType data
#' @title CDC Pediatric and Adolescent BMI Table
#' @usage cdc_bmi
#' @format A data frame with the following 9 columns: Sex (1 = male), Agemos; 
#' L, M, S (coefficients for calculating z-scores), P3, P5, P10, P25, P50, P75, 
#' P85, P90, P95, P97: age and sex specific BMI percentiles
#' @author Michael Neely
#' @keywords datasets

"cdc_bmi"


#' Revised Pediatric and Adolescent BMI Table
#'
#' Coefficients to calculate sex-specific BMI z-scores and percentiles based on the
#' supplemental data published by Gerhart et al:
#' Gerhart, Jacqueline G., Fernando O. Carreño, Andrea N. Edginton, Jaydeep Sinha, Eliana M. Perrin, 
#' Karan R. Kumar, Aruna Rikhi, et al. “Development and Evaluation of a Virtual 
#' Population of Children with Obesity for Physiologically Based Pharmacokinetic Modeling.” 
#' Clinical Pharmacokinetics 61, no. 2 (February 2022): 307–20. [](https://doi.org/10.1007/s40262-021-01072-4).
#' These data are in the same format as [cdc_bmi] but are derived from more recent NHANES data.
#'
#' @name ger_bmi
#' @docType data
#' @title CDC Pediatric and Adolescent BMI Table
#' @usage ger_bmi
#' @format A data frame with the following 9 columns: Sex (1 = male), Agemos; 
#' L, M, S (coefficients for calculating z-scores), P3, P5, P10, P25, P50, P75, 
#' P85, P90, P95, P97: age and sex specific BMI percentiles
#' @author Michael Neely
#' @keywords datasets

"ger_bmi"

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
#' This is an R6 Pmetrics [PM_result] object created with [PM_load] after an NPAG run.
#' The run consisted of a model with an absorptive compartment and a central compartment.
#' There were 4 parameters in the model: lag time of absorption (Tlag1),
#' rate constant of absorption (Ka), volume (V) and rate constant of elmination (Ke).
#' There were 20 subjects in the dataset.  The run was
#' 100 cycles long and did not converge.
#'
#' @name NPex
#' @docType data
#' @title Example NPAG Output
#' @usage NPex
#' @format R6 [PM_result]
#' @author Michael Neely
#' @keywords datasets
#'

"NPex"

#' Example output from an NPAG run with validation.
#'
#' This is an R6 Pmetrics [PM_result] object created with [PM_load] after an NPAG run.
#' The run consisted of a model with an absorptive compartment and a central compartment.
#' There were 4 parameters in the model: lag time of absorption (Tlag1),
#' rate constant of absorption (Ka), volume (V) with weight as a covariate, and 
#' rate constant of elmination (Ke).
#' There were 20 subjects in the dataset.  The run was
#' 100 cycles long and did not converge. It was then validated with the `$validate`
#' method for [PM_result] objects.
#'
#' @name NPex_val
#' @docType data
#' @title Example NPAG Output with validation
#' @usage NPex_val
#' @format R6 [PM_result]
#' @author Michael Neely
#' @keywords datasets
#'

"NPex_val"

#' Example output from an IT2B run.
#'
#' This is an R6 Pmetrics [PM_result] object created with [PM_load] after an NPAG run.
#' The run consisted of a model with an absorptive compartment and a central compartment.
#' There were 4 parameters in the model: lag time of absorption (Tlag1),
#' rate constant of absorption (Ka), volume (V) and rate constant of elmination (Ke).
#' There were 20 subjects in the dataset.  The run was
#' 100 cycles long and did not converge.
#'
#' @name ITex
#' @docType data
#' @title Example IT2B Output
#' @usage ITex
#' @format R6 [PM_result]
#' @author Michael Neely
#' @keywords datasets
#' 

"ITex"

#' Example data set for an NPAG/IT2B run.
#'
#' Data are kindly supplied by Chuck Peloquin, PharmD. They consist of multiple 
#' rifapentine oral doses followed by 6-7 concentrations in 20 adult subjects.
#' Covariates include weight (kg), africa (origin), age (years), 
#' gender (1 = male), and height (cm).
#' 
#' @name dataEx
#' @docType data
#' @title Pmetrics data file 
#' @usage dataEx
#' @format [PM_data]
#' @author Michael Neely
#' @keywords datasets

"dataEx"

#' Example data set for an NPAG/IT2B run, which has been corrupted with errors.
#'
#' Errors include missing covariate on first line for subject 1, 
#' alphanumeric covariate for
#' gender, and trailing dose for subject 1.
#' 
#'
#' @name badData
#' @docType data
#' @title Pmetrics data file with errors
#' @usage badData
#' @format [PM_data]
#' @author Michael Neely
#' @keywords datasets

"badData"



#' Example model for an NPAG/IT2B run. There are 4 parameters in the model: lag time of absorption (Tlag1),
#' rate constant of absorption (Ka), volume (V) and rate constant of elmination (Ke).
#' There are 5 covariates: weight in kg (WT), whether from Africa or not (AFRICA), age in years (AGE), 
#' 1 for male (GENDER), and height in cm (HEIGHT).  There is one output equation, and the model uses
#' gamma plus an error polynomial derived from the assay. 
#'

#'
#' @name modEx
#' @docType data
#' @title Pmetrics model object 
#' @usage modEx
#' @format R6 [PM_model]
#' @author Michael Neely
#' @keywords datasets

"modEx"

#' Example simulator output
#'
#' This is an R6 Pmetrics [PM_sim] object created by
#' created by running `$sim()` on a [PM_result] object, e.g.
#' `NPex$sim(include = 1:4, limits = NA, nsim = 100).`
#'
#' @name simEx
#' @docType data
#' @title Example simulator output
#' @usage simEx
#' @format R6 [PM_sim]
#' @author Michael Neely
#' @keywords datasets
#'

"simEx"


#' Model Library
#'
#' This is a tibble containing models in the Pmetrics library.
#'
#' @name modelLibrary
#' @docType data
#' @title Pmetrics Model Library
#' @usage modelLibrary
#' @format A tibble with the following columns
#' * **ncomp** The number of compartments in the model, including any bolus
#' depo compartment(s).
#' * **par** Either "K" for parameterization with rate constants, or "CL" for
#' clearances.
#' * **route** Input route(s)
#' * **elim** List of compartment numbers with elimination
#' * **mod** the [PM_model]
#' * **name** Model name in plain English
#' * **algebraic** Token indicating algebraic solution. Format is a character
#' vector of 4 elements:
#'   - P\[\]: primary parameters of at least Ke, V, and optionally Ka, KCP and KPC,
#'   e.g. P\[Ke,V\]
#'   - B\[\]: Bolus input compartment numbers, 0 for no bolus, e.g. B\[1\]
#'   - R\[\]: RateIV inputs, 0 for no IV, e.g. R\[2\]
#'   - O\[\]: Output compartments, e.g. O\[2\]
#' For example, one compartment IV model with no oral input: 
#' "P\[Ke,V\], B\[0\], R\[1\], O\[1\]"
#' @author Michael Neely
#' @keywords datasets
#'

"modelLibrary"

#' @name locales
#' @docType data
#' @title Pmetrics locales
#' @usage locales
#' @format Dataframe with languages and iso693 two- and three-letter codes.
#' @author Michael Neely
#' @keywords datasets

"locales"


#' @name model
#' @docType data
#' @title Pmetrics model
#' @usage model
#' @format Sample model text
#' @author Michael Neely
#' @keywords datasets

"model"