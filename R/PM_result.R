#' Results of a Pmetrics run
#'
#' This object contains all of the results after a Pmetrics runs. It is created 
#' by using the [PM_load] function.
#' 
#' After a run completes, results are stored on your hard drive. They are loaded
#' back into R with [PM_load] to create the [PM_result] object, which contains both
#' the results and functions to analyze or plot the result.
#' 
#' @author Michael Neely, Julian Otalvaro
#' @export
PM_result <- R6::R6Class(
  "PM_result",
  public <- list(
    #' @field NPdata List with all output from NPAG, made by [NPparse]
    NPdata = NULL,
    #' @field ITdata List with all output from IT2B, made by [ITparse]
    ITdata = NULL,
    #' @field pop A [PM_pop] object
    pop = NULL,
    #' @field post A [PM_post] object
    post = NULL,
    #' @field final A [PM_final] object
    final = NULL,
    #' @field cycle A [PM_cycle] object
    cycle = NULL,
    #' @field op A [PM_op] object
    op = NULL,
    #' @field cov A [PM_cov] object
    cov = NULL,
    #' @field data [PM_data] object representing the original .csv data file used in the run
    data = NULL,
    #' @field model text string representing the original model file used in the run
    model = NULL,
    #' @field errfile Name of error file if it exists
    errfile = NULL,
    #' @field success Boolean if successful run
    success = NULL,
    #' @field valid If the `$validate` method has been executed after a run, this object will be added to
    #' the `PM_result` object.  It contains the information required to plot visual predictive checks and normalized prediction
    #' error discrepancies via the npde code developed by Comets et al. Use the 
    #' `$save` method on the augmented `PM_result` object to save it with the 
    #' new validation results.
    valid = NULL,

    #' @description
    #' Create new object populated with data from previous run
    #' @details
    #' Creation of new `PM_result` objects is via [PM_load].
    #' @param out The parsed output from [PM_load].
    #' @param quiet Quietly validate. Default is `FALSE`.
    initialize = function(out, quiet = T) {
      if(!is.null(out$NPdata)){
        self$NPdata <- out$NPdata
        class(self$NPdata) <- c("NPAG", "list")
      } else {self$NPdata <- NULL}
      if(!is.null(out$ITdata)){
        self$ITdata <- out$ITdata
        class(self$ITdata) <- c("IT2B", "list")
      } else {self$ITdata <- NULL}
      self$pop <- if is.null(out$pop){NULL}else{PM_pop$new(out$pop)}
      self$post <- is.null(out$pop){NULL}else{PM_post$new(out$post)}
      self$final <- PM_final$new(out$final)
      self$cycle <- PM_cycle$new(out$cycle)
      self$op <- PM_op$new(out$op)
      self$cov <- PM_cov$new(out$cov)
      self$data <- PM_data$new(data = out$data, quiet = quiet) # no need to report
      self$model <- out$model
      self$errfile <- out$errfile
      self$success <- out$success
    },

    #' @description
    #' Plot generic function based on type
    #' @param type Type of plot based on class of object
    #' @param ... Plot-specific arguments
    plot = function(type, ...) {
      if(is.null(type)){stop("Please provide the type of plot.")
      } else {self[[type]]$plot(...)}
    },
    
    #' @description
    #' Summary generic function based on type
    #' @param type Type of summary based on class of object
    #' @param ... Summary-specific arguments
    summary = function(type, ...) {
      if(is.null(type)){stop("please provide the type of summary you want to obtain")
        } else {self[[type]]$summary(...)} 
    },
    
    #' @description
    #' AUC generic function based on type
    #' @param type Type of AUC based on class of object
    #' @param ... Summary-specific arguments
    auc = function(type, ...) {
      if(!type %in% c("op", "pop", "post", "sim")){
        stop("makeAUC is defined only for PMop, PMpop, PMpost, PMsim objects.\n")
      }
      self[[type]]$auc(...)
    },
    
    #' @description
    #' Perform non-compartmental analysis
    #' @details
    #' See [makeNCA].
    #' @param ... Arguments passed to [makeNCA].
    nca = function(...) {
      make_NCA(self, ...)
    },
    
    #' @description
    #' Simulates using the self$final object.
    #' For parameter information refer to [SIMrun]. It will return a `PM_sim` object
    #' by running [SIMparse] at the end of the simulation.
    #' @param ... Parameters passed to [SIMrun]
    sim = function(...) {
      #store copy of the final object
      bk_final <- self$final$clone()
      sim <- PM_sim$run(self, ...)
      self$final <- bk_final
      sim
    },
    
    #' @description
    #' Save the current PM_result object into a .rds file. This is useful if you
    #' have run the `$make_valid` method on a `PM_result` object, which returns 
    #' an internal simulation based validation as a new `valid` field. To save this
    #' validation, use the `$save` method. You could also save the result if you wish
    #' to share it with someone else.
    #' @param file_name Name of the file to be created, the default is PMresult.rds
    save = function(file_name = "PMresult.rds") {
      saveRDS(self, file_name)
    },
    
    #' @description 
    #' Validate the result by internal simulation methods.
    #' @param ... Arguments passed to [make_valid].
    validate = function(...) {
      self$valid <- PM_valid$new(self, ...)
      self$valid
    },
    
    #' @description 
    #' Conduct stepwise linear regression of Bayesian posterior parameter values
    #' and covariates.
    #' @param ... Arguments passed to [PMstep].
    step = function(...) {
      PMstep(self$cov$data, ...)
    },
    
    #' @description 
    #' Calculate optimal sample times from result and template data file.
    #' @param ... Arguments passed to [MM_opt].
    MM_opt = function(...) {
      MM_opt(self, ...)
    },
    
    #' @description
    #' This function loads an rds file created using the `$save` method on a 
    #' `PM_result` object.
    #' @details
    #' `PM_result` objects contain a `save` method which invokes [saveRDS] to write
    #' the object to the hard drive as an .rds file. This is the corresponding load
    #' function.
    #' @param file_name Name of the file to be read, the default is "PMresult.rds".
    #' @return A `PM_result` object.
    #' @examples 
    #' \dontrun{newRes <- PM_result$load("PMresult.rds")}
    load = function(file_name){
      return(invisible)
    }
  ) # end public
) # end PM_result

#' Load results of previously saved analyses
#' 
#' If the `$save` method has previously been invoked on a [PM_result] object that 
#' was changed, for  example by using the `$validate` method, this function
#' will load those results.
#' 
#' The saved object is an .rds file. When loaded, it should be assigned to an R
#' object, e.g. `run2 <- PM_result$load("filename")`. This contrasts with loading
#' unmodified results after a run, which is accomplished with [PM_load].
#' 
#' @param file_name The name of the .rds file to load.
#' @return A [PM_result] object
#' @export
#' @name PM_result
PM_result$load <- function(file_name = "PMresult.rds") {
  readRDS(file_name)
}

#' Observed vs. predicted data
#' 
#' Contains observed vs. predicted data after a run
#' 
#' @details
#' Contains the results of [makeOP], which is a 
#' data frame suitable for analysis and plotting of observed vs. population or
#' or individual predicted outputs. To provide a more traditional experience in R,
#' the data frame is separated by columns into fields, e.g. `id` or `time`. This 
#' allows you to access them in an S3 way, e.g. `run1$op$time` if `run1` is a 
#' `PM_result` object. 
#' 
#' However, if you wish to manipulate the entire data frame,
#' use the `data` field, e.g. `trough <- run1$op$data %>% filter(time == 24)`. If
#' you are unfamiliar with the `%>%` pipe function, please type `help("%>%", "magrittr")`
#' into the R console and look online for instructions/tutorials in tidyverse, a
#' powerful approach to data manipulation upon which Pmetrics is built.
#' 
PM_op <- R6::R6Class(
  "PM_op",
  public <- list(
    #' @field data A data frame with the following columns
    #' * id Subject identification
    #' * time observation time in relative units, usually hours
    #' * obs observation
    #' * pred prediction
    #' * pred.type Population predictions based on Bayesian prior parameter value distribution,
    #' or individual predictions based on Bayesian posterior parameter value distributions
    #' * icen Predictions based on mean or median of Bayesian `pred.type`parameter values
    #' * outeq output equation number
    #' * block dosing block number for each subject, as defined by dose resets (evid=4).
    #' * obsSD standard deviation of the observation based on the assay error polynomial
    #' * d prediction error, `pred` - `obs`
    #' * ds squared prediction error
    #' * wd weighted prediction error, which is the prediction error divided by the \code{obsSD}
    #' * wds weighted squared prediction error
    data = NULL,
    #' @description
    #' Create new object populated with observed vs. predicted data
    #' @details
    #' Creation of new `PM_op` object is automatic and not generally necessary
    #' for the user to do.
    #' @param op The parsed output from [makeOP].
    initialize = function(op) {
      class(op) <- append("tidy_op",class(op))
      self$data <- op
    },
    #' @description
    #' Plot method
    #' @details
    #' See [plot.PM_op].
    #' @param ... Arguments passed to [plot.PM_op]
    plot = function(...) {
      plot.PM_op(self, ...)
    },
    #' @description
    #' Summary method
    #' @details
    #' See [summary.PMop].
    #' @param ... Arguments passed to [summary.PMop]
    summary = function(...) {
      summary.PMop(self$data, ...)
    },
    #' @description
    #' Calculate AUC
    #' @details 
    #' See [makeAUC]
    #' @param ... Arguments passed to [makeAUC]
    auc = function(...) {
      makeAUC(data = self$data, ...)
    }
  )
)

#' Wrapper function for summmary.PMop
#' 
#' This redirects to summary.PMop for PM_op R6 objects
#' 
#' See [summary.PMop]. Alternative way to summarize is
#' `PM_result$op$summary()`.
#' 
#' @param obj The *PM_op* object to summarize
#' @param ... Arguments passed to [summary.PMop]
#' @return A [summary.PMop] object
#' @export
summary.PM_op <- function(obj, ...) {
  obj$summary(...)
}

#' Wrapper function for plot.PM_op
#' 
#' This redirects to plot.PM_op for tidy_op objects
#' 
#' See [plot.PM_op]. Alternative way to plot is
#' `PM_result$op$plot()`.
#' 
#' @param x A *PMop* object with the *tidy_op* class to be plotted
#' @param ... Arguments passed to [plot.PM_op]
#' @return A [plot.PM_op] object
#' @export
plot.tidy_op <- function(x,...){
  plot.PM_op(x,...)
}

#' Individual Bayesian posterior predictions at short intervals
#' 
#' Contains the Bayesian posterior predictions at short intervals 
#' specified as an argument to [PM_fit$run]. Default is every 12 minutes.
#' 
#' @details
#' Contains the results of [makePost], which is a 
#' data frame with Bayesian posterior predicted outputs for all subjects. 
#' To provide a more traditional experience in R,
#' the data frame is separated by columns into fields, e.g. `id` or `time`. This 
#' allows you to access them in an S3 way, e.g. `run1$post$time` if `run1` is a 
#' `PM_result` object. 
#' 
#' However, if you wish to manipulate the entire data frame,
#' use the `data` field, e.g. `trough <- run1$post$data %>% filter(time == 24)`. If
#' you are unfamiliar with the `%>%` pipe function, please type `help("%>%", "magrittr")`
#' into the R console and look online for instructions/tutorials in tidyverse, a
#' powerful approach to data manipulation upon which Pmetrics is built.
#' @author Michael Neely, Julian Otalvaro
#' @export
PM_post <- R6::R6Class(
  "PM_post",
  public <- list(
    #' @field data A data frame with the following columns
    #' * id Subject id
    #' * time Time of predictions in decimal hours
    #' * icen Prediction based on mean or median of Bayesian posterior parameter distribution
    #' * outeq Output equation number
    #' * pred Predicted output for each outeq
    #' * block Observation blocks within subjects as defined by *EVID=4* dosing events
    data = NULL,    
    #' @description
    #' Create new object populated with Bayesian posterior predicted data at 
    #' regular, frequent intervals
    #' @details
    #' Creation of new `PM_post` object is automatic and not generally necessary
    #' for the user to do.
    #' @param post The parsed output from [makePost].
    initialize = function(post) {
      class(post) <- append("tidy_post",class(post))
      self$data <- post
    },
    #' @description
    #' Calculate AUC
    #' @details 
    #' See [makeAUC]
    #' @param ... Arguments passed to [makeAUC]
    auc = function(...) {
      makeAUC(data = self$data, ...)
    }
  )
)

#' Final Cycle Population Values
#' 
#' Contains final cycle information from run.
#' 
#' @details
#' Contains the results of [makeFinal], which is a 
#' list suitable for analysis and plotting of final cycle population values.
#' 
#' However, if you wish to manipulate the entire data frame,
#' use the `data` field, e.g. `probs <- run1$final$data$popPoints %>% select(prob)`. 
#' This will select the probabilities of the support points. If
#' you are unfamiliar with the `%>%` pipe function, please type `help("%>%", "magrittr")`
#' into the R console and look online for instructions/tutorials in tidyverse, a
#' powerful approach to data manipulation upon which Pmetrics is built.

PM_final <- R6::R6Class(
  "PM_final",
  public <- list(
    #' @field popPoints (NPAG only) Data frame of the final cycle joint population density of grid points
    #'  with column names equal to the name of each random parameter plus *prob* for the
    #'  associated probability of that point
    popPoints = NULL,
    #' @field popMean The final cycle mean for each random parameter distribution
    popMean = NULL,
    #' @field popSD The final cycle standard deviation for each random parameter distribution
    popSD = NULL,
    #' @field popCV The final cycle coefficient of variation (SD/Mean) for each random parameter distribution
    popCV = NULL,
    #' @field popVar The final cycle variance for each random parameter distribution
    popVar = NULL,
    #' @field popCov The final cycle random parameter covariance matrix
    popCov = NULL,
    #' @field popCor The final cycle random parameter correlation matrix
    popCor = NULL,
    #' @field popMedian The final cycle median values for each random parameter,
    #' i.e. those that have unknown mean and unknown variance, both of which are 
    #' fitted during the run
    popMedian = NULL,
    #' @field popRanFix The final cycle median values for each parameter that is
    #' random but fixed to be the same for all subjects, i.e. unknown mean, zero
    #' variance, with only mean fitted in the run
    popRanFix = NULL,
    #' @field postPoints (NPAG only) Data frame of posterior population points for each of the first 100 subject,
    #' with columns id, point, parameters and probability.  The first column is the subject, the second column has the population
    #' point number, followed by the values for the parameters in that point and the probability.
    postPoints = NULL,
    #' @field postMean A *nsub* x *npar* data frame containing 
    #' the means of the posterior distributions for each parameter.
    postMean = NULL,
    #' @field postSD A *nsub* x *npar* data frame containing 
    #' the SDs of the posterior distributions for each parameter.
    postSD = NULL,
    #' @field postVar A *nsub* x *npar* data frame containing 
    #' the variances of the posterior distributions for each parameter.
    postVar = NULL,
    #' @field postCov NPAG only: An array of dimensions *npar* x *npar* x *nsub* that
    #' contains the covariances of the posterior distributions for each parameter and subject.*
    postCov = NULL,
    #' @field postCor NPAG only: An array of dimensions *npar* x *npar* x *nsub* that
    #' contains the correlations of the posterior distributions for each parameter and subject.
    postCor = NULL,
    #' @field postMed A *nsub* x *npar* data frame containing 
    #' the medians of the posterior distributions for each parameter.*
    postMed = NULL,
    #' @field shrinkage A data frame with the shrinkage for each parameter.  `popVar`
    #' is comprised of variance(EBE) + variance(EBD), where EBE is the Empirical 
    #' Bayes Estimate or mean of the posterior
    #' distribution for the parameter. EBD is the Empirical Bayes Distribution, or
    #' the full Bayesian posterior distribution. In other words, if Bayesian 
    #' posterior distributions are wide
    #' for a given parameter due to sparse or uninformative sampling, 
    #' then most of the population variance is due
    #' to this variance and shrinkage of the EBE variance is high because 
    #' individual posterior estimates
    #' shrink towards the population mean.
    shrinkage = NULL,
    #' @field gridpts (NPAG only) Initial number of support points
    gridpts = NULL,
    #' @field nsub Number of subjects
    nsub = NULL,
    #' @field ab Matrix of boundaries for random parameter values
    ab = NULL,
    #' @field data A data frame combining all the above fields as its columns
    data = NULL,
    #' @description
    #' Create new object populated with final cycle information 
    #' @details
    #' Creation of new `PM_final` object is automatic and not generally necessary
    #' for the user to do.
    #' @param final The parsed output from [makeFinal].
    initialize = function(final) {
      class(final) <- append("tidy_final",class(final))
      self$data <- final
      self$popPoints <- final$popPoints
      self$popMean <- final$popMean
      self$popSD <- final$popSD
      self$popCV <- final$popCV
      self$popVar <- final$popVar
      self$popCov <- final$popCov
      self$popCor <- final$popCor
      self$popMedian <- final$popMedian
      self$popRanFix <- final$popRanFix
      self$postPoints <- final$postPoints
      self$postMean <- final$postMean
      self$postSD <- final$postSD
      self$postVar <- final$postVar
      self$postCov <- final$postCov
      self$postCor <- final$postCor
      self$postMed <- final$postMed
      self$shrinkage <- final$shrinkage
      self$gridpts <- final$gridpts
      self$nsub <- final$nsub
      self$ab <- final$ab
      class(self) <- c(c("NPAG", "IT2B")[1+as.numeric(is.null(self$popPoints))], class(self))
    },
    #' @description
    #' Summary method
    #' @details
    #' See [summary.PMfinal].
    #' @param ... Arguments passed to [summary.PMfinal]
    summary = function(...) {
      summary.PMfinal(self$data, ...)
    },
    #' @description
    #' Plot method
    #' @details
    #' See [plot.PMfinal].
    #' @param ... Arguments passed to [plot.PMfinal]
    plot = function(...) {
      plot.PM_final(self, ...)
    }
  )
)

#' Wrapper function for summmary.PMfinal
#' 
#' This redirects to summary.PMfinal for PM_final R6 objects
#' 
#' See [summary.PMfinal]. Alternative way to summarize is
#' `PM_result$final$summary()`.
#' 
#' @param obj The *PM_final* object to summarize
#' @param ... Arguments passed to [summary.PMfinal]
#' @return A [summary.PMfinal] object
#' @export
summary.PM_final <- function(obj, ...) {
  obj$summary(...)
}


#' Pmetrics Run Cycle Information
#' 
#' Contains the cycle information after a run.
#' 
#' @details
#' This contains the output of [makeCycle] after a run, which
#' generates information suitable for analysis and plotting of cycle information.
#' Each field corresponds to a column in the complete data frame.
#' 
#' To manipulate the entire data frame,
#' use the `data` field, e.g. `final <- run1$cycle$data %>% slice_tail(n=1)`. If
#' you are unfamiliar with the `%>%` pipe function, please type `help("%>%", "magrittr")`
#' into the R console and look online for instructions/tutorials in tidyverse, a
#' powerful approach to data manipulation upon which Pmetrics is built.

PM_cycle <- R6::R6Class(
  "PM_cycle",
  public <- list(
    #' @field data A data frame with the following columns
    #' * names Vector of names of the random parameters
    #' * cynum Vector cycle numbers, which may start at numbers greater 
    #' than 1 if a non-uniform prior was specified for the run (NPAG only)
    #' * ll Matrix of cycle number and -2*Log-likelihood at each cycle
    #' * gamlam A matrix of cycle number and gamma or lambda at each cycle
    #' * mean A matrix of cycle number and the mean of each random parameter 
    #' at each cycle, normalized to initial mean
    #' * sd A matrix of cycle number and the standard deviation of each random parameter
    #' at each cycle,  normalized to initial standard deviation
    #' * median A matrix of cycle number and the median of each random 
    #' parameter at each cycle,  normalized to initial median
    #' * aic A matrix of cycle number and Akaike Information Criterion at each cycle
    #' * bic A matrix of cycle number and Bayesian (Schwartz) Information Criterion at each cycle
    data = NULL,
    #' @description
    #' Create new object populated with  cycle information 
    #' @details
    #' Creation of new `PM_cycle` object is automatic and not generally necessary
    #' for the user to do.
    #' @param cycle The parsed output from [makeCycle].
    initialize = function(cycle) {
      class(cycle) <- append("tidy_cycle",class(cycle))
      self$data <- cycle
    },
    #' @description
    #' Plot method
    #' @details
    #' See [plot.PMcycle].
    #' @param ... Arguments passed to [plot.PMcycle]
    plot = function(...) {
      plot.PMcycle(self$data, ...) #update to (self,...) when plot.PM_cycle done
    }
  )
)

# #' Wrapper function for plot.PM_cycle
# #' 
# #' This redirects to plot.PM_cycle for tidy_cycle objects
# #' 
# #' See [plot.PMcycle]. Alternative way to plot is
# #' `PM_result$cycle$plot()`.
# #' 
# #' @param x A *PMcycle* object with the *tidy_cycle* class to be plotted
# #' @param ... Arguments passed to [plot.PMcycle]
# #' @return A [plot.PM_cycle] object
# #' @export
# plot.tidy_cycle <- function(x,...){
#   plot.PMcycle(x,...)
# }

#' Population predictions at short intervals
#' 
#' Contains the population predictions at short intervals 
#' specified as an argument to [PM_fit$run]. Default is every 12 minutes.
#' 
#' Contains the results of [makePop], which is a 
#' data frame with population predicted outputs for all subjects. 
#' To provide a more traditional experience in R,
#' the data frame is separated by columns into fields, e.g. `id` or `time`. This 
#' allows you to access them in an S3 way, e.g. `run1$pop$time` if `run1` is a 
#' `PM_result` object. 
#' 
#' However, if you wish to manipulate the entire data frame,
#' use the `data` field, e.g. `trough <- run1$pop$data %>% filter(time == 24)`. If
#' you are unfamiliar with the `%>%` pipe function, please type `help("%>%", "magrittr")`
#' into the R console and look online for instructions/tutorials in tidyverse, a
#' powerful approach to data manipulation upon which Pmetrics is built.
#' @author Michael Neely, Julian Otalvaro
#' @export
PM_pop <- R6::R6Class(
  "PM_pop",
  public <- list(
    #' @field data A data frame with the following columns
    #' * id Subject id
    #' * time Time of predictions in decimal hours
    #' * icen Prediction based on mean or median of Bayesian posterior parameter distribution
    #' * outeq Output equation number
    #' * pred Predicted output for each outeq
    #' * block Observation blocks within subjects as defined by *EVID=4* dosing events
    data = NULL,
    #' @description
    #' Create new object populated with population predicted data at 
    #' regular, frequent intervals
    #' @details
    #' Creation of new `PM_pop` object is automatic and not generally necessary
    #' for the user to do.
    #' @param pop The parsed output from [makePop].
    initialize = function(pop) {
      class(pop) <- append("tidy_pop",class(pop))
      self$data <- pop
      # self$id <- pop$id
      # self$time <- pop$time
      # self$icen <- pop$icen
      # self$outeq <- pop$outeq
      # self$pred <- pop$pred
      # self$block <- pop$block
    },
    #' @description
    #' Calculate AUC
    #' @details 
    #' See [makeAUC]
    #' @param ... Arguments passed to [makeAUC]
    auc = function(...) {
      makeAUC(data = self$data, ...)
    }
  )
)

#' Contains covariate data
#' 
#' Contains a data frame with subject-specific covariate data output
#' from [makeCov]
#' 
#' For each subject, [makeCov] extracts covariate information and 
#' Bayesian posterior parameter estimates.
#' This output of this function is suitable for exploration of covariate-
#' parameter, covariate-time, or parameter-time relationships.
#' @author Michael Neely, Julian Otalvaro
#' @export

PM_cov <- R6::R6Class(
  "PM_cov",
  public <- list(
    #' @field data A data frame with the following columns
    #' * id Subject identification
    #' * time Times of covariate observations
    #' * covnames... Columns with each covariate observations in the dataset for each subject and `time`
    #' * parnames... Columns with each parameter in the model and the `icen` summary
    #' for each subject, replicated as necessary for covariate observation times and duplicated for Bayesian 
    #' parameter means and medians 
    #' * icen The type of summarized Bayesian posterior individual parameter values: mean or median
    data = NULL,
    #' @description
    #' Create new object populated with covariate-parameter information 
    #' @details
    #' Creation of new `PM_cov` object is automatic and not generally necessary
    #' for the user to do.
    #' @param cov The parsed output from [makeCov].
    initialize = function(cov) {
      class(cov) <- append("tidy_cov",class(cov))
      self$data <- cov
    },
    #' @description
    #' Summary method
    #' @details
    #' See [summary.PMcov].
    #' @param ... Arguments passed to [summary.PMcov]
    summary = function(...) {
      summary.PMcov(self$data, ...)
    },
    #' @description
    #' Plot method
    #' @details
    #' See [plot.PMcov].
    #' @param ... Arguments passed to [plot.PMcov]
    plot = function(...) {
      plot.PMcov(self$data, ...) #update to (self,...) when plot.PM_cov done
    },
    #' @description
    #' Print method
    #' @details
    #' Print method for *PM_cov*
    #' @param ... Arguments passed to [print]
    print = function(...) {
      print(x = self$data, ...)
    }
  )
)

#' Wrapper function for summmary.PMcov
#' 
#' This redirects to summary.PMcov for PM_final R6 objects
#' 
#' See [summary.PMcov]. Alternative way to summarize is
#' `PM_result$cov$summary()`.
#' 
#' @param obj The *PM_covl* object to summarize
#' @param ... Arguments passed to [summary.PMcov]
#' @return A [summary.PMcov] object
#' @author Michael Neely, Julian Otalvaro
#' @export
summary.PM_cov <- function(obj, ...) {
  obj$summary(...)
}

#' Wrapper function for plot.PM_cov
#' 
#' This redirects to plot.PM_cov for tidy_cov objects
#' 
#' See [plot.PMcov]. Alternative way to plot is
#' `PM_result$cov$plot()`.
#' 
#' @param x A *PMcov* object with the *tidy_cov* class to be plotted
#' @param ... Arguments passed to [plot.PMcov]
#' @return A [plot.PMcov] object
#' @export
plot.tidy_cov <- function(x,...){
  plot.PM_cov(x,...)
}