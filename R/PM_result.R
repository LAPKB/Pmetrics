#' @title Results of a Pmetrics run
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This object contains all of the results after a Pmetrics runs. It is created
#' by using the [PM_load] function.
#'
#' @details After a run completes, results are stored on your hard drive. They are loaded
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
    #' @param out The parsed output from [PM_load], which is
    #' automatically generated. This is not a user-modifiable.
    #' @param quiet Quietly validate. Default is `FALSE`.
    initialize = function(out, quiet = TRUE) {
      if (!is.null(out$NPdata)) {
        self$NPdata <- out$NPdata
        class(self$NPdata) <- c("NPAG", "list")
      } else {
        self$NPdata <- NULL
      }
      if (!is.null(out$ITdata)) {
        self$ITdata <- out$ITdata
        class(self$ITdata) <- c("IT2B", "list")
      } else {
        self$ITdata <- NULL
      }
      if(is.null(out$NPdata)) {
        self$NPdata <- out
        class(self$NPdata) <- c("NPAG", "rust", "list")
      }
      self$pop <- PM_pop$new(out$pop)
      self$post <- PM_post$new(out$post)
      self$final <- PM_final$new(out$final)
      self$cycle <- PM_cycle$new(out$cycle)
      self$op <- PM_op$new(out$op)
      self$cov <- PM_cov$new(out$cov)
      self$data <- PM_data$new(data = out$data, quiet = quiet) # no need to report
      self$model <- out$model
      self$errfile <- out$errfile
      self$success <- out$success
      if (!is.null(out$valid)) {
        self$valid <- out$valid
      } else {
        self$valid <- NULL
      }
      return(self)
    },

    #' @description
    #' Plot generic function based on type
    #' @param type Type of plot based on class of object
    #' @param ... Plot-specific arguments
    plot = function(type, ...) {
      if (is.null(type)) {
        stop("Please provide the type of plot.")
      } else {
        self[[type]]$plot(...)
      }
    },

    #' @description
    #' Summary generic function based on type
    #' @param type Type of summary based on class of object
    #' @param ... Summary-specific arguments
    summary = function(type, ...) {
      if (is.null(type)) {
        stop("please provide the type of summary you want to obtain")
      } else {
        self[[type]]$summary(...)
      }
    },

    #' @description
    #' AUC generic function based on type
    #' @param type Type of AUC based on class of object
    #' @param ... Summary-specific arguments
    auc = function(type, ...) {
      if (!type %in% c("op", "pop", "post", "sim")) {
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
      # store copy of the final object
      bk_final <- self$final$clone()
      sim <- PM_sim$new(poppar = self, ...)
      self$final <- bk_final
      return(sim)
    },

    #' @description
    #' Save the current PM_result object to an .Rdata file.
    #' @details
    #' This is useful if you have updated the result in some way, for example you
    #' have run the `$make_valid` method on the `PM_result` object, which returns
    #' an internal simulation based validation as a new `valid` field. To save this
    #' validation, use this `$save` method. Note that unless a `file` name is provided,
    #' the changes will overwrite the
    #' previous run results, although unchanged items will be preserved. This is the
    #' usual workflow. However, a custom file name may be useful to share the run
    #' results with someone.
    #'
    #' The saved object is an .Rdata file. When loaded, it should be assigned to an R
    #' object, e.g. `run2 <- PM_result$new("filename")`. An equivalent statement would
    #' be `run2 <- PM_load(file = "filename")`.
    #' @param run The run output folder number to save the revised result. If missing,
    #' will save in the current working directory. For example, if folder "1" is in
    #' your current working directory, specify `run = 1` to save the result to the "outputs"
    #' subfolder of the "1" folder.
    #' @param file Custom file name. Default is "PMout.Rdata".
    save = function(run, file) {
      if (missing(run)) {
        outputfolder <- getwd()
      } else {
        if (is.na(suppressWarnings(as.numeric(run)))) {
          stop("The run argument is not numeric. Do you need to say 'file = '? See help for PM_result.")
        }
        outputfolder <- paste0(run, "/outputs")
        if (!file.exists(outputfolder)) {
          stop(paste0(outputfolder, " does not exist from the current working directory./n"))
        }
      }
      if (missing(file)) {
        if(is.null(self$NPdata$backend)){
          file <- "PMout.Rdata"
        } else {
          file <- "NPcore.Rdata"
        }
      } 
      PMout <- list(
        NPdata = self$NPdata,
        ITdata = self$ITdata,
        pop = self$pop$data, post = self$post$data,
        final = self$final$data, cycle = self$cycle$data,
        op = self$op$data, cov = self$cov$data, data = self$data$data,
        model = self$model, errfile = self$errfile,
        success = self$success,
        valid = self$valid
      )
      save(PMout, file = paste0(outputfolder, "/", file))
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
    #' `r lifecycle::badge("deprecated")`
    #'
    #' Deprecated method to load prior results saved with the `$save` method.
    #' Replaced by [PM_load].
    #' @param ... Not used.
    #' @keywords internal
    load = function(...) {
      lifecycle::deprecate_warn("2.1.0", "PM_result$load()", details = "PM_result$load() is deprecated. Please use PM_load() instead.")
    }
  ) # end public
) # end PM_result

#' @keywords internal
#' @name PM_result
#' @export
PM_result$load <- function(...) {
  lifecycle::deprecate_warn("2.1.0", "PM_result$load()", details = "Please use PM_load() instead. ?PM_load for details.")
}

#' @title Observed vs. predicted data
#'
#' @description
#' `r lifecycle::badge("stable")`
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
    #' @field id subject identification
    id = NULL,
    #' @field time observation time in relative units, usually hours
    time = NULL,
    #' @field obs observation
    obs = NULL,
    #' @field pred prediction
    pred = NULL,
    #' @field pred.type Population predictions based on Bayesian prior parameter value distribution,
    #' or individual predictions based on Bayesian posterior parameter value distributions
    pred.type = NULL,
    #' @field icen Predictions based on mean or median of Bayesian `pred.type`parameter values
    icen = NULL,
    #' @field outeq output equation number
    outeq = NULL,
    #' @field block dosing block number for each subject, as defined by dose resets (evid=4).
    block = NULL,
    #' @field obsSD standard deviation of the observation based on the assay error polynomial
    obsSD = NULL,
    #' @field d prediction error, `pred` - `obs`
    d = NULL,
    #' @field ds squared prediction error
    ds = NULL,
    #' @field wd weighted prediction error, which is the prediction error divided by the \code{obsSD}
    wd = NULL,
    #' @field wds weighted squared prediction error
    wds = NULL,
    #' @field data A data frame combining all the above fields as its columns
    data = NULL,
    #' @description
    #' Create new object populated with observed vs. predicted data
    #' @details
    #' Creation of new `PM_op` object is automatic and not generally necessary
    #' for the user to do.
    #' @param op The parsed output from [makeOP].
    initialize = function(op) {
      self$data <- op
      self$id <- op$id
      self$time <- op$time
      self$obs <- op$obs
      self$pred <- op$pred
      self$pred.type <- op$pred.type
      self$icen <- op$icen
      self$outeq <- op$outeq
      self$block <- op$block
      self$obsSD <- op$obsSD
      self$d <- op$d
      self$ds <- op$ds
      self$wd <- op$wd
      self$wds <- op$wds
    },
    #' @description
    #' Plot method
    #' @details
    #' See [plot.PM_op].
    #' @param ... Arguments passed to [plot.PM_op]
    plot = function(...) {
      plot.PM_op(self$data, ...)
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
    #' @param data The object to use for AUC calculation
    #' @param ... Arguments passed to [makeAUC]
    auc = function(...) {
      makeAUC(data = self$data, ...)
    }
  )
)

#' @title Wrapper function for summmary.PMop
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This redirects to summary.PMop for PM_op R6 objects
#'
#' @details See [summary.PMop]. Alternative way to summarize is
#' `PM_result$op$summary()`.
#'
#' @param object The *PM_op* object to summarize
#' @param ... Arguments passed to [summary.PMop]
#' @return A [summary.PMop] object
#' @export
summary.PM_op <- function(object, ...) {
  object$summary(...)
}

#' @title Individual Bayesian posterior predictions at short intervals
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Contains the Bayesian posterior predictions at short intervals
#' specified as an argument to the $run method of [PM_fit]. Default is every 12 minutes.
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
    #' @field id Subject id
    id = NULL,
    #' @field time Time of predictions in decimal hours
    time = NULL,
    #' @field icen Prediction based on mean or median of Bayesian posterior parameter distribution
    icen = NULL,
    #' @field outeq Output equation number
    outeq = NULL,
    #' @field pred Predicted output for each outeq
    pred = NULL,
    #' @field block Observation blocks within subjects as defined by *EVID=4* dosing events
    block = NULL,
    #' @field data A data frame combining all the above fields as its columns
    data = NULL,
    #' @description
    #' Create new object populated with Bayesian posterior predicted data at
    #' regular, frequent intervals
    #' @details
    #' Creation of new `PM_post` object is automatic and not generally necessary
    #' for the user to do.
    #' @param post The parsed output from [makePost].
    initialize = function(post) {
      self$data <- post
      self$id <- post$id
      self$time <- post$time
      self$icen <- post$icen
      self$outeq <- post$outeq
      self$pred <- post$pred
      self$block <- post$block
    },
    #' @description
    #' Calculate AUC
    #' @details
    #' See [makeAUC]
    #' @param data The object to use for AUC calculation
    #' @param ... Arguments passed to [makeAUC]
    auc = function(...) {
      makeAUC(data = self$data, ...)
    }
  )
)

#' @title Final Cycle Population Values
#'
#' @description
#' `r lifecycle::badge("stable")`
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
      class(self) <- c(c("NPAG", "IT2B")[1 + as.numeric(is.null(self$popPoints))], class(self))
    },
    #' @description
    #' Summary method
    #' @details
    #' See [summary.PMfinal].
    #' @param ... Arguments passed to [summary.PMfinal]
    summary = function(...) {
      summary.PMfinal(self, ...)
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

#' @title Wrapper function for summmary.PMfinal
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This redirects to summary.PMfinal for PM_final R6 objects
#'
#' @details
#' See [summary.PMfinal]. Alternative way to summarize is
#' `PM_result$final$summary()`.
#'
#' @param object The *PM_final* object to summarize
#' @param ... Arguments passed to [summary.PMfinal]
#' @return A [summary.PMfinal] object
#' @export
summary.PM_final <- function(object, ...) {
  object$summary(...)
}


#' @title Pmetrics Run Cycle Information
#'
#' @description
#' `r lifecycle::badge("stable")`
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
    #' @field names Vector of names of the random parameters
    names = NULL,
    #' @field cynum Vector cycle numbers, which may start at numbers greater
    #' than 1 if a non-uniform prior was specified for the run (NPAG only)
    cycnum = NULL,
    #' @field ll Vector of -2*Log-likelihood at each cycle
    ll = NULL,
    #' @field gamlam A tibble of cycle number and gamma or lambda at each cycle for each output equation
    gamlam = NULL,
    #' @field mean A tibble of cycle number and the mean of each random parameter
    #' at each cycle, normalized to initial mean
    mean = NULL,
    #' @field median A tibble of cycle number and the median of each random
    #' parameter at each cycle,  normalized to initial median
    median = NULL,
    #' @field sd A tibble of cycle number and the standard deviation of each random parameter
    #' at each cycle,  normalized to initial standard deviation
    sd = NULL,
    #' @field aic A vector of Akaike Information Criterion at each cycle
    aic = NULL,
    #' @field bic A vector of Bayesian (Schwartz) Information Criterion at each cycle
    bic = NULL,
    #' @field data A data frame combining all the above fields as its columns
    data = NULL,
    #' @description
    #' Create new object populated with  cycle information
    #' @details
    #' Creation of new `PM_cycle` object is automatic and not generally necessary
    #' for the user to do.
    #' @param cycle The parsed output from [makeCycle].
    initialize = function(cycle) {
      self$data <- cycle
      self$names <- cycle$names
      self$cycnum <- cycle$cycnum
      self$ll <- cycle$ll
      self$gamlam <- cycle$gamlam
      self$mean <- cycle$mean
      self$sd <- cycle$sd
      self$median <- cycle$median
      self$aic <- cycle$aic
      self$bic <- cycle$bic
    },
    #' @description
    #' Plot method
    #' @details
    #' See [plot.PM_cycle].
    #' @param ... Arguments passed to [plot.PM_cycle]
    plot = function(...) {
      plot.PM_cycle(self, ...)
    }
  )
)

#' @title Population predictions at short intervals
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Contains the population predictions at short intervals
#' specified as an argument to the run method of [PM_fit]. Default is every 12 minutes.
#'
#' @details
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
    #' @field id Subject id
    id = NULL,
    #' @field time Time of predictions in decimal hours
    time = NULL,
    #' @field icen Prediction based on mean or median of Bayesian posterior parameter distribution
    icen = NULL,
    #' @field outeq Output equation number
    outeq = NULL,
    #' @field pred Predicted output for each outeq
    pred = NULL,
    #' @field block Observation blocks within subjects as defined by *EVID=4* dosing events
    block = NULL,
    #' @field data A data frame combining all the above fields as its columns
    data = NULL,
    #' @description
    #' Create new object populated with population predicted data at
    #' regular, frequent intervals
    #' @details
    #' Creation of new `PM_pop` object is automatic and not generally necessary
    #' for the user to do.
    #' @param pop The parsed output from [makePop].
    initialize = function(pop) {
      self$data <- pop
      self$id <- pop$id
      self$time <- pop$time
      self$icen <- pop$icen
      self$outeq <- pop$outeq
      self$pred <- pop$pred
      self$block <- pop$block
    },
    #' @description
    #' Calculate AUC
    #' @details
    #' See [makeAUC]
    #' @param data The object to use for AUC calculation
    #' @param ... Arguments passed to [makeAUC]
    auc = function(...) {
      makeAUC(data = self$data, ...)
    }
  )
)

#' @title Contains covariate data
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Contains a data frame with subject-specific covariate data output
#' from [makeCov]
#'
#' @details
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
      self$data <- cov
    },
    #' @description
    #' Stepwise linear regression of covariates and Bayesian posterior
    #' parameter values
    #' @details
    #' See [PMstep].
    #' @param ... Arguments passed to [PMstep]
    step = function(...) {
      PMstep(self$data, ...)
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
    #' See [plot.PM_cov].
    #' @param ... Arguments passed to [plot.PM_cov]
    plot = function(...) {
      plot.PM_cov(self$data, ...)
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

#' @title Wrapper function for summmary.PMcov
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This redirects to summary.PMcov for PM_final R6 objects
#'
#' @details
#' See [summary.PMcov]. Alternative way to summarize is
#' `PM_result$cov$summary()`.
#'
#' @param object The *PM_cov* object to summarize
#' @param ... Arguments passed to [summary.PMcov]
#' @return A [summary.PMcov] object
#' @author Michael Neely, Julian Otalvaro
#' @export
summary.PM_cov <- function(object, ...) {
  object$summary(...)
}
