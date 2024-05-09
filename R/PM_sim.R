#Use menu item Code -> Jump To... for rapid navigation
#Keyboard Option+Command+O (Mac) or Alt+O (Windows) to fold all


# R6 ----------------------------------------------------------------------


#' @title
#' Object to contain results of simulation
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This object is created after a successful run of the simulator.
#'
#' @details
#' There are two methods of creating a PM_sim object.
#' * **PM_result$sim()**
#' * **PM_sim$new()**
#'
#' These both call [SIMrun] to execute the simulation and [SIMparse] to process
#' the results and return the PM_sim objects. See help on both of these functions
#' for further details.
#'
#' @export
PM_sim <- R6::R6Class(
  "PM_sim",
  public = list(
    #' @field obs Observations for each output as a data frame with columns 
    #' *id*, *time*, *out*, *outeq*, or in the case of multiple regimens
    #' in the template data file, a list of such data frames
    obs = NULL,
    #' @field amt Amounts in each compartment as a data frame with columns 
    #' *id*, *time*, *out*, *comp*, or in the case of multiple regimens
    #' in the template data file, a list of such data frames
    amt = NULL,
    #' @field parValues Retained simulated parameter values after discarding
    #' any due to truncation limits, as a data frame with columns 
    #' *id* and parameters for each simulated subject, or in the case of multiple regimens
    #' in the template data file, a list of such data frames
    parValues = NULL,
    #' @field totalSets Number of all simulated parameter values needed to obtain the
    #' requested number of simulated sets within any limits
    totalSets = NULL,
    #' @field totalMeans Vector of means of all simulated parameter values, 
    #' or in the case of multiple regimens
    #' in the template data file, a list of such vectors
    totalMeans = NULL,
    #' @field totalCov Covariance matrix for all simulated parameter values, 
    #' or in the case of multiple regimens
    #' in the template data file, a list of such matrices
    totalCov = NULL,
    #' @field data For one simulation regimen in the template data, a list of class *PMsim* that 
    #' contains all the above elements. For multiple simulation regimens, a list of
    #' class *PM_simlist* that contains as many *PMsim* objects as regimens in the 
    #' template data file used for the simulation, i.e `data` will be a list of lists.
    data = NULL,

    #' @description
    #' This is a wrapper function that combines [SIMrun] and [SIMparse] in R6.
    #' It can be called directly
    #' or via the `$sim` method for [PM_result] objects.
    #' @param poppar One of four things:
    #' * Population prior parameters as a [PM_final] object found in
    #' `PM_result$final`. Normally these would be supplied by calling the
    #' `$sim` method for a [PM_result] object, e.g. `PmetricsData::NPex$sim(...)`.
    #' * A manually specified prior, see [SIMrun] for more details.
    #' * The name of a previously saved simulation via the `$save` method. The
    #' file will be loaded.
    #' * The results of running [SIMparse] on a prior simulation.
    #' @param ... Additional parameters to be passed to [SIMrun] and optionally,
    #' `combine = TRUE` as an argument will be passed to [SIMparse].
    #' @return A `PM_sim` object created by calling [SIMparse] at the completion of the
    #' simulation.
    initialize = function(poppar, ...) {
      dots <- list(...)
      combine <- if (exists("combine", where = dots)) {
        dots$combine
      } else {
        FALSE
      }
      clean <- if (exists("clean", where = dots)) {
        dots$clean
      } else {
        TRUE
      }
      outname <- if (exists("outname", where = dots)) {
        dots$outname
      } else {
        "simout"
      }
      if (inherits(poppar, c("PM_result", "PM_final", "PMfinal"))) {
        poppar <- poppar # SIMrun will handle any of these
      } else if (inherits(poppar, "PMsim")) { # from SIMparse as single
        private$populate(poppar, type = "sim")
        return(self) #end, we have loaded a prior sim
      } else if (inherits(poppar, "PM_simlist")) { # from SIMparse as list
        private$populate(poppar, type = "simlist")
        return(self) #end, we have loaded a prior sim
      } else if (inherits(poppar, "PM_sim")) { # from R6
        private$populate(poppar, type = "R6sim")
        return(self) #end, we have loaded a prior sim
      } else if (inherits(poppar, "list")) {
        poppar <- poppar # PMfinal and PMsim are lists, so needs to be after those for manual list
      } else { # try it as a filename
        if (file.exists(poppar)) {
          sim <- readRDS(poppar) #open saved object
          if(inherits(sim, "PMsim")) {
            private$populate(sim, type = "sim")
          } else if(inherits(sim, "PM_simlist")){
            private$populate(sim, type = "simlist")
          } else if(inherits(sim, "PM_sim")){
            private$populate(sim, type = "R6sim")
          } else {
            stop(paste0(poppar, " is not a Pmetrics simulation."))
          }
          return(self) #end, we have loaded a prior sim
        } else {
          stop(paste0(poppar, " does not exist in the current working directory.\n"))
        }
      }

      system("echo 347 > SEEDTO.MON") # TODO: look to fix the simulator without this
      SIMrun(poppar, ...)

      # TODO: read files and fix the missing E problem
      sim_files <- list.files() %>% .[grepl(paste0(outname, "*"), .)]
      if (length(sim_files) == 1) {
        parseRes <- SIMparse(file = paste0(outname, "*")) %>% private$populate(type = "sim")
      } else {
        if (combine) {
          parseRes <- SIMparse(file = paste0(outname, "*"), combine = T) %>% private$populate(type = "sim")
        } else {
          parseRes <- list()
          for (i in seq_len(length(sim_files))) {
            parseRes[[i]] <- SIMparse(file = sprintf("simout%i.txt", i))
          }
          private$populate(parseRes, type = "simlist")
        }
      }
      if (clean) system(paste0("rm ", outname, "*"))
      return(self)
    },
    #'
    #' @description
    #' Save the current PM_sim object into a .rds file.
    #' @param file_name Name of the file to be created, the default is PMsim.rds
    save = function(file_name = "PMsim.rds") {
      saveRDS(self, file_name)
    },
    #' @description
    #' Plot `PM_sim` object.
    #' @param at Index of the PM_sim object to be plotted. Default is 1.
    #' result.
    #' @param ... Arguments passed to [plot.PM_sim].
    plot = function(at = 1, ...) {
      if (inherits(self$data, "PM_simlist")) {
        if (at > length(self$data)) {
          stop(sprintf("Error: Index is out of bounds. index: %i , length(simlist): %i", at, length(self$data)))
        }
        plot.PM_sim(self$data[[at]], ...)
      } else {
        plot.PM_sim(self$data, ...)
      }
    },
    #' @description
    #' Estimates the Probability of Target Attaintment (PTA), based on the results
    #' of the current Simulation.
    #' @param ... Additional parameters, refer to [makePTA]
    pta = function(...) {
      PM_pta$new(self, ...)
    },
    #' @description
    #' Calculates the AUC of the specified simulation
    #' @param at Index of the PM_sim object to use. Default is 1.
    #' @param ... Arguments passed to [makeAUC].
    auc = function(at = 1, ...) {
      if (inherits(self$data, "PM_simlist")) {
        if (at > length(self$data)) {
          stop(sprintf("Error: Index is out of bounds. index: %i , length(simlist): %i", at, length(self$data)))
        }
        makeAUC(self$data[[at]], ...)
      } else {
        makeAUC(self$data, ...)
      }
    },
    #' @description
    #' Summarize simulation
    #' @param at Index of the PM_sim object to be summarized. Default is 1.
    #' @param field Quoted character value, one of
    #' * obs for simulated observations
    #' * amt for simulated amounts in each compartment
    #' * parValues for simulated parameter values
    #' @param by Optional quoted column name(s) to group by, e.g. `by = "id"` or
    #' `by = c("id", "outeq")`.
    #' @param individual If "id" is included within `by`, the summary statistics
    #' will be calculated for each individual, i.e. each individual's mean, sd, median,
    #' min and max observations. If `individual` is `FALSE` (the default), then those
    #' summaries will again be summarized to present, for example, the mean of
    #' all subjects' mean observations, or the mean of their maximum observations.
    #' This argument will be ignored if "id" is not in `by`.
    #' @return If `by` is ommitted, a data frame with rows for each data element except ID,
    #' and columns labeled as mean, sd, median, min and max. If `by` is specified,
    #' return will be a list with named elements mean, sd, median, min and max, each containing
    #' the corresponding value for each group in `by`.
    summary = function(at = 1, field, by, individual = FALSE) {
      summaries <- c("mean", "sd", "median", "min", "max")
      # get the right data
      if (inherits(self$data, "PM_simlist")) {
        if (at > length(self$data)) {
          stop(sprintf("Error: Index is out of bounds. index: %i , length(simlist): %i", at, length(self$data)))
        }
        dat <- self$data[[at]][[field]]
      } else {
        dat <- self$data[[field]]
      }

      if (!missing(by)) {
        summ <- purrr::map(
          summaries,
          \(x) dplyr::summarize(dat,
            dplyr::across(everything(), !!!dplyr::syms(x)),
            .by = by
          )
        )
        names(summ) <- summaries
        if ("id" %in% by) {
          if (!individual) {
            # group by individual, then summarize
            by <- by %>% purrr::discard(\(x) x == "id") # remove id
            if (length(by) == 0) {
              by <- NULL
            } # set to NULL if character(0)
            summ <- purrr::map(summ, \(x){
              purrr::map_df(summaries, \(y) {
                dplyr::summarize(x,
                  dplyr::across(everything(), !!!dplyr::syms(y)),
                  .by = by
                ) %>%
                  select(-id)
              }) %>%
                t() %>%
                as.data.frame() %>%
                rename_with(~summaries)
            })
          }
        } else {
          # id is not in grouping, so just remove id column
          summ <- purrr::map(summ, \(x) x %>% select(-id))
        }
      } else { # no grouping
        summ <- purrr::map_df(summaries, \(x) summarize(dat, across(everything(), !!!syms(x))))
        if ("id" %in% names(summ)) {
          summ <- summ %>% select(-id)
        }
        summ <- summ %>%
          t() %>%
          as.data.frame()
        names(summ) <- summaries
      }

      return(summ)
    },
    #' @description
    #' `r lifecycle::badge("deprecated")`
    #'
    #' Deprecated method to run a simulation. Replaced by `PM_sim$new()` to be
    #' consistent with R6.
    #' @param ... Not used.
    #' @keywords internal
    run = function(...) {
      lifecycle::deprecate_warn("2.1.0", "PM_sim$run()", details = "PM_sim$run() is deprecated. Please use PM_sim$new() instead.")
    },
    #' @description
    #' `r lifecycle::badge("deprecated")`
    #'
    #' Deprecated method to load a prior simulation. Replaced by `PM_sim$new()` to be
    #' consistent with R6.
    #' @param ... Not used.
    #' @keywords internal
    load = function(...) {
      lifecycle::deprecate_warn("2.1.0", "PM_sim$load()", details = "PM_sim$load() is deprecated. Please use PM_sim$new() instead.")
    }
  ), # end public
  private = list(
    # Create new simulation objects with results of simulation
    populate = function(simout, type) {
      if (type == "sim") {
        self$obs <- simout$obs
        self$amt <- simout$amt
        self$parValues <- simout$parValues
        self$totalMeans <- simout$totalMeans
        self$totalCov <- simout$totalCov
        self$data <- simout
        class(self$data) <- c("PMsim", "list")
      } else if (type == "simlist") {
        purrr::map(1:length(simout), \(x){
          self$obs[[x]] <- simout[[x]]$obs
          self$amt[[x]] <- simout[[x]]$amt
          self$parValues[[x]] <- simout[[x]]$parValues
          self$totalMeans[[x]] <- simout[[x]]$totalMeans
          self$totalCov[[x]] <- simout[[x]]$totalCov
          self$data[[x]] <- simout[[x]]
          class(self$data[[x]]) <- c("PMsim", "list")
        })
        class(self$data) <- c("PM_simlist", "list")
      } else if (type == "R6sim"){
        self$obs <- simout$obs
        self$amt <- simout$amt
        self$parValues <- simout$parValues
        self$totalMeans <- simout$totalMeans
        self$totalCov <- simout$totalCov
        self$data <- simout$data
      }
      return(self)
    }
  ) # end private
)


#' @keywords internal
#' @name PM_sim
#' @export
PM_sim$run <- function(...) {
  lifecycle::deprecate_warn("2.1.0", "PM_sim$run()", details = "Please use PM_sim$new() instead. ?PM_sim for details.")
}

#' @keywords internal
#' @name PM_sim
#' @export
PM_sim$load <- function(...) {
  lifecycle::deprecate_warn("2.1.0", "PM_sim$load()", details = "Please use PM_sim$new() instead. ?PM_sim for details.")
}

# MAKE (SIMrun) -----------------------------------------------------------
#' @title Run the Pmetrics Simulator
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' This is largely superseded as it is automatically called by either of the simulation
#' methods: `PM_result$sim()` or `PM_sim$new()`. There is rarely a need any longer to call this
#' function directly.
#'
#' @details
#'
#' The Monte Carlo simulator in Pmetrics generates randomly sampled sets of
#' parameters from the *#PRIMARY* block of
#' a model according to a prior distribution and calculates outputs based upon
#' a template data file. It is a powerful tool for parametric or
#' semi-parametric sampling.  There are three ways to execute the simulator.
#'
#' * **PM_result$sim**
#' * **PM_sim$run**
#' * **SIMrun**
#'
#' The first two are the preferred methods in R6 Pmetrics. They return fully
#' parsed simulator output as [PM_sim] objects in R. The third method is for
#' legacy Pmetrics or when a user-specified prior is necessary, i.e. not from
#' a prior NPAG or IT2B run. In this case, the output of the simulation is one
#' or more files on your hard drive, which must be read into R using  [SIMparse].
#' [SIMparse] is not necessary with the first two methods, as R includes a call
#' to that function at the end of the simulation.
#'
#' Via the first two methods of calling SIMrun, NPAG or IT2B final objects
#' can easily be used as
#' the prior distributions for sampling. Via the third method, prior distributions
#' may be manually
#' specified.  Prior distributions may be unimodal-multivariate (parametric
#' sampling), or multimodal-multivariate (semi-parametric sampling). For priors
#' from NPAG, this can be accomplished with the `split` argument.
#'
#' It is also possible to simulate with covariates if they are included as part
#' of the model. By specifying a covariate list argument, Pmetrics will first
#' calculate the correlation matrix between the covariates and the Bayesian
#' posterior parameter values for each subject in the population model.  Using
#' either the mean and standard deviation of each covariate in the population,
#' or a user-specified mean and/or standard deviation, Pmetrics will then
#' calculate an augmented covariance matrix to be used in simulations.  Pmetrics
#' will make a copy of the model file with all covariates moved into the primary
#' block as parameters to be simulated.
#'
#' Noise can be applied to the simulated observations. Noise may also be applied
#' to the observation times, to the dose times, or to the dose amounts.
#'
#' Limits on the simulated parameter sets can also be specified using the limits
#' on primary parameters in the model file or by specifying them manually as an
#' argument. Limits can also be applied to simulated covariates.
#'
#' It is permissible to fix a parameter for simulation that was a random
#' parameter in the model prior by changing the range in the model file to a
#' single value for that parameter.
#'
#' The same model and data structures are used for the simulator as for any
#' other Pmetrics functions.  In this case, the data object will serve as the
#' template for the information regarding dosing, covariate values, and
#' observations.  Template data may have more than one subject in them, in
#' which case the simulator will use each subject specified by the
#' `include` argument (default is all subjects) to generate `nsim`
#' parameter sets and corresponding observations.
#'
#' Simulator output is directed to text files, one for each template subject,
#' which is read back into R by [SIMparse].  As mentioned above, this occurs
#' automatically for the first two methods of calling SIMrun. Output may also be
#' directed to a new Pmetrics .csv data file using the `makecsv` argument.
#'
#' @param poppar Either an object of class *PM_final* (see
#' [PM_result]) or a list containing three items in this order,
#' but of any name: vector of weights, vector of mean parameter values, and a
#' covariance matrix. If only one distribution is to be specified the
#' `weights` vector should be of length 1 and contain a 1. If multiple
#' distributions are to be sampled, the `weights` vector should be of
#' length equal to the number of distributions and its values should sum to 1,
#' e.g. `c(0.25,0.05,0.7)`.  The means matrix may be a vector for a
#' single distribution, or a matrix with `length(weights)` rows and
#' number of columns equal to the number of parameters, *npar*. The
#' covariance matrix will be divided by `length(weights)` and applied to
#' each distribution.
#'
#' @param limits If limits are specified, each simulated parameter set that
#' contains a value outside of the limits will be ignored and another set will
#' be generated.  Four options exist for limits.  1) The default `NULL`
#' indicates that no limits are to be applied to simulated parameters. 2) The
#' second option is to set `limits` to `NA`. This will use the
#' parameter limits on the primary parameters that are specified in the model
#' file. 3) The third option is a numeric vector of length 1 or 2, e.g. `limits = 3` or
#' `limits = c(0.5, 4)`, which specifies what to multiply the columns of the limits in the
#' model file.  If length 1, then the lower limits will be the same as in the
#' model file, and the upper limits will be multiplied by value specified.  If
#' length 2, then the lower and upper limits will be multiplied by the
#' specified values.  If this option is used, `popppar` must be a
#' `PM_final` object. 4) The fourth option for limits is a fully
#' customized matrix of limits for simulated values for each parameter which
#' will overwrite any limits in the model file.  If specified, it should be a
#' data.frame or matrix with number of rows equal to the number of random
#' paramters and 2 columns, corresponding to the minimum and maximum values.
#' For example, a final$ab object, or a directly coded matrix, e.g.
#' `matrix(c(0, 5, 0, 5, 0.01, 100), nrow = 3,ncol = 2, byrow = T)` for 3 parameters with
#' limits of (0, 5), (0, 5) and (0.01, 100), respectively.  It is possible to
#' convert a parameter to fixed by omitting the second limit. Means and
#' covariances of the total number of simulated sets will be returned to
#' verify the simulation, but only those sets within the specified limits will
#' be used to generate output(s) and the means and covariances of the retained
#' sets may (and likely will be) different than those specified by
#' `poppar`.
#'
#' @param model Name of a suitable [PM_model] object or a model file template
#' in the working directory.
#' The default is "model.txt".
#'
#' @param data Either a [PM_data] object (R6 Pmetrics), a `PMmatrix` object
#' previously loaded with
#' [PMreadMatrix] (legacy Pmetrics) or a character vector with the file name of a
#' Pmetrics data file in the working directory that contains template regimens
#' and observation times.
#' The value for outputs can be coded as any number(s) other than -99.  The
#' number(s) will be replaced in the simulator output with the simulated
#' values. Outputs equal to -99 will be simulated as missing.
#'
#' @param split Boolean operator controlling whether to split an NPAG
#' [PM_final] object into one distribution per support point, with means
#' equal to the vector of parameter values for that point, and covariance
#' equal to the population covariance divided by the number of support points.
#' Default for NPAG [PM_final] objects is `TRUE`, otherwise
#' `FALSE`.
#'
#' @param include A vector of subject IDs in the `matrixfile1 to iterate
#' through, with each subject serving as the source of an independent
#' simulation.  Default is `NA` and all subjects in the data file will be used.
#'
#' @param exclude A vector of subject IDs to exclude in the simulation, e.g.
#' `exclude = c(4, 6:14, 16:20)`. Default is `NA` and
#' all subjects in the data file will be used, i.e. none excluded.
#'
#' @param nsim The number of simulated profiles to create, per subject.  Default
#' is 1000.  Entering 0 will result in one profile being simulated from each
#' point in the non-parametric prior (for NPAG final objects only).
#'
#' @param predInt The interval in fractional hours for simulated predicted
#' outputs at times other than those specified in the template `data`.
#' The default is 0, which means there will be simulated outputs only at times
#' specified in the data file (see below).  Values greater than 0 result in
#' simulated outputs at the specified value, e.g. every 15 minutes
#' for `predInt = 0.25` from time 0 up to the maximal time in the template file,
#' per subject if nsub > 1.  You may also specify `predInt` as a vector
#' of 3 values, e.g. `predInt = c(1, 4, 1)`, similar to the R command
#' [seq], where the first value is the start time, the second is
#' the stop time, and the third is the step value.  Finally, you can have
#' multiple such intervals by specifying `predInt` as a list of such
#' vectors, e.g. `predInt = list(c(0, 24, 1), c(72, 96, 1))`.  Outputs for times
#' specified in the template file will also be simulated. To simulate outputs
#' *only* at the output times in the template data (i.e. EVID=0 events),
#' use `predInt = 0`, which is the default. Note that the maximum number of
#' predictions is 594, so the prediction interval must be sufficiently long to
#' accommodate this for a given number of output equations and total time to
#' simulate over.  If `predInt` is set so that this cap is exceeded,
#' predictions will be truncated.
#'
#' @param covariate If you are using the results of an NPAG or IT2B run to
#' simulate, i.e. a [PM_final] object as `poppar`, then you can also
#' simulate with covariates. This argument is a list with the following names.
#' * `cov` The name of a [PM_result] or [PM_cov] object
#' Pmetrics will use this covariate object to calculate the correlation
#' matrix between all covariates and Bayesian posterior parameter values.
#' * `mean` A named list that allows you to specify a different mean
#' for one or more of the covariates. Each named item in the list is the name
#' of a covariate in your data that is to have a different mean. If this
#' argument is missing then the mean covariate values in the population will
#' be used for simulation. The same applies to any covariates that are not
#' named in this list.  Example:
#' `run1 <- PM_load(1); covariate = list(cov = run1, mean = list(wt = 50))`.
#' * `sd` This functions just as the mean list argument does - allowing you to
#' specify different standard deviations for covariates in the simulation. If
#' it, or any covariate in the sd list is missing, then the standard
#' deviations of the covariates in the population are used. Example:
#' `covariate = list(cov = run1$cov, sd = list(wt = 10))`.
#' * `limits` This is a bit different than the limits for population
#' parameters above. Here,
#' limits is similar to mean and sd for covariates in
#' that it is a named list with the minimum and maximum allowable simulated
#' values for each covariate.  If it is missing altogether, then no limits
#' will apply.  If it is specified, then named covariates will have the
#' indicated limits, and covariates not in the list will have limits that are
#' the same as in the original population.  If you want some to be limited and
#' some to be unlimited, then specify the unlimited ones as items in this list
#' with very large ranges.  Example:
#' `covariate = list(cov = run1, limits = list( wt = c(10, 70)))`.
#' * `fix` A character vector (not a list) of covariates to fix and not simulate.  In
#' this case values in the template data file will be used and not simulated.
#' Example: `fix = c("wt", "age")`.  Whether you use the means and
#' standard deviations in the population or specify your own, the covariance
#' matrix in `poppar` will be augmented by the covariate covariances for any
#' non-fixed covariates. The parameter plus covariate means and this augmented
#' covariance matrix will be used for simulations. In effect, all non-fixed
#' covariates are moved into the _#Primary_ block of the model file to become
#' parameters that are simulated. In fact, a copy of your model file is made
#' with a "c" prepended to the model name (e.g. "model.txt" ->
#' "c_model.txt").
#'
#' @param usePost Boolean argument.  Only applicable when `poppar` is an
#' NPAG [PM_final] object. If so, and `usePost` is `TRUE`, the
#' posterior for each subject (modified by `include` or `exclude`)
#' in `poppar` will be used to simulate rather than the population prior.
#' The number of subjects in the template `data` file must be the same.
#' Normally one uses the same data file as used to make the model final
#' parameter distribution in `poppar`, but if different templates are
#' desired, the number must be equivalent to the number of included subjects
#' from whom the posteriors are obtained.
#'
#' @param seed The seed for the random number generator.  For `nsub` > 1,
#' should be a vector of length equal to `nsub`. Shorter vectors will be
#' recycled as necessary.  Default is -17.
#'
#' @param ode Ordinary Differential Equation solver log tolerance or stiffness.
#' Default is -4, i.e. 0.0001.  Higher values will result in faster runs, but
#' simulated concentrations may not be as accurate.
#'
#' @param obsNoise The noise added to each simulated concentration for each
#' output equation, where the noise is randomly drawn from a normal
#' distribution with mean 0 and SD = C0 + C1\*conc + C2\*conc^2 + C3\*conc^3.
#' Default values are 0 for all coefficients (i.e.) no noise. If present will
#' override any other values in the data file or model file. Specify as a
#' vector of length 4 times the number of output equations, e.g.
#' c(0.1,0.1,0,0) for one output and c(0.1,0.1,0,0,0.01,0.2,-0.001,0) for two
#' output equations. If specified as `NA`, values in the data file will
#' be used (similar to `limits`, above).  If they are missing, values in
#' the model file will be used.
#'
#' @param doseTimeNoise A vector of length four to specify dose time error
#' polynomial coefficients.  The default is 0 for all coefficients.
#'
#' @param doseNoise A vector of length four to specify dose amount error
#' polynomial coefficients.  The default is 0 for all coefficients.
#'
#' @param obsTimeNoise A vector of length four to specify observation timing
#' error polynomial coefficients.  The default is 0 for all coefficients.
#'
#' @param makecsv A character vector for the name of the single .csv file to be
#' made for all simulated "subjects".  If missing, no files will be
#' made. If a `makecsv` filename is supplied, ID numbers will
#' be of the form nsub.nsim, e.g. 1.001 through 1.1 for the first subject,
#' 2.001 through 2.1 for the second subject, etc. if 1000 simulations are made
#' from each subject.
#'
#' @param outname The name for the output file(s) without an extension.  Numbers
#' 1 to `nsub` will be appended to the files. If missing, will default to
#' "simout".
#'
#' @param clean Boolean parameter to specify whether temporary files made in the
#' course of the simulation run should be deleted. Defaults to `TRUE`.
#' This is primarily used for debugging.
#'
#' @param quiet Boolean operator controlling whether a model summary report is
#' given.  Default is `FALSE`.
#'
#' @param nocheck Suppress the automatic checking of the data file with
#' [PMcheck].  Default is `FALSE`.
#'
#' @param overwrite Cleans up any old output files without asking before
#' creating new output. Default is `FALSE`.
#'
#' @param combine Pass through to [SIMparse]. Ignored for SIMrun.
#'
#' @return No value is returned, but simulated file(s) will be in the working
#' directory.
#' @author Michael Neely
#' @seealso [SIMparse]
#' @examples
#' \dontrun{
#' # Load results of NPAG run
#' run1 <- PM_load(1)
#'
#' # Two methods to simulate
#' # The first uses the population prior, data, and model in run1, with "..."
#' # as additional parameters passed to SIMrun, e.g. limits, nsim, etc.
#'
#' sim1 <- run1$sim(...)
#'
#' # The second uses the population prior and model in run1, and a new template
#' # data file in the working directory
#'
#' sim2 <- PM_sim$new(poppar = run1, data = newfile.csv, ...)
#'
#' # These methods are entirely interchangeable. The first can accept a different
#' # data template. The difference is that poppar must be explicitly
#' # declared when using PM_sim$new. This makes it the method to use when poppar 
#' # is derived from th literature.
#'
#' # An example of a manual prior
#' # make 1 lognormal distribution for each parameter
#' weights <- 1
#' mean <- log(c(0.7, 0.05, 100))
#' cov <- matrix(rep(0, length(mean)**2), ncol = length(mean))
#' diag(cov) <- (c(0.15, 0.15, 0.15) * mean)**2
#' # make the prior for the simulation
#' poppar <- list(weights, mean, cov)
#' 
#' # run simulation, assuming temp1.csv and model.txt are in working directory
#' 
#' sim1 <- PM_sim$new(poppar, "temp1.csv",
#'   nsim = 15, model = "model.txt", include = 1:4, limits = NA,
#'   obsNoise = c(0.02, 0.1, 0, 0)
#' )
#' }
#' @export

SIMrun <- function(poppar, limits = NULL, model, data, split,
                   include = NA, exclude = NA, nsim = 1000, predInt = 0, covariate, usePost = F,
                   seed = -17, ode = -4,
                   obsNoise, doseTimeNoise = rep(0, 4), doseNoise = rep(0, 4), obsTimeNoise = rep(0, 4),
                   makecsv, outname, clean = T, quiet = F, nocheck = F, overwrite = F, combine) {
  if (inherits(poppar, "PM_result")) {
    is_res <- TRUE
    res <- poppar$clone()
    poppar <- poppar$final$clone()
    # number of random parameters
    npar <- nrow(poppar$popCov)
  } else if (inherits(poppar, "PM_final")) {
    poppar <- poppar$clone()
    npar <- nrow(poppar$popCov)
    is_res <- FALSE
  } else {
    npar <- nrow(poppar[[3]])
    is_res <- FALSE
  }
  
  
  if (missing(model)) {
    if (is_res) {
      model <- "simmodel.txt"
      res$model$write(model)
      model_file_src <- F # did not use file as source
    } else {
      model <- FileExists("model.txt") # default name if model is missing
      mod_obj <- PM_model$new(model) # we have valid filename, create new PM_model
      mod_obj$write("simmodel.txt")
      model_file_src <- T # used a file as source
    }
  } else { # model is  specified
    if (inherits(model, "PM_model")) {
      model$write("simmodel.txt") # write the PM_model to "simmodel.txt" file
      model_file_src <- F # did not use a file as source
    } else { # model is a filename
      # make sure data file name is <=8 characters
      if (!FileNameOK(model)) { # function in PMutilities
        endNicely(paste("Data file name must be 8 characters or fewer.\n"), model)
      }
      # make sure it exists
      model <- FileExists(model)
      mod_obj <- PM_model$new(model) # we have valid filename, create new PM_data
      mod_obj$write("simmodel.txt")
      model_file_src <- T # used a file as source
    }
  }
  
  
  if (missing(data)) { # data is not specified as an argument
    if (is_res) {
      data <- "simdata.csv"
      res$data$write(data) # create a file named simdata.csv from PM_result$data
      data_file_src <- F # did not use a file as source
    } else {
      data <- FileExists("data.csv") # try default
      data_obj <- PM_data$new(data) # we have valid filename, create new PM_data
      data_obj$write("simdata.csv")
      data_file_src <- T # used a file as source
    }
  } else { # data is  specified
    if (inherits(data, "PM_data")) {
      data$write("simdata.csv") # write the PM_data to "simdata.csv" file
      data_file_src <- F # did not use a file as source
    } else {
      if (inherits(data, "PMmatrix")) { # old format
        data_obj <- PM_data$new(data) # create new PM_data
        data_obj$write("simdata.csv")
        data_file_src <- F # did not use a file as source
      } else {
        # data is a filename
        # make sure data file name is <=8 characters
        if (!FileNameOK(data)) { # function in PMutilities
          endNicely(paste("Data file name must be 8 characters or fewer.\n"), model)
        }
        # make sure it exists
        data <- FileExists(data)
        data_obj <- PM_data$new(data) # we have valid filename, create new PM_data
        data_obj$write("simdata.csv")
        data_file_src <- T # used a file as source
      }
    }
  }
  
  model <- "simmodel.txt" # working name for model file
  data <- "simdata.csv" # working name for data file
  dataFile <- PM_data$new("simdata.csv")$standard_data # the data
  
  
  # set default split values when split is missing
  if (missing(split)) {
    if (inherits(poppar, "NPAG")) {
      split <- T
    } else {
      split <- F
    }
  }
  
  
  
  # deal with limits on parameter simulated values
  if (all(is.null(limits))) {
    # limits are omitted altogether
    parLimits <- matrix(rep(NA, 2 * npar), ncol = 2)
    omitParLimits <- T
  } else {
    if (!is.na(limits[1]) & is.vector(limits)) {
      # limits not NA and specified as vector of length 1 or 2
      # so first check to make sure poppar is a PMfinal object
      if (!inherits(poppar, "PM_final")) endNicely("\npoppar must be a PM_final object when multiplicative limits specified.\n", modeltxt, data)
      orig.lim <- poppar$ab
      if (length(limits) == 1) limits <- c(1, limits)
      final.lim <- t(apply(orig.lim, 1, function(x) x * limits))
      parLimits <- final.lim
    } else {
      if (is.na(limits[1])) {
        # limits specified as NA (use limits in model file)
        parLimits <- matrix(rep(NA, 2 * npar), ncol = 2)
      } else {
        parLimits <- limits
      }
      # limits specified as a matrix
    }
    omitParLimits <- F
  }
  
  # check if simulating with the posteriors and if so, get all subject IDs
  if (usePost) {
    if (length(poppar$postPoints) == 0) endNicely("\nPlease remake your final object with makeFinal() and save with PMsave().\n", model, data)
    postToUse <- unique(poppar$postPoints$id)
    if (split) {
      split <- F
      cat("\nsplit set to FALSE for simulations from posteriors.\n")
      flush.console()
    }
  } else {
    postToUse <- NULL
  }
  
  # if covariate is not missing, augment prior with covariate and modify model file
  if (!missing(covariate)) {
    if (length(postToUse) > 0) endNicely("\nYou cannot simulate from posteriors while simulating covariates.\n", model, data)
    simWithCov <- T
    # check to make sure poppar is PMfinal object
    if (!inherits(poppar, "PM_final")) endNicely("\npoppar must be a PM_final object if covariate simulations are used.\n", model, data)
    
    # check to make sure covariate arugment is list (PMcov, mean, sd, limits,fix)
    if (!inherits(covariate, "list")) endNicely("\nThe covariate argument must be a list; see ?SIMrun for help.\n", model, data)
    # check to make sure names are correct
    covArgNames <- names(covariate)
    badNames <- which(!covArgNames %in% c("cov", "mean", "sd", "limits", "fix"))
    if (length(badNames) > 0) endNicely("\nThe covariate argument must be a named list; see ?SIMrun for help.\n", model, data)
    
    # check to make sure first element is PMcov
    if (inherits(covariate$cov, "PM_result")) {
      covariate$cov <- covariate$cov$cov$data
    }
    if (inherits(covariate$cov, "PM_cov")) {
      covariate$cov <- covariate$cov$data
    }
    if (!inherits(covariate$cov, "PMcov")) {
      endNicely("\nThe cov element of covariate must be a PMcov object; see ?SIMrun for help.\n", model, data)
    }
    # get mean of each covariate and Bayesian posterior parameter
    CVsum <- summary(covariate$cov, "mean")
    # take out fixed covariates not to be simulated
    if (length(covariate$fix) > 0) {
      fixedCov <- which(names(CVsum) %in% covariate$fix)
      if (length(fixedCov) > 0) {
        CVsum <- CVsum[, -fixedCov]
      }
    }
    # get correlation matrix
    corCV <- suppressWarnings(cor(CVsum[, -c(1, 2)]))
    # remove those that are missing because they have all the same value
    corCVmiss <- which(is.na(corCV[, 1]))
    if (length(corCVmiss) > 0) {
      corCV <- corCV[-corCVmiss, -corCVmiss]
    }
    nsimcov <- ncol(corCV) - npar
    # augment poppar correlation matrix
    corMat <- poppar$popCor
    if (nsimcov == 1) {
      corCVsub <- as.matrix(corCV[(nsimcov + 1):(npar + nsimcov), (1:nsimcov)], ncol = 1)
      dimnames(corCVsub)[[2]] <- dimnames(corCV)[[1]][1] # replace dropped name
      corMat <- cbind(corMat, corCVsub)
      corMat2 <- as.matrix(c(corCV[(1:nsimcov), (nsimcov + 1):(npar + nsimcov)], corCV[(1:nsimcov), (1:nsimcov)]), ncol = 1)
      dimnames(corMat2)[[2]] <- dimnames(corCV)[[1]][1] # replace dropped name
      dimnames(corMat2)[[1]] <- dimnames(corMat)[[2]] # temp fix for Katharine's issue
      corMat <- rbind(corMat, t(corMat2))
    } else {
      corCVsub <- corCV[(nsimcov + 1):(npar + nsimcov), (1:nsimcov)]
      corMat <- cbind(corMat, corCVsub)
      corMat2 <- cbind(corCV[(1:nsimcov), (nsimcov + 1):(npar + nsimcov)], corCV[(1:nsimcov), (1:nsimcov)])
      # dimnames(corMat2)[[1]] <- dimnames(corMat)[[2]] #temp fix for Katharine's issue
      corMat <- rbind(corMat, corMat2)
    }
    
    # get SD of covariates (removing ID and time)
    covSD <- apply(CVsum[, -c(1, 2)], 2, sd, na.rm = T)
    # remove those with missing correlation
    if (length(corCVmiss) > 0) {
      covSD <- covSD[-corCVmiss]
    }
    # set SDs of named variables, and use population values for others
    if (length(covariate$sd) > 0) {
      badNames <- which(!names(covariate$sd) %in% names(covSD))
      if (length(badNames) > 0) {
        endNicely("\nThe sd element of covariate must be a list with parameter names; see ?SIMrun for help.\n", model, data)
      }
      covSD[which(names(covSD) %in% names(covariate$sd))] <- covariate$sd
      covSD <- unlist(covSD)
    }
    # multiply augmented correlation matrix by pairwise SD to get covariance
    covMat <- corMat
    sdVector <- unlist(c(poppar$popSD, covSD[1:nsimcov]))
    for (i in 1:nrow(covMat)) {
      for (j in 1:ncol(covMat)) {
        covMat[i, j] <- sdVector[i] * sdVector[j] * corMat[i, j]
      }
    }
    # get means of covariates
    covMean <- apply(CVsum[, -c(1, 2)], 2, mean, na.rm = T)
    # remove those with missing correlation
    if (length(corCVmiss) > 0) {
      covMean <- covMean[-corCVmiss]
    }
    # set means of named variables, and use population values for others
    if (length(covariate$mean) > 0) {
      badNames <- which(!names(covariate$mean) %in% names(covMean))
      if (length(badNames) > 0) {
        endNicely("\nThe mean element of covariate must be a list with parameter names; see ?SIMrun for help.\n", model, data)
      }
      covMean[which(names(covMean) %in% names(covariate$mean))] <- covariate$mean
      covMean <- unlist(covMean)
    }
    
    
    meanVector <- c(poppar$popMean, covMean[1:nsimcov])
    # get the covariate limits
    # get min of original population covariates
    covMin <- apply(CVsum[, -c(1, 2)], 2, min, na.rm = T)
    # remove those with missing correlation
    if (length(corCVmiss) > 0) {
      covMin <- covMin[-corCVmiss]
    }
    # and get max of original population covariates
    covMax <- apply(CVsum[, -c(1, 2)], 2, max, na.rm = T)
    # remove those with missing correlation
    if (length(corCVmiss) > 0) {
      covMax <- covMax[-corCVmiss]
    }
    orig.covlim <- cbind(covMin[1:nsimcov], covMax[1:nsimcov])
    
    if (length(covariate$limits) == 0) {
      # limits are omitted altogether
      covLimits <- orig.covlim # they will be written to file, but ignored in sim
      omitCovLimits <- T
    } else {
      # covariate limits are supplied as named list
      badNames <- which(!names(covariate$limits) %in% names(covMean))
      if (length(badNames) > 0) {
        endNicely("\nThe limit element of covariate must be a list with parameter names; see ?SIMrun for help.\n", model, data)
      }
      # first, make matrix with original covariate limits
      covLimits <- orig.covlim
      # now figure out which covariates have different limits and change them
      goodNames <- which(names(covMean) %in% names(covariate$limits))
      if (length(goodNames) > 0) {
        covLimits[goodNames, ] <- t(sapply(1:length(goodNames), function(x) {
          covariate$limits[[x]]
        }))
      }
      omitCovLimits <- F # we are not omitting covariate limits
    }
    
    # combine limits and covLimits
    limits <- rbind(parLimits, covLimits)
    dimnames(limits) <- NULL
    
    
    # now, modify model file by moving covariates up to primary
    # do it non-destructively, so new model is c_model and old model is preserved
    
    blocks <- parseBlocks(model)
    covPrim <- sapply(1:nsimcov, function(x) paste(dimnames(orig.covlim)[[1]][x], paste(covLimits[x, ], collapse = ","), sep = ","))
    blocks$primVar <- c(blocks$primVar, covPrim)
    if (length(covariate$fix) > 0 && length(fixedCov) == 0) {
      blocks$covar <- "" # no fixed covariates so all are moved to #Pri
    } else {
      blocks$covar <- covariate$fix
    }
    # some fixed, so leave these behind
    blocks <- blocks[unlist(lapply(blocks, function(x) x[1] != ""))]
    
    newmodel <- file(paste("c_", model, sep = ""), open = "wt")
    invisible(
      lapply(
        1:length(blocks),
        function(x) {
          cat(paste("#", toupper(names(blocks)[x]), "\n", sep = ""), file = newmodel, append = T)
          cat(paste(blocks[[x]], collapse = "\n"), file = newmodel, append = T)
          cat("\n\n", file = newmodel, append = T)
        }
      )
    )
    close(newmodel)
    
    # re-assign model
    model <- paste("c_", model, sep = "")
    
    # remove simulated covariates from data file non-destructively
    if (length(covariate$fix) > 0) {
      keepCov <- which(names(dataFile) %in% covariate$fix)
      dataFile <- dataFile[, c(1:getFixedColNum(), keepCov)]
    } else {
      dataFile <- dataFile[, 1:getFixedColNum()]
    }
    # re-assign data
    data <- paste("c_", data, sep = "")
    PMwriteMatrix(dataFile, data, override = T)
    
    # remake poppar
    poppar$popMean <- meanVector
    poppar$popCov <- covMat
    # clean up covlimits if necessary
    if (omitCovLimits) {
      covLimits <- matrix(rep(NA, 2 * nsimcov), ncol = 2)
      limits <- rbind(parLimits, covLimits)
    }
    
    # if split is true, then remake (augment) popPoints by adding mean covariate prior to each point
    if (split) {
      popPoints <- poppar$popPoints
      covToAdd <- covMean[1:nsimcov]
      npoints <- nrow(popPoints)
      prob <- popPoints[, npar + 1]
      covDF <- matrix(covToAdd, nrow = 1)
      covDF <- matrix(covDF[rep(1, npoints), ], ncol = length(covToAdd))
      covDF <- data.frame(covDF)
      names(covDF) <- names(covToAdd)
      popPoints <- cbind(popPoints[, 1:npar], covDF)
      popPoints$prob <- prob
      poppar$popPoints <- popPoints
    }
  } else {
    simWithCov <- F
    limits <- parLimits
  }
  # end if (covariate) block
  
  # get information from datafile
  dataoffset <- 2 * as.numeric("addl" %in% names(dataFile))
  ncov <- ncol(dataFile) - (12 + dataoffset)
  if (ncov > 0) {
    covnames <- names(dataFile)[(13 + dataoffset):ncol(dataFile)]
  } else {
    covnames <- NA
  }
  numeqt <- max(dataFile$outeq, na.rm = T)
  
  # handle include/exclude
  dataFile <- includeExclude(dataFile, include, exclude)
  toInclude <- unique(dataFile$id)
  nsub <- length(toInclude)
  
  
  postToUse <- postToUse[postToUse %in% toInclude] # subset the posteriors if applicable
  if (length(postToUse) > 0 && length(postToUse) != nsub) endNicely(paste("\nYou have ", length(postToUse), " posteriors and ", nsub, " selected subjects in the data file.  These must be equal.\n", sep = ""), model, data)
  
  if (missing(obsNoise)) {
    # obsNoise not specified, set to 0 for all outeq or NA (will use model file values) if makecsv
    obsNoise <- rep(0, 4 * numeqt)
    if (!missing(makecsv)) {
      obsNoise <- rep(NA, 4 * numeqt)
      cat("Setting obsNoise to model file assay error.  When making a csv file, you cannot specify no obsNoise.\n")
      flush.console()
    }
  }
  if (all(is.na(obsNoise))) {
    # obsNoise set to NA, so get coefficients from data file; if missing will grab from model file later
    obsNoiseNotMiss <- lapply(1:numeqt, function(x) which(!is.na(dataFile$c0) & dataFile$outeq == x)[1]) # get non-missing coefficients for each output
    
    checkObsNoise <- function(x, outeq) {
      if (is.na(x)) {
        if (!quiet) {
          cat(paste("Missing error coefficients in data file; model file defaults used for output ", outeq, ".\n", sep = ""))
          flush.console()
        }
        return(rep(NA, 4))
      } else {
        return(c(dataFile$c0[x], dataFile$c1[x], dataFile$c2[x], dataFile$c3[x]))
      }
    }
    obsNoise <- unlist(lapply(1:numeqt, function(x) checkObsNoise(obsNoiseNotMiss[[x]], x)))
  }
  
  
  
  # attempt to translate model file into  fortran model file
  modeltxt <- model
  engine <- list(alg = "SIM", ncov = ncov, covnames = covnames, numeqt = numeqt, limits = limits, indpts = -99)
  trans <- makeModel(model = model, data = dataFile, engine = engine, write = T, quiet = quiet)
  if (trans$status == -1) {
    endNicely(trans$msg, modeltxt, data)
  } else {
    model <- trans$model
    nvar <- trans$nvar # number of random parameters
    nofix <- trans$nofix # number of fixed constant parameters
    if (length(trans$nranfix) > 0) {
      nranfix <- trans$nranfix # number of fixed random parameters
    } else {
      nranfix <- 0
    }
    valfix <- trans$valfix
    asserr <- trans$asserr
    
    # get final values of fixed but random parameters
    if (nranfix > 0) {
      valranfix <- poppar$popRanFix
    } else {
      valranfix <- NULL
    }
    
    # grab limits from model file if they were not set to null
    if (!omitParLimits) {
      parLimits <- as.matrix(trans$ab[1:npar, ])
    }
    if (simWithCov && !omitCovLimits) {
      covLimits <- as.matrix(trans$ab[(1 + npar):(nsimcov + npar), ])
    }
    
    # final limits
    if (simWithCov) {
      limits <- rbind(parLimits, covLimits)
    } else {
      limits <- parLimits
    }
    
    # parameter and covariate types
    ptype <- ifelse(trans$ptype == 1, "r", "f") # will be fixed for either fixed random or fixed constant
    ctype <- trans$ctype
    if (ctype[1] == -99) {
      ctype <- NULL
    }
    
    # make the correct string of values for fixed parameters
    posranfix <- which(trans$ptype == 2)
    posfix <- which(trans$ptype == 0)
    allFix <- c(valfix, valranfix)
    allFix <- allFix[!is.na(allFix)]
    fixedVals <- allFix[rank(c(posfix, posranfix))]
  }
  if (identical(modeltxt, model)) {
    modelfor <- T
  } else {
    modelfor <- F
  }
  
  
  OS <- getOS()
  # read or define the Fortran compiler
  fortSource <- paste(system.file("", package = "Pmetrics"), "compiledFortran", sep = "/")
  # TODO: change this
  if (!binaries.installed()) {
    PMbuild()
  }
  compiler <- getPMoptions()$compilation_statements
  # choose serial compiliation
  if (length(compiler) == 2) {
    compiler <- compiler[1]
  }
  if (is.null(compiler)) {
    cat("\nExecute SIMrun after fortran is installed.\n")
    return(invisible(NULL))
  }
  
  enginefiles <- shQuote(normalizePath(list.files(fortSource, pattern = "sSIMeng", full.names = T)))
  enginecompile <- sub("<exec>", "montbig.exe", compiler)
  enginecompile <- sub("<files>", enginefiles, enginecompile, fixed = T)
  
  if (missing(makecsv)) {
    makecsv <- 0
  } else {
    if (nsim > 50) {
      ans <- readline("Warning: creating a csv file with more than 50 simulations can take a very long time.\nDo you wish to proceed (y/n)?")
      if (ans == "n") {
        cat("\nAborting simulation...\n")
        return()
      }
    }
    if (makecsv == "simdata.csv") stop(paste0(crayon::red("simdata.csv "), "is reserved. Use another name for ", crayon::green("makecsv"), "."))
    if (file.exists(makecsv)) file.remove(makecsv)
    orig.makecsv <- makecsv
    makecsv <- c("1", "abcde.csv")
  }
  
  # get prior density
  getSimPrior <- function(i) {
    # get prior density
    if (inherits(poppar, "NPAG")) {
      if (split) {
        popPoints <- poppar$popPoints
        ndist <- nrow(popPoints)
        if (ndist > 30) {
          ndist <- 30
        }
        # take the 30 most probable points as there are max 30 distributions in simulator
        popPointsOrdered <- popPoints[order(popPoints$prob), ]
        pop.weight <- popPointsOrdered$prob[1:ndist]
        pop.mean <- popPointsOrdered[1:ndist, 1:(ncol(popPointsOrdered) - 1)]
        pop.cov <- poppar$popCov
      } else {
        if (length(postToUse) == 0) {
          pop.weight <- 1
          pop.mean <- data.frame(t(poppar$popMean))
          pop.cov <- poppar$popCov
          ndist <- 1
        } else {
          thisPost <- which(poppar$postMean$id == toInclude[i])
          pop.weight <- 1
          pop.mean <- data.frame(poppar$postMean[thisPost, -1])
          pop.cov <- poppar$postCov[, , thisPost]
          ndist <- 1
        }
      }
      # if there are fixed variables in simulation, check to see which should be fixed in prior and remove if necessary
      if (nofix > 0) {
        whichfix <- trans$blocks$primVar[ptype == "f"]
        whichrand <- trans$blocks$primVar[ptype == "r"]
        modelpar <- names(pop.mean)
        if (!all(modelpar %in% c(whichfix, whichrand))) stop("Primary parameters in simulation model file do not match parameters\nin the PM_final object used as a simulation prior.\n")
        tofix <- which(modelpar %in% whichfix)
        if (length(tofix) > 0) {
          pop.mean <- pop.mean[, -tofix]
          pop.cov <- pop.cov[-tofix, -tofix]
        }
      }
    } else {
      pop.weight <- poppar[[1]]
      ndist <- length(pop.weight)
      if (inherits(poppar[[2]], "numeric")) {
        pop.mean <- data.frame(t(poppar[[2]]))
      } else {
        pop.mean <- data.frame(poppar[[2]])
      }
      pop.mean <- pop.mean[order(pop.weight), ] # sort means by order of probability
      if (ndist > 30) {
        ndist <- 30
      }
      # take the 30 most probable points as there are max 30 distributions in simulator
      pop.weight <- sort(pop.weight)
      pop.weight <- pop.weight[1:ndist]
      pop.mean <- pop.mean[1:ndist, ]
      pop.cov <- data.frame(poppar[[3]])
    }
    
    # check to make sure pop.cov (within 15 sig digits, which is in file) is pos-def and fix if necessary
    posdef <- eigen(signif(pop.cov, 15))
    if (any(posdef$values < 0)) {
      cat("Warning: your covariance matrix is not positive definite.\nThis is typically due to small population size.\n")
      ans <- readline("\nChoose one of the following:\n1) end simulation\n2) fix covariance\n3) set covariances to 0\n ")
      if (ans == 1) stop()
      if (ans == 2) {
        # eigendecomposition to fix the matrix
        eigen_values <- eigen(pop.cov)$values
        eigen_vectors <- eigen(pop.cov)$vectors
        pop.cov <- eigen_vectors %*% diag(pmax(eigen_values, 0)) %*% t(eigen_vectors)
        # pop.cov <- as.matrix(Matrix::nearPD(as.matrix(pop.cov), keepDiag = T)$mat)
      }
      if (ans == 3) {
        pop.cov2 <- diag(0, nrow(pop.cov))
        diag(pop.cov2) <- diag(pop.cov)
        pop.cov <- pop.cov2
      }
    }
    
    # transform pop.cov into a vector for inclusion in the instruction file
    pop.cov[upper.tri(pop.cov)] <- NA
    pop.cov <- as.vector(t(pop.cov))
    pop.cov <- pop.cov[!is.na(pop.cov)]
    # divide it by the number of points (max 30); if split=F, ndist=1
    pop.cov <- pop.cov / ndist
    
    # if nsim=0 then we will use each population point to simulate a single
    # output based on the template; otherwise, we will use the specified prior
    
    
    if (nsim == 0 & inherits(poppar, "NPAG")) {
      if (simWithCov) {
        # can't simulate from each point with covariate sim
        endNicely(paste("You cannot simulate each point with simulated covariates.\n"), model, data)
      }
      if (length(postToUse) == 0) {
        popPoints <- poppar$popPoints
      } else {
        popPoints <- poppar$postMean
      }
      # put it all together in the following order
      # 2:                             enter values from "results of BIG NPAG run"
      # 2:                             use each grid point once
      # 1:                             enter values manually
      # ndist:                         number of grid points
      # gridpts:                       values of gridpoints
      gridpts <- c(t(popPoints[, 1:nvar]))
      priorSource <- c(2, 2, 1, nrow(popPoints), gridpts)
      
      # make some confirmation answers
      # rep(1,2):                            #confirm one point per sim
      # confirm gridpoints
      
      confirm <- rep(1, 2)
    } else { # end of block to make distribution when nsim=0
      
      
      
      # put it all together in the following order
      # 1:                             enter values from "keyboard"
      # ndist:                         number of distributions
      # 0:                             covariances
      # dist:                          weight, mean, 0 for covariance matrix, and cov matrix for each dist
      # 1:                             gaussian distributions
      # make distribution string
      dist <- list()
      for (i in 1:ndist) {
        dist[[i]] <- unlist(c(pop.weight[i], pop.mean[i, ], 0, pop.cov))
      }
      dist <- unlist(dist)
      priorSource <- c(1, ndist, 0, dist, 1)
      
      # make some confirmation answers
      # 0:                            #covariance matrix
      # rep("go",ndist):                #view distributions
      # rep("1",2):                     #distribution info is correct
      # restrictions on parameters are correct
      confirm <- c("0", rep("go", ndist), rep("1", 2))
    }
    # end of block to make distribution when nsim>0
    
    
    # apply limits as necessary
    # this will result in string with "f" or "r,1" or "r,0,a,b" for fixed, random no limits,
    # or random with limits a and b, respectively
    if (sum(ptype == "r") > ncol(pop.mean)) stop("You have specified variables to be random in your model file\nthat were not random in poppar.\n")
    varDF <- data.frame(ptype = ptype, limit = ifelse(ptype == "r", apply(limits, 1, function(x) ifelse(all(is.na(x)), 1, 0)), NA))
    varDF$a[varDF$ptype == "r"] <- limits[, 1]
    varDF$b[varDF$ptype == "r"] <- limits[, 2]
    varVec <- c(apply(varDF, 1, c))
    varVec <- varVec[!is.na(varVec)]
    varVec <- gsub("[[:space:]]", "", varVec)
    
    return(list(varVec = varVec, priorSource = priorSource, confirm = confirm))
  }
  # end getSimPrior function
  
  # check if output files already exist
  
  # if nsim==0, change to 1, but will be ignored
  if (nsim == 0) {
    nsimtxt <- 1
  } else {
    nsimtxt <- nsim
  }
  
  # other simulation arguments
  if (missing(outname)) {
    outname <- "simout"
  }
  oldfiles <- Sys.glob(paste(outname, "*", sep = ""))
  nexisting <- length(oldfiles)
  if (nexisting > 0) {
    if (overwrite) {
      file.remove(oldfiles)
    } else {
      cat("The working directory already contains ", nexisting, " files matching the output name (from ", format(file.mtime(oldfiles[[1]]), format = "%a %b %d %Y"), ").", sep = "")
      ans <- readline(cat("\nWhat would you like to do?\n1) delete all existing files '", outname, "*.txt'\n2) prefix existing files with 'old_' (overwriting any that may already exist)\n3) abort function", sep = ""))
      if (ans == 1) {
        file.remove(oldfiles)
      } else if (ans == 2) {
        file.rename(oldfiles, paste("old_", oldfiles, sep = ""))
      } else {
        stop("Function aborted, please re-run with a different output name.", call. = F)
      }
    }
  }
  
  # other simulation arguments
  if (identical(length(obsNoise), length(asserr))) {
    obsNoise[is.na(obsNoise)] <- asserr[is.na(obsNoise)] # try to set any missing obsNoise to asserr from model file
  } else {
    obsNoise[is.na(obsNoise)] <- 0
  }
  # but if can't, set any missing obsNoise to 0
  ode <- c(0, 10**ode)
  
  # compile simulator
  if (OS == 1 | OS == 3) {
    system(paste(enginecompile, model))
  } else {
    shell(paste(enginecompile, model))
  }
  # create seed
  if (length(seed) < nsub) seed <- rep(seed, nsub)
  seed <- floor(seed) # ensure that seed is a vector of integers
  
  if (!clean) {
    instructions <- c("1", "sim.inx")
  } else {
    (instructions <- "0")
  }
  
  
  
  # cycle through the subjects and simulate
  if (!quiet) cat(paste("\nThe following subject(s) in the data will serve as the template(s) for simulation: ", paste(toInclude, collapse = " "), "\n\n"))
  for (i in 1:nsub) {
    if (!quiet) {
      cat(paste("Simulating from subject", toInclude[i], "...\n"))
      flush.console()
    }
    
    temp <- subset(dataFile, dataFile$id == toInclude[i])
    if (nrow(temp) == 0) {
      if (!quiet) {
        cat(paste("\nThere is no subject with an ID of ", toInclude[i], ". Skipping...\n", sep = ""))
        flush.console()
      }
      next
    }
    # add prediction times if necessary
    predTimes <- NA
    if (is.list(predInt)) {
      # predInt is a list of (start,end,interval)
      if (any(sapply(predInt, length) != 3)) {
        stop("If a list, each element of predInt must be of the form c(start,end,interval).\n")
      }
      predTimes <- sapply(predInt, function(x) rep(seq(x[1], x[2], x[3]), each = numeqt))
      # catenate columns into single vector
      predTimes <- unlist(predTimes)
    } else {
      # predTimes is not a list
      if (length(predInt) == 1) {
        # predInt is a single value
        if (predInt != 0) {
          # it is not zero
          predTimes <- rep(seq(0, ceiling(max(temp$time, na.rm = T)), predInt)[-1], each = numeqt)
        }
        # it was 0 so do nothing
      } else {
        # predInt is a single vector of c(start,stop,interval)
        if (length(predInt) == 3) {
          predTimes <- rep(seq(predInt[1], predInt[2], predInt[3]), each = numeqt)
        } else {
          stop("\npredInt is misspecified.  See help for SIMrun.\n")
        }
      }
    }
    # now, if predInt was specified in any way, predTimes will not be NA
    if (!is.na(predTimes[1])) {
      predTimes <- predTimes[predTimes > 0] # remove predictions at time 0
      predTimes <- predTimes[!predTimes %in% dataFile$time[dataFile$evid == 0]] # remove prediction times at times that are specified in template
      numPred <- length(predTimes)
      maxsim <- 594 - length(temp$evid[temp$evid == 0])
      if (numPred > maxsim) {
        # too many predictions
        numPred.total <- numPred + length(temp$evid[temp$evid == 0])
        predTimes <- predTimes[1:(maxsim - maxsim %% numeqt)]
        numPred <- length(predTimes)
        cat(paste("The maximum number of simulated observations is 594.  Your prediction interval, specific prediction times, and time horizon results in ", numPred.total, " predictions.\nInterval predictions will be truncated at time ", predTimes[numPred], ", plus predictions at specific times in the template (if any).\n", sep = ""))
      }
      newPred <- data.frame(matrix(NA, nrow = numPred, ncol = ncol(temp)))
      newPred[, 1] <- temp$id[1] # id
      newPred[, 2] <- 0 # evid
      newPred[, 3] <- predTimes # time
      newPred[, 9] <- 1 # out
      newPred[, 10] <- rep(1:numeqt, numPred / numeqt) # outeq
      names(newPred) <- names(temp)
      temp <- rbind(temp, newPred)
      temp <- temp[order(temp$time, temp$outeq), ]
    }
    
    PMwriteMatrix(temp, "ZMQtemp.csv", override = T, version = "DEC_11")
    if (length(makecsv) == 2) makecsv[2] <- paste("abcde", i, ".csv", sep = "")
    outfile <- paste(outname, i, ".txt", sep = "")
    if (file.exists(outfile)) {
      file.remove(outfile)
    }
    if (file.exists("sim.inx")) {
      file.remove("sim.inx")
    }
    
    if (length(postToUse) > 0) {
      thisPrior <- getSimPrior(i)
    } else {
      if (i == 1) thisPrior <- getSimPrior(i)
    }
    
    # build the control stream
    simControl <- unlist(c(
      "1", # files in current directory
      "0", # input from "keyboard"
      model, # name of model file
      thisPrior$varVec, # random parameters and limits if they exist
      "1", # input from .csv file
      "ZMQtemp.csv", # name of .csv file
      ctype, # piecewise covariates
      "go",
      nsimtxt, # number of simulations/subject
      fixedVals, # value of any fixed parameters
      ode, # ode tolerance
      obsNoise, # observation noise
      "1", # skip explanation of noisy values
      doseTimeNoise, # dose time noise
      doseNoise, # dose noise
      obsTimeNoise, # observation time noise
      thisPrior$priorSource, # prior
      outfile, # output file name (without extension)
      makecsv, # make .csv file?
      "0", # read file seeto.mon for seed
      rep("1", 2), # data file info is correct
      # nsim is correct
      "go",
      rep("go", numeqt),
      rep("1", 3), # observation error information is correct
      # skip explanation of noisy values
      # other error information is correct
      thisPrior$confirm, # confirm answers
      rep("1", 4), # common confimations
      # output file is correct
      # confirm .wrk file generation
      # confirm starting seed
      # all instructions are now correct
      instructions
    )) # instruction file
    simControl <- simControl[!is.na(simControl)]
    f <- file("simControl.txt", "w")
    writeLines(simControl, f, sep = "\r\n")
    close(f)
    
    # make seed file and run
    if (OS == 1 | OS == 3) {
      system(paste("echo", seed[i], "> seedto.mon"))
      system("./montbig.exe MacOSX < simControl.txt", ignore.stdout = T)
    } else {
      shell(paste("echo", seed[i], "> seedto.mon"))
      shell("montbig.exe DOS < simControl.txt", invisible = T)
    }
  }
  # clean up csv files if made
  if (length(makecsv) == 2) {
    trunc <- ceiling(log10(nsim + 1)) + 1
    temp <- PMreadMatrix("abcde1.csv", quiet = T)
    simnum <- unlist(lapply(temp$id, function(x) substr(gsub("[[:space:]]", "", x), 9 - trunc, 8)))
    temp$id <- paste(toInclude[1], simnum, sep = "_")
    # add back simulated covariates if done, but keep non-simulated ones
    if (simWithCov) {
      getBackCov <- function(temp, n) {
        parValues <- SIMparse(paste(outname, n, ".txt", sep = ""), quiet = T)$parValues
        covOrig <- names(CVsum)[3:(ncol(CVsum) - npar)]
        covSim <- names(parValues)[names(parValues) %in% covOrig]
        covSimPos <- which(covOrig %in% covSim)
        covNotSimPos <- which(!covOrig %in% covSim)
        if (length(covSimPos) > 0) {
          nfixed <- getFixedColNum()
          covDF <- cbind(parValues[, 1 + npar + covSimPos])
          if (length(covNotSimPos) > 0) {
            covDF <- cbind(covDF, temp[, nfixed + covNotSimPos])
          }
          covDF <- covDF[, rank(c(covSimPos, covNotSimPos))]
        }
        temp[, (nfixed + 1):(nfixed + ncol(covDF))] <- NA
        temp[!duplicated(temp$id), (nfixed + 1):(nfixed + ncol(covDF))] <- covDF
        names(temp)[(nfixed + 1):(nfixed + ncol(covDF))] <- covOrig
        return(temp)
      }
      temp <- getBackCov(temp, 1)
    }
    
    if (any(unlist(lapply(as.character(unique(temp$id)), function(x) nchar(x) > 11)))) stop("Shorten all template id values to 6 characters or fewer.\n")
    if (nsub > 1) {
      for (j in 2:nsub) {
        curTemp <- PMreadMatrix(paste("abcde", j, ".csv", sep = ""), quiet = T)
        simnum <- unlist(lapply(curTemp$id, function(x) substr(gsub("[[:space:]]", "", x), 9 - trunc, 8)))
        curTemp$id <- paste(toInclude[j], simnum, sep = "_")
        if (simWithCov) {
          curTemp <- getBackCov(curTemp, j)
        }
        if (any(unlist(lapply(as.character(unique(curTemp$id)), function(x) nchar(x) > 11)))) stop("Shorten all template id values to 6 characters or fewer.\n")
        temp <- rbind(temp, curTemp)
      }
    }
    zero <- which(temp$evid == 1 & temp$dur == 0 & temp$dose == 0)
    if (length(zero) > 0) temp <- temp[-zero, ]
    
    # remove any white space
    temp <- purrr::map_df(temp, stringr::str_trim)
    names(temp) <- purrr::map_chr(names(temp), stringr::str_trim)
    
    ### update the version once simulator updated
    PMwriteMatrix(temp, orig.makecsv, override = T, version = "DEC_11")
  }
  exampleName <- paste(outname, "1.txt", sep = "")
  if (!quiet) {
    if (length(Sys.glob(paste(outname, "*", sep = ""))) > 0) cat("\nUse, for example, SIMparse(", dQuote(exampleName), ") to extract simulator output for analysis or plotting.\n", sep = "")
    flush.console()
  }
  if (clean) {
    invisible(file.remove(Sys.glob(c("fort.*", "*.Z3Q", "*.ZMQ", "montbig.exe", "ZMQtemp.csv", "simControl.txt", "seedto.mon", "abcde*.csv"))))
    if (!modelfor) {
      invisible(file.remove(model))
    }
    if (!model_file_src) {
      invisible(file.remove("simmodel.txt"))
    }
    if (!data_file_src) {
      invisible(file.remove("simdata.csv"))
    }
  }
}


# SIMparse ----------------------------------------------------------------

#' @title Parse Pmetrics Simulator Output
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' Parses the output of the Pmetrics simulator.
#' This is largely superseded as it is automatically called by either of the simulation
#' methods: `PM_result$sim()` or `PM_sim$new()`. There is rarely a need any longer to call this
#' function directly.
#' @details
#' For \code{file} specification \dQuote{?} will be matched by just a single numeral or character; \dQuote{*} will be
#' matched by any number of consecutive alphanumeric characters.  Examples include \code{file='simout1.txt,simout2.txt,simout3.txt'},
#' \code{file='simout?.txt'} and \code{file='sim*.txt'}.All three will find the files simout1.txt,
#' simout2.txt, and simout3.txt in the working directory. The second example would also find simout4.txt, etc.  The third
#' example would also find sim_1.txt if that existed.
#' Note that to combine simulator output files, the numbers of simulated profiles may differ.
#' The number of outputs and times of observations also may differ, although combining these may lead to
#' strange plots since not all profiles have the same observations.
#' For parallel execution, the function requires packages 'doParallel' and 'foreach'. If not installed, it will try to install, failing that it will run in serial mode.
#'
#' @param file An output file or files of the simulator in the current working directory, or the full
#' pathname to the file.  To load and combine multiple outputs, specify files separated by commas
#' or using wild cards.  See details.
#' @param include A vector of files to include in the parsing.  For example, if you used a wild
#' card in the \code{file} argument, such as \dQuote{simout?.txt}, which returned four files:
#' simout1.txt, simout2.txt, simout3.txt and simout4.txt, and you wished to only parse the first
#' and fourth file, specify \code{include=c(1,4)}.
#' @param exclude See the discussion for \code{include}, but this will exclude specified files.
#' @param combine Boolean parameter, default \code{False}, which specifies whether you wish to combine
#' the parsed files into a single PMsim object.  This can be useful for making visual predictive
#' checks, for example.  If \code{combine=F}, and multiple files are parsed, then the return object
#' will be a list of PMsim objects, which can be plotted or otherwise accessed using standard list
#' referencing, e.g. `simlist[[1]]`, `simlist[[2]]`, etc.
#' @param quiet Suppress messages
#' @return If one file is parsed or multiple files are parsed and combined, the return will be a list with five items, of class \emph{PMsim}.
#' If multiple files are parsed and not combined, then the return will be a *PM_simlist* of \emph{PMsim} objects.
#' \item{obs }{An data frame of simulated observations with 4 columns: id, time, out, outeq.
#' \emph{id} is the number of the simulated subject, which will have a unique ending appended
#' if simulations are combined, such that \emph{id} will become x.y with x being the simulated profile
#' number and y being the simulation template number.  \emph{time} is the time of the simulated
#' output, \emph{out} of output equation number \emph{outeq}.}
#' \item{amt }{An data frame of simulated amounts with 4 columns: id, time, out, comp.
#' \emph{id} is the number of the simulated subject, which will have a unique ending appended
#' if simulations are combined, such that \emph{id} will become x.y with x being the simulated profile
#' number and y being the simulation template number.  \emph{time} is the time of the simulated
#' amount, \emph{out} in compartment  number \emph{comp}.}
#' \item{parValues }{A datframe of the simulated parameter values, combined across files as necessary}
#' \item{totalSets}{The total number of parmeter sets simulated, which may be greater than the number of rows in \code{parValues} if some sets
#' were discarded for being outside specified limits.  For more than one file parsed, this will the total number in all files.}
#' \item{totalMeans}{The means of each simulated parameter based on all profiles in a given file (even those discarded for exceeding limits).
#' For more than one file parsed, this will be the weighted averages for all simulations.}
#' \item{totalCov}{The covariances of the simulated parameter sets based on all profiles in a given file (even those discarded for exceeding limits).
#' For more than one file parsed, this will be the weighted averages for all simulations.}
#' A plot method exists in \code{\link{plot.PMsim}} for \emph{PMsim} objects.
#' @author Michael Neely
#' @seealso [PM_result], [PM_sim], [SIMrun]
#' @export

SIMparse <- function(file, include, exclude, combine = F, quiet = F) {
  processfile <- function(n) {
    out <- readLines(allfiles[n])
    nsim <- as.numeric(stringr::str_extract(out[grep(" THE NO. OF SIMULATED SUBJECTS", out)], "[[:digit:]]+"))
    nout <- as.numeric(stringr::str_extract(out[grep(" THE NO. OF OUTPUT EQUATIONS", out)], "[[:digit:]]+"))
    nobs <- as.numeric(stringr::str_extract(out[grep(" VALUES FOR EACH OUTPUT EQUATION", out)], "[[:digit:]]+"))
    i <- grep("CONTAIN THE SIMULATED OBSERVED$", out)
    times <- as.numeric(scan(allfiles[n], skip = i + 6, n = nobs + 1, what = "character", quiet = T)[-1])
    
    
    # get compartment amounts and outeq concentrations for each output
    # places for compartment amounts
    amtlines <- grep("COMPARTMENT NO", out) + 1
    # number of compartments
    ncomp <- length(amtlines)
    
    
    # places for observations
    obslines <- grep("OUTPUT EQUATION NO", out) + 1
    # skip observed assay noise
    obslines <- obslines[-(1:nout)]
    
    # id is a block of 1:nsim repeated for each observation, all repeated for each compartment
    # time is a block of the times repeated for each subject, all repeated for each compartment
    # amt all amounts
    # outeq is a block of 1:nout, repeated for nsim*nobs
    if (ncomp > 0) {
      amt <- data.frame(
        id = rep(rep(1:nsim, each = nobs), ncomp),
        time = rep(rep(times, nsim), ncomp),
        out = c(unlist(sapply(1:ncomp, function(a) {
          scan(allfiles[n], skip = amtlines[a], n = nsim * (nobs + 1), quiet = T)[-(seq(0, (nsim - 1) * (nobs + 1), (nobs + 1)) + 1)]
        }))),
        comp = rep(1:ncomp, each = nsim * nobs)
      )
    } else {
      amt <- NA
    }
    
    # id is a block of 1:nsim repeated for each observation, all repeated for each output
    # time is a block of the times repeated for each subject, all repeated for each output
    # out is all observations
    # outeq is a block of 1:nout, repeated for nsim*nobs
    obs <- data.frame(
      id = rep(rep(1:nsim, each = nobs), nout),
      time = rep(rep(times, nsim), nout),
      out = c(unlist(sapply(1:nout, function(a) {
        scan(allfiles[n], skip = obslines[a], n = nsim * (nobs + 1), quiet = T)[-(seq(0, (nsim - 1) * (nobs + 1), (nobs + 1)) + 1)]
      }))),
      outeq = rep(1:nout, each = nsim * nobs)
    )
    obs$out[obs$out == -99] <- NA
    
    
    # get simulated parameter values
    
    i <- grep("PARAMETER VALUES FOR ALL THE SUBJECTS.", out)
    parNames <- unlist(strsplit(out[i + 2], " +"))[-1]
    parValues <- t(sapply(
      strsplit(out[(i + 3):(i + 2 + nsim)], " +"),
      function(x) as.numeric(x[-1])
    ))
    parValues <- data.frame(parValues)
    names(parValues) <- parNames
    names(parValues)[names(parValues) == "SUBJ."] <- "id"
    
    
    # get means and covariances of entire simulated set
    
    i <- grep("BECAUSE OF PARAMETER BOUNDARY RESTRICTIONS", out)
    if (length(i) > 0) {
      totalSets <- as.numeric(scan(allfiles[1], what = "character", skip = i, nlines = 1, quiet = T, strip.white = T)[1])
      i <- grep("SAMPLE MEANS OF ALL PARAMETER DATA ARE", out)
      totalMeans <- as.numeric(scan(allfiles[n], what = "character", skip = i, nlines = 1, quiet = T, strip.white = T))
      i <- grep("SAMPLE COV. MATRIX OF ALL PARAMETER DATA IS", out)
      totalCov <- suppressWarnings(as.numeric(scan(allfiles[n], what = "character", skip = i, nlines = length(totalMeans), quiet = T, strip.white = T)))
      NArows <- which(is.na(totalCov))
      NArows <- c(NArows, NArows + 1)
      totalCov <- totalCov[-NArows]
      mat <- matrix(NA, nrow = length(totalMeans), ncol = length(totalMeans))
      mat[lower.tri(mat, diag = T)] <- totalCov
      mat[upper.tri(mat, diag = T)] <- totalCov
      totalCov <- t(mat)
    } else {
      totalSets <- nsim
      if (length(parValues) > 2) {
        totalMeans <- apply(parValues[, -1], 2, mean)
        totalCov <- cov(parValues[, -1], method = "pearson")
      } else {
        totalMeans <- parValues[1, 2]
        totalCov <- NA
      }
    }
    names(totalMeans) <- parNames[-1]
    if (length(parValues) > 2) dimnames(totalCov) <- list(parNames[-1], parNames[-1])
    
    return(list(obs = obs, amt = amt, parValues = parValues, totalSets = totalSets, totalMeans = totalMeans, totalCov = totalCov))
  } # end of processfile function
  
  if (missing(file)) {
    cat("Please provide filename of Pmetrics simulation output file(s).\n")
    return()
  }
  
  # extract pattern from strings
  strparse <- function(pattern, x) {
    match <- regexpr(pattern, x, perl = T) # perl=T required for the lookahead
    start <- match[1]
    stop <- match[1] + attr(match, "match.length") - 1
    return(substr(x, start, stop))
  }
  
  # separate files if more than one
  files <- unlist(strsplit(file, ","))
  # remove leading and trailing spaces
  files <- sub("^[[:blank:]]*", "", files)
  files <- sub("[[:blank:]]*$", "", files)
  
  # check that wildcard-generated list does not include outdated files
  # if out-of-order files are found, the user has the option to include them, exclude them, or abort
  nfilepar <- length(files)
  tobeignored <- c()
  for (n in 1:nfilepar) { # cycle through individual filename parameters passed
    if (length(grep("\\?", files[[n]])) > 0 | length(grep("\\*", files[[n]])) > 0) {
      workfiles <- Sys.glob(files[[n]])
      worksimnum <- as.numeric(sapply(workfiles, function(x) strparse("([[:digit:]]+)(?!.*[[:digit:]])", x)))
      workfiles <- workfiles[order(worksimnum)]
      if (!missing(include)) {
        workfiles <- workfiles[include]
      }
      if (!missing(exclude)) {
        workfiles <- workfiles[-exclude]
      }
      nworkfiles <- length(workfiles)
      if (nworkfiles > 1) {
        for (n in 2:nworkfiles) {
          if (file.mtime(workfiles[n]) < file.mtime(workfiles[n - 1])) {
            cat("\nFile ", workfiles[n], " is older than ", workfiles[n - 1], ". Possible leftover from previous SIMrun?", sep = "")
            ans <- readline(cat("\nWhat would you like to do?\n1) include and continue\n2) ignore files after ", workfiles[n - 1], "\n3) abort function", sep = ""))
            if (ans == 1) {
              next
            } else if (ans == 2) {
              tobeignored <- append(tobeignored, workfiles[n:nworkfiles])
              break
            } else {
              stop("Function aborted.", call. = F)
            }
          } # end of IF older
        }
      } # end of check list of files generated by wildcard block
    } # end of IF wildcard used block
  } # end of filecheck cycle
  
  allfiles <- unique(Sys.glob(files)) # unique to exclude duplicates
  simnum <- as.numeric(sapply(allfiles, function(x) strparse("([[:digit:]]+)(?!.*[[:digit:]])", x)))
  # new reg exp matches the last number in filename rather than the first; "run5out1.txt" will now return 1 rather than 5
  allfiles <- allfiles[order(simnum)]
  if (!missing(include)) {
    allfiles <- allfiles[include]
  }
  if (!missing(exclude)) {
    allfiles <- allfiles[-exclude]
  }
  allfiles <- setdiff(allfiles, tobeignored) # delete files that are to be ignored from the date check
  
  nfiles <- length(allfiles)
  if (nfiles == 0) {
    stop("No files found.\n")
  }
  
  
  # process return objects
  
  if (!quiet) {
    cat(paste("\nProcessing ", nfiles, " simulated data file(s).\n", sep = ""))
    flush.console()
  }
  
  #map through files in pseudo-parallel; full parallel removed as actually longer
  simlist <- purrr::pmap(list(1:nfiles), processfile, .progress = list(type = "tasks", name = "Files") )
  
  
  # combine obs if requested
  if (combine & nfiles > 1) {
    if (!quiet) {
      cat("\nCombining files...\n")
      flush.console()
    }
    # make unique id values of "id.simnumber" for each set
    simlist <- lapply(seq_along(simlist), function(x) {
      simlist[[x]]$obs$id <- paste(simlist[[x]]$obs$id, sprintf("%02d", x), sep = ".")
      simlist[[x]]
    })
    
    obs <- do.call(rbind, lapply(simlist, function(x) x$obs))
    amt <- do.call(rbind, lapply(simlist, function(x) x$amt))
    parValues <- do.call(rbind, lapply(simlist, function(x) x$parValues))
    totalSets <- sum(sapply(simlist, function(x) x$totalSets))
    totalMeans <- do.call(rbind, lapply(simlist, function(x) x$totalMeans * x$totalSets))
    totalMeans <- apply(totalMeans, 2, function(x) sum(x) / totalSets)
    totalCov <- Reduce("+", lapply(simlist, function(x) x$totalCov * x$totalSets)) / totalSets
    simlist <- list(obs = obs, amt = amt, parValues = parValues, totalSets = totalSets, totalMeans = totalMeans, totalCov = totalCov)
  }
  
  # return simlist
  if (nfiles > 1) { # more than one file
    if (combine) { # combined
      class(simlist) <- c("PMsim", "list")
      message <- paste("\nThe following files were successfully parsed and combined: ", paste(allfiles, collapse = ", "), "\n", sep = "")
    } else { # not combined
      simlist <- lapply(simlist, function(x) {
        class(x) <- c("PMsim", "list")
        x
      })
      class(simlist) <- c("PM_simlist", "list")
      message <- paste("\nThe following files were successfully parsed as a PM_simlist: ", paste(allfiles, collapse = ", "), "\n", sep = "")
    }
  } else { # only one file
    simlist <- simlist[[1]]
    class(simlist) <- c("PMsim", "list")
    message <- paste(paste("\nThe following file was successfully parsed: ", paste(allfiles, collapse = ", "), "\n", sep = ""))
  }
  
  if (!quiet) cat(message)
  return(simlist)
}

# PLOT --------------------------------------------------------------------
#' @title Plot Pmetrics Simulation Objects
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Plots *PM_sim* objects with the option to perform a visual and numerical predictive check
#' @details
#' Simulated observations are plotted as quantiles on the y-axis vs. time on the x.axis.  If measured
#' observations are included, a visual and numerical predictive check will be performed.
#' The default plot is to omit markers, but if the marker argument is set to `TRUE`,
#' the resulting marker will have the following plotly properties:
#' `list(symbol = "circle-open", color = "black", size = 8)`. By default a grid is
#' omitted. The legend is also omitted by default, but if included,
#' clicking on a quantile item in the legend will hide it in the plot,
#' and double clicking will hide all other quantiles.
#'
#' @method plot PM_sim
#' @param x The name of an *PM_sim* data object generated by [PM_sim]
#' @param mult `r template("mult")`
#' @param ci Width of confidence interval bands around simulated quantiles,
#' from 0 to 1.  If 0, or *nsim*<100, will not plot.
#' Default is 0.95, i.e. 95th percentile with tails of 2.5 percent above and below excluded.
#' @param binSize Width of binning interval for simulated concentrations, in time units, e.g. hours.
#' A `binSize` of 0.5 will pull all simulated concentrations +/- 0.5 hours into
#' the same time.  This is useful
#' for plotting PMsim objects made during [make_valid]. The default is 0, i.e. no binning.
#' If an `obs` object is provided, it will be binned similarly.
#' @param outeq `r template("outeq")`
#' @param line Controls the appearance of lines. It can be specified in several ways.
#' * Default is `TRUE` which results in simulated profiles summarized
#' as quantiles, with default values of 0.05, 0.25, 0.5, 0.75, and 0.95. The default
#' format will be applied, which is solid black lines of width 1.
#' Numerical predictive checking will be calculated if observations are also included
#' (see *obs* below).
#' * `FALSE` results in no lines plotted and the plot will be blank.
#' * `NA` Quantile summaries will be suppressed, but lines joining simulated outputs
#' will be plotted in default format as above. In other words, all profiles will be plotted,
#' not just the quantiles. Numerical predictive checking will be suppressed.
#' * List of quantiles and formats to plot with the following elements:
#'     - `probs` Vector of quantiles to include. If missing, will be set to
#'     defaults above, i.e., `c(0.05, 0.25, 0.5, 0.75, and 0.95)`
#'     Example: `line = list(probs = c(0.25, 0.5, 0.75))`.
#'     - `color` Vector of color names whose order corresponds to `probs`.
#'     If shorter than `probs`, will be recycled. Default is "dodgerblue".
#'     Examples: `line = list(color = "red")` or `line = list(color = c("red", "blue"))`.
#'     - `width` Vector of widths in pixels, as for `color`. Default is 1.
#'     Example: `line = list(width = 2)`.
#'     - `dash` Vector of dash types, as for color. Default is "solid".
#'     See `plotly::schema()`, traces > scatter > attributes > line > dash > values.
#'     Example: `line = list(dash = "dashdot")`.
#' @param marker `r template("include")` Formatting will only be applied to observations
#' if included via the `obs` argument.
#' @param obs The name of a [PM_result] data object or the PM_op field in the
#' PM_result object, all generated by [PM_load].  For example, if
#' `run1 <- PM_load(1)` and `sim1` is a PM_sim object, then
#' `sim1$plot(obs = run1)` or `sim1$plot(obs = run1$op)`.
#' If specified,
#' the observations will be overlaid upon the simulation plot
#' enabling a visual predictive check.  In this case,
#' a list object will be returned with two items: $npc containing the quantiles
#' and probability that the observations
#' are below each quantile (binomial test); and $simsum, the times of each
#' observation and the
#' value of the simulated quantile with upper and lower confidence intervals at that time.
#' Additionally, the number of observations beyond the 5th and 95th percentiles will be reported
#' and the binomial test P-value if this number is different than the expected 10% value.
#' @param quiet Suppress plot if `TRUE`.
#' @param legend `r template("legend")` Default is `FALSE`
#' @param log `r template("log")` Default is `TRUE`.
#' @param grid `r template("grid")` Default is `FALSE`
#' @param xlab `r template("xlab")` Default is "Time".
#' @param ylab `r template("ylab")` Default is "Output".
#' @param title `r template("title")` Default is to have no title.
#' @param xlim `r template("xlim")`
#' @param ylim `r template("ylim")`
#' @param simnum Choose which object to plot in a PM_simlist,
#' i.e., a list of [PM_sim] objects created when more than one
#' subject is included in a simultation data template and
#' `combine = F` (the default) when parsing the results of a simulation.
#' @param \dots `r template("dotsPlotly")`
#' @return Plots the simulation object.  If `obs` is included, a list will be returned with
#' the folowing items:
#' * *npc* A dataframe with three columns: quantile, prop_less, pval.
#' ** *quantile* are those specified by the `probs` argument to the plot call
#' ** *prop_less* are the proportion of simulated
#' observations at all times less than the quantile
#' ** *pval* is the P-value of the difference in the
#' prop.less and quantile by the beta-binomial test.
#' * *simsum* A dataframe with the quantile concentration at each simulated time,
#' with lower and upper confidence intervals
#' * *obs* A data frame similar to a the `$data` field of a [PM_op] object 
#' with the addition of the quantile for each observation
#' @author Michael Neely
#' @seealso [PM_sim], [plot_ly], [schema]
#' @importFrom tidyr unnest_longer
#' @importFrom dplyr summarize
#' @export
#' @examples
#' library(PmetricsData)
#' simEx$plot()
#' simEx$plot(log = FALSE, line = list(color = "orange"))
#' @family PMplots

plot.PM_sim <- function(x,
                        mult = 1,
                        ci = 0.95,
                        binSize = 0,
                        outeq = 1,
                        line = TRUE,
                        marker = FALSE,
                        obs,
                        quiet = FALSE,
                        legend = FALSE,
                        log = TRUE,
                        grid = FALSE,
                        xlab, ylab,
                        title,
                        xlim, ylim,
                        simnum, ...) {
  if (all(is.na(line))) {
    line <- list(probs = NA)
  } # standardize
  if (is.logical(line)) {
    if (line) {
      lineList <- list(
        probs = c(0.05, 0.25, 0.5, 0.75, 0.95),
        color = rep("dodgerblue", 5),
        width = rep(1, 5),
        dash = rep("solid", 5)
      ) # line = T
    } else { # line = F
      lineList <- amendLine(FALSE)
      lineList$probs <- NULL # line was FALSE, so probs = NULL -> no join lines or quantiles
      lineList$width <- 0
    }
  } else { # line was not T/F
    if (!is.list(line) & length(line) > 1) {
      stop(paste0(
        "Specify the line argument as ",
        crayon::green$bold("list(probs = c(...), ...) "),
        "not probs = c(...)."
      ))
    }
    if (!is.null(purrr::pluck(line, "probs"))) {
      lineList <- list(probs = line$probs)
    } else {
      lineList <- list(probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
    }
    nprobs <- length(lineList$probs)
    if (!is.null(purrr::pluck(line, "color"))) {
      lineList$color <- rep(line$color, nprobs)[1:nprobs]
    } else {
      lineList$color <- rep("dodgerblue", nprobs)
    }
    if (!is.null(purrr::pluck(line, "width"))) {
      lineList$width <- rep(line$width, nprobs)[1:nprobs]
    } else {
      lineList$width <- rep(1, nprobs)
    }
    if (!is.null(purrr::pluck(line, "dash"))) {
      lineList$dash <- rep(line$dash, nprobs)[1:nprobs]
    } else {
      lineList$dash <- rep("solid", nprobs)
    }
  }
  
  probValues <- lineList$probs
  probFormats <- lineList[-1] %>%
    purrr::transpose() %>%
    purrr::simplify_all()
  join <- amendLine(probFormats[[1]])
  
  # parse marker
  if (!missing(obs)) {
    if (!is.list(marker) && !marker) marker <- T
  }
  marker <- amendMarker(marker, default = list(color = "black", symbol = "circle-open", size = 8))
  
  # process dots
  layout <- amendDots(list(...))
  
  # axis labels
  xlab <- if (missing(xlab)) {
    "Time"
  } else {
    xlab
  }
  ylab <- if (missing(ylab)) {
    "Output"
  } else {
    ylab
  }
  
  layout$xaxis$title <- amendTitle(xlab)
  if (is.character(ylab)) {
    layout$yaxis$title <- amendTitle(ylab, layout$xaxis$title$font)
  } else {
    layout$yaxis$title <- amendTitle(ylab)
  }
  
  # grid
  layout$xaxis <- setGrid(layout$xaxis, grid)
  layout$yaxis <- setGrid(layout$yaxis, grid)
  
  # axis ranges
  if (!missing(xlim)) {
    layout$xaxis <- modifyList(layout$xaxis, list(range = xlim))
  }
  if (!missing(ylim)) {
    layout$yaxis <- modifyList(layout$yaxis, list(range = ylim))
  }
  
  # log y axis
  if (log) {
    layout$yaxis <- modifyList(layout$yaxis, list(type = "log"))
  }
  
  # title
  if (missing(title)) {
    title <- ""
  }
  layout$title <- amendTitle(title, default = list(size = 20))
  
  
  # legend
  legendList <- amendLegend(legend, default = list(title = list(text = "<b> Quantiles </b>")))
  layout <- modifyList(layout, list(showlegend = legendList[[1]], legend = legendList[-1]))
  
  
  # numerical check function
  NPsimInterp <- function(time, out, sim_sum, probs) {
    if (min(sim_sum$time) <= time) {
      lower_time <- max(sim_sum$time[sim_sum$time <= time], na.rm = TRUE)
    } else {
      return(NA)
    }
    if (max(sim_sum$time >= time)) {
      upper_time <- min(sim_sum$time[sim_sum$time >= time], na.rm = TRUE)
    } else {
      return(NA)
    }
    sim_quantile <- 0
    for (i in probs) {
      if (lower_time != upper_time) {
        lower_sim <- sim_sum$value[sim_sum$time == lower_time & sim_sum$quantile == i]
        upper_sim <- sim_sum$value[sim_sum$time == upper_time & sim_sum$quantile == i]
        slope <- (upper_sim - lower_sim) / (upper_time - lower_time)
        calc_sim <- lower_sim + slope * (time - lower_time)
      } else {
        calc_sim <- sim_sum$value[sim_sum$time == lower_time & sim_sum$quantile == i]
      }
      if (out >= calc_sim) {
        sim_quantile <- i
      }
    }
    return(sim_quantile)
  }
  
  if (inherits(x, "PM_simlist")) {
    if (missing(simnum)) {
      cat("Plotting first object in the PM_simlist.\nUse simnum argument to choose different PM_sim object in PM_simlist.\n")
      simout <- x[[1]]
    } else {
      simout <- x[[simnum]]
    }
  } else {
    simout <- x
  }
  
  
  if (!inherits(simout, c("PM_sim", "PMsim"))) {
    stop("Use PM_sim$run() to make object of class PM_sim.\n")
  }
  if (!missing(obs)) {
    if (!inherits(obs, c("PM_result", "PM_op"))) stop("Supply a PM_result or PM_op object for the obs argument.")
    if (inherits(obs, "PM_result")) {
      obs <- obs$op$data
    }
    if (inherits(obs, "PM_op")) {
      obs <- obs$data
    }
    obs <- obs %>%
      filter(
        outeq == !!outeq, icen == "median",
        pred.type == "post"
      ) %>%
      select(id, time, obs) # just need obs; median and post are arbitrary
  } else {
    obs <- data.frame(time = NA, obs = NA)
  }
  
  # change <=0 to NA if log plot
  if (log) {
    if (all(is.na(obs$obs))) {
      if (any(simout$obs <= 0, na.rm = TRUE)) {
        if (!quiet) {
          cat("Values <= 0 omitted from log plot.\n")
        }
        simout$obs[simout$obs <= 0] <- NA
      }
    } else {
      if (any(obs$obs <= 0, na.rm = TRUE) | any(simout$obs <= 0, na.rm = TRUE)) {
        if (!quiet) {
          cat("Values <= 0 omitted from log plot.\n")
        }
        obs$obs[obs$obs <= 0] <- NA
        simout$obs[simout$obs <= 0] <- NA
      }
    }
  }
  
  # multiply
  simout$obs$out <- simout$obs$out * mult
  obs$obs <- obs$obs * mult
  
  # simplify
  sim_out <- simout$obs[!is.na(simout$obs$out), ]
  
  # bin times if requested
  if (binSize > 0) {
    binned_sim_times <- seq(floor(min(sim_out$time, na.rm = TRUE)), ceiling(max(sim_out$time, na.rm = TRUE)), binSize)
    sim_out$time <- binned_sim_times[.bincode(sim_out$time, binned_sim_times, include.lowest = TRUE)]
    sim_out <- sim_out %>%
      group_by(id, time, outeq) %>%
      summarize(out = mean(out), .groups = "drop")
    if (!all(is.na(obs$obs))) {
      binned_obs_times <- seq(floor(min(obs$time, na.rm = TRUE)), ceiling(max(obs$time, na.rm = TRUE)), binSize)
      obs$time <- binned_obs_times[.bincode(obs$time, binned_obs_times, include.lowest = TRUE)]
      obs <- obs %>%
        group_by(id, time) %>%
        summarize(obs = mean(obs), .groups = "drop")
    }
  }
  
  nout <- max(sim_out$outeq)
  nsim <- nrow(simout$parValues)
  
  
  sim <- sim_out %>% filter(outeq == !!outeq)
  times <- sort(unique(sim$time))
  nobs <- length(times)
  
  if (!all(is.na(probValues)) & nsim >= 10) {
    # make DF of time, quantile and value
    sim_quant_df <- sim %>%
      dplyr::group_by(time) %>%
      group_map(~ quantile(.x$out, probs = probValues, na.rm = TRUE)) %>%
      dplyr::tibble() %>%
      tidyr::unnest_longer(1, indices_to = "quantile", values_to = "value") %>%
      dplyr::mutate(
        time = rep(times, each = length(probValues)),
        quantile = readr::parse_number(quantile) / 100
      ) %>%
      dplyr::select(time, quantile, value)
    
    lower_confint <- function(n) {
      l.ci <- ceiling(n * probValues - qnorm(1 - (1 - ci) / 2) * sqrt(n * probValues * (1 - probValues)))
      l.ci[l.ci == 0] <- NA
      return(l.ci)
    }
    
    upper_confint <- function(n) {
      u.ci <- ceiling(n * probValues + qnorm(1 - (1 - ci) / 2) * sqrt(n * probValues * (1 - probValues)))
      return(u.ci)
    }
    
    lconfint <- tapply(sim$out, sim$time, function(x) sort(x)[lower_confint(length(x))])
    uconfint <- tapply(sim$out, sim$time, function(x) sort(x)[upper_confint(length(x))])
    
    sim_quant_df$lowerCI <- unlist(lconfint)
    sim_quant_df$upperCI <- unlist(uconfint)
    
    # plot main data
    p <- sim_quant_df %>%
      group_by(quantile) %>%
      plotly::plot_ly(x = ~time, y = ~value)
    
    # add confidence intervals
    if (nsim < 100) {
      if (!quiet) {
        cat("\nNote: Confidence intervals for simulation quantiles omitted when nsim < 100\n")
      }
    } else {
      p <- p %>%
        plotly::add_ribbons(
          ymin = ~lowerCI, ymax = ~upperCI,
          color = I("grey"), opacity = 0.5,
          line = list(width = 0),
          hoverinfo = "none",
          showlegend = FALSE
        )
    }
    
    # add quantile lines, allowing for the independent formats
    for (i in 1:length(probValues)) {
      thisQ <- sim_quant_df %>% filter(quantile == probValues[i])
      p <- p %>% plotly::add_lines(
        x = ~time, y = ~value, data = thisQ, line = probFormats[[i]],
        hovertemplate = "Time: %{x}<br>Out: %{y}<br>Quantile: %{text}<extra></extra>",
        text = ~quantile,
        name = ~quantile
      )
    }
    retValue <- list()
    
    # add observations if supplied, and calculate NPC
    if (!all(is.na(obs))) {
      p <- p %>% add_markers(x = ~time, y = ~obs, data = obs, marker = marker)
      obs$sim_quant <- NA
      
      for (i in 1:nrow(obs)) {
        obs$sim_quant[i] <- ifelse(is.na(obs$obs[i]), NA,
                                   NPsimInterp(obs$time[i], obs$obs[i], sim_quant_df, probs = probValues)
        )
      }
      not.miss <- sum(!is.na(obs$sim_quant))
      npc <- data.frame(
        quantile = probValues,
        prop_less = rep(NA, length(probValues)),
        pval = rep(NA, length(probValues))
      )
      for (i in 1:nrow(npc)) {
        success <- sum(as.numeric(obs$sim_quant < probValues[i]), na.rm = TRUE)
        
        pval <- tryCatch(
          binom.test(success, not.miss, probValues[i],
                     alternative = "two"
          )$p.value,
          error = function(e) NA
        )
        npc$prop_less[i] <- round(success / not.miss, 3)
        npc$pval[i] <- pval
      }
      
      # calculate proportion between 0.05 and 0.95
      between <- rep(NA, nrow(obs))
      for (i in 1:nrow(obs)) {
        between[i] <- ifelse(is.na(obs$obs[i]), NA,
                             NPsimInterp(obs$time[i], obs$obs[i], sim_quant_df, probs = c(0.05, 0.95))
        )
      }
      success90 <- sum(as.numeric(between >= 0.05 & between < 0.95), na.rm = TRUE)
      attr(npc, "05-95") <- success90 / not.miss
      attr(npc, "P-90") <- binom.test(success90, not.miss, 0.9, "two")$p.value
      
      if (not.miss < nrow(obs)) {
        cat(paste("\n", nrow(obs) - not.miss, " observed values were obtained beyond the \nsimulated time range of ", min(sim_quant_df$time), " to ", max(sim_quant_df$time), " and were excluded.", sep = ""))
      }
      
      
      retValue <- modifyList(retValue, list(npc = npc, simsum = sim_quant_df, obs = obs))
      class(retValue) <- c("PMnpc", "list")
    }
  } else { # probs was set to NA or nsim < 10
    
    # plot all simulated profiles
    p <- sim %>%
      group_by(id) %>%
      plotly::plot_ly(x = ~time, y = ~out) %>%
      plotly::add_lines(line = join)
    # plot observations if available
    if (!all(is.na(obs))) {
      p <- p %>% add_markers(x = ~time, y = ~obs, data = obs, marker = marker)
    }
    retValue <- list()
  }
  
  
  # common to all plots
  p <- p %>% plotly::layout(
    xaxis = layout$xaxis,
    yaxis = layout$yaxis,
    showlegend = layout$showlegend,
    legend = layout$legend,
    title = layout$title
  )
  
  
  if (!quiet) print(p)
  retValue <- modifyList(retValue, list(p = p))
  return(invisible(retValue))
}
