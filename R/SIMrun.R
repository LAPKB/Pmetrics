#' Run the Pmetrics Simulator
#'
#' Generates randomly sampled sets of parameters from the *#PRIMARY* block of
#' a model according to a prior distribution and calculates outputs based upon
#' a template data file.
#' 
#' @details
#' The Monte Carlo simulator in Pmetrics is a powerful tool for parametric or
#' semi-parametric sampling.  There are three ways to execute the simulator.
#' * [PM_result$sim]
#' * [PM_sim$run]
#' * SIMrun
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
#' sim2 <- PM_sim$run(poppar = run1, data = newfile.csv, ...)
#' 
#' # These methods are entirely interchangeable. The first can accept a different
#' # data template. The difference is that poppar must be explicitly
#' # declared when using PM_sim$run.
#'
#' # An example of a manual prior
#' # make 1 lognormal distribution for each parameter
#' weights <- 1
#' mean <- log(c(0.7, 0.05, 100))
#' cov <- matrix(rep(0, length(mean)**2), ncol = length(mean))
#' diag(cov) <- (c(0.15, 0.15, 0.15) * mean)**2
#' # make the prior for the simulation
#' poppar <- list(weights, mean, cov)
#' # run simulation, assuming temp1.csv and model.txt are in working directory
#' SIMrun(poppar, "temp1.csv",
#'   nsim = 15, model = "model.txt", include = 1:4,
#'   obsNoise = c(0.02, 0.1, 0, 0)
#' )
#' # extract results of simulation
#' simout <- SIMparse("simout.txt")
#' }
#' @export

SIMrun <- function(poppar, limits = NULL, model, data, split,
                   include = NA, exclude = NA, nsim = 1000, predInt = 0, covariate, usePost = F,
                   seed = -17, ode = -4,
                   obsNoise, doseTimeNoise = rep(0, 4), doseNoise = rep(0, 4), obsTimeNoise = rep(0, 4),
                   makecsv, outname, clean = T, quiet = F, nocheck = F, overwrite = F, combine) {
  
  if (inherits(poppar, "PM_result")) {
    is_res <- T
    res <- poppar$clone()
    poppar <- poppar$final$clone()
  } else {
    if(inherits(poppar, "PM_final")){
      poppar <- poppar$clone()
    }
    is_res <- F
  }
  
  
  if(missing(model)){
    if (is_res) {
      model <- "simmodel.txt"
      res$model$write(model) 
      model_file_src <- F #did not use file as source
    } else {
      model <- FileExists("model.txt") #default name if model is missing
      mod_obj <- PM_model$new(model) #we have valid filename, create new PM_model
      mod_obj$write("simmodel.txt")
      model_file_src <- T #used a file as source
    }
  } else { #model is  specified
    if (inherits(model, "PM_model")) {
      model$write("simmodel.txt") #write the PM_model to "simmodel.txt" file
      model_file_src <- F #did not use a file as source
    } else { #model is a filename
      # make sure data file name is <=8 characters
      if (!FileNameOK(model)) { #function in PMutilities
        endNicely(paste("Data file name must be 8 characters or fewer.\n"), model)
      }
      #make sure it exists
      model <- FileExists(model)
      mod_obj <- PM_model$new(model) #we have valid filename, create new PM_data
      mod_obj$write("simmodel.txt")
      model_file_src <- T #used a file as source
    }
  }
  
  
  if (missing(data)) { #data is not specified as an argument
    if (is_res) {
      data <- "simdata.csv"
      res$data$write(data) #create a file named simdata.csv from PM_result$data
      data_file_src <- F #did not use a file as source
    } else {
      data <- FileExists("data.csv") #try default
      data_obj <- PM_data$new(data) #we have valid filename, create new PM_data
      data_obj$write("simdata.csv")
      data_file_src <- T #used a file as source
    }
    
  } else { #data is  specified
    if (inherits(data, "PM_data")) {
      data$write("simdata.csv") #write the PM_data to "simdata.csv" file
      data_file_src <- F #did not use a file as source
    } else { 
      if(inherits(data, "PMmatrix")){ #old format
        data_obj <- PM_data$new(data) #create new PM_data
        data_obj$write("simdata.csv")
        data_file_src <- F #did not use a file as source
      } else {
        #data is a filename
        # make sure data file name is <=8 characters
        if (!FileNameOK(data)) { #function in PMutilities
          endNicely(paste("Data file name must be 8 characters or fewer.\n"), model)
        }
        #make sure it exists
        data <- FileExists(data)
        data_obj <- PM_data$new(data) #we have valid filename, create new PM_data
        data_obj$write("simdata.csv")
        data_file_src <- T #used a file as source
      }
    }
    
  }
  
  model <- "simmodel.txt" #working name for model file
  data <- "simdata.csv" #working name for data file
  dataFile <- PM_data$new("simdata.csv")$standard_data #the data
  
  
  # set default split values when split is missing
  if (missing(split)) {
    if (inherits(poppar, "NPAG")) {
      split <- T
    } else {
      split <- F
    }
  }
  
  # number of random parameters
  if (inherits(poppar, "PM_final")) {
    npar <- nrow(poppar$popCov)
  } else {
    npar <- nrow(poppar[[3]])
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
    if(inherits(covariate$cov, "PM_result")){covariate$cov <- covariate$cov$cov$data}
    if(inherits(covariate$cov, "PM_cov")){covariate$cov <- covariate$cov$data}
    if (!inherits(covariate$cov, "PMcov")){endNicely("\nThe cov element of covariate must be a PMcov object; see ?SIMrun for help.\n", model, data)}
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
      dimnames(corMat2)[[1]] <- dimnames(corMat)[[2]] #temp fix for Katharine's issue 
      corMat <- rbind(corMat, t(corMat2))
    } else {
      corCVsub <- corCV[(nsimcov + 1):(npar + nsimcov), (1:nsimcov)]
      corMat <- cbind(corMat, corCVsub)
      corMat2 <- cbind(corCV[(1:nsimcov), (nsimcov + 1):(npar + nsimcov)], corCV[(1:nsimcov), (1:nsimcov)])
      #dimnames(corMat2)[[1]] <- dimnames(corMat)[[2]] #temp fix for Katharine's issue 
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
  
  #handle include/exclude
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
  if (!file.exists(fortSource)) {
    PMbuild()
  }
  compiler <- PMFortranConfig()
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
    if (file.exists(makecsv)) file.remove(makecsv)
    orig.makecsv <- makecsv
    makecsv <- c("1", "abcde.csv")
  }
  
  # get prior density
  getSimPrior <- function(i) {
    # get prior density
    if (inherits(poppar, "PM_final")) {
      if (split & inherits(poppar, "NPAG")) {
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
        # checkRequiredPackages("matrix")
        pop.cov <- as.matrix(Matrix::nearPD(as.matrix(pop.cov), keepDiag = T)$mat)
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
    }
    # end of block to make distribution when nsim=0
    
    else {
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
    if (sum(ptype == "r") > nrow(pop.mean)) stop("You have specified variables to be random in your model file\nthat were not random in poppar.\n")
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
      predTimes <- c(predTimes)
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
    if(!model_file_src){
      invisible(file.remove("simmodel.txt"))
    }
    if(!data_file_src){
      invisible(file.remove("simdata.csv"))
    }
  }
}
