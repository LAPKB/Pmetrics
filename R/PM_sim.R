# Use menu item Code -> Jump To... for rapid navigation
# Keyboard Option+Command+O (Mac) or Alt+O (Windows) to fold all


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
#' They return fully parsed simulator output as [PM_sim] objects in R. Details
#' can be found in the documentation for `$new` method below.
#'
#' @export
PM_sim <- R6::R6Class(
  "PM_sim",
  public = list(
    #' @field data A list of class *PM_sim* that contains the following elements. 
    #' 
    #' * **obs** Observations for all simulated templates as a data frame with these columns:
    #'    - *id* Template ID, corresponding to the ID in the template data file
    #'    - *nsim* The simulated profile number, from 1 to the value for `nsim`
    #'    specified when the simulation was run.
    #'    - *time* Time of the simulated observation.
    #'    - *out* The simulated observation.
    #'    - *outeq* The output equation number.
    #' * **amt** Compartment amounts for each simulated template as a data frame with these columns:
    #'   - *id* As for `obs`.
    #'   - *nsim* As for `obs`.
    #'   - *time* As for `obs`.
    #'   - *out* The simulated amount.
    #'   - *comp* The compartment number that contains the `out` amount.
    #' * **parValues**  A list of data frames with retained simulated parameter 
    #' values after discarding any due to truncation limits. Each data frame has columns
    #' *id* and the parameters for each simulated subject.
    #' * **totalSets** Number of all simulated parameter values needed to obtain the
    #' requested number of simulated sets within any `limits` specified.
    #' * **totalMeans** A list of vectors with the means of simulated parameter values
    #' for each subject in the template `data` file. If `usePost` is `FALSE`, which
    #' is typical, then each element in this list will be the same, since a single
    #' distribution for `poppar`, which may be multimodal and complex if `split` is `TRUE`,
    #' is used for all templates in the `data` file.
    #' This can be useful to check against the original values in `poppar`, since the
    #' mean of the `parValues` may be different due to truncation.
    #' * **totalCov** Similar to `totalMeans`, a list of covariance matrices 
    #' for simulated parameter values from each template. Again, every element
    #' of this list will be the same if `usePost` is `FALSE`, and `totalCov` can
    #' be useful as a check against the original values in `poppar`.
    data = NULL,
    
    #' @description
    #' This function simulates outputs from given inputs and a model.
    #' It can be called directly
    #' or via the `$sim` method for [PM_result] objects.
    #' @details
    #'
    #' The Monte Carlo simulator in Pmetrics generates randomly sampled sets of
    #' parameters from the *#PRIMARY* block of
    #' a model according to a prior distribution and calculates outputs based upon
    #' a template data file. It is a powerful tool for parametric or
    #' semi-parametric sampling.  There are two ways to execute the simulator.
    #'
    #' * **PM_result$sim()**
    #' * **PM_sim$new()**
    #'
    #' They return fully parsed simulator output as [PM_sim] objects in R.
    #' NPAG or IT2B final objects can easily be used as
    #' the prior distributions for sampling. Prior distributions
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
    #' Simulator output is directed to text files prefixed according to the
    #' `outfile` value (default "simout"), one for each template subject,
    #' which are read back into R to populate the [PM_sim] object.
    #' Output may also be directed to a new Pmetrics .csv data file
    #' using the `makecsv` argument.
    #'
    #' @param poppar One of four things:
    #' 
    #' * Population prior parameters as a [PM_final] object found in
    #' `PM_result$final`. Normally these would be supplied by calling the
    #' `$sim` method for a [PM_result] object, e.g. `PmetricsData::NPex$sim(...)`.
    #' * The name of a previously saved simulation via the `$save` method. The
    #' file will be loaded. This filename should have the ".rds" extension, e.g. "sim.rds".
    #' * A manually specified prior as a list containing three items in this order,
    #' but of any name: 1) vector of weights; 2) vector of mean parameter values;
    #' and 3) a covariance matrix. If only one distribution is to be specified the
    #' `weights` vector should be of length 1 and contain a 1. If multiple
    #' distributions are to be sampled, the `weights` vector should be of
    #' length equal to the number of distributions and its values should sum to 1,
    #' e.g. `c(0.25,0.05,0.7)`.  The means element may be a vector for a
    #' single distribution, or a matrix with `length(weights)` rows and
    #' number of columns equal to the number of parameters. The
    #' covariance matrix will be divided by `length(weights)` and applied to
    #' each distribution. For example:
    #' `poppar = list(wt = c(0.25, 0.75), mean = matrix(c(0.5, 1, 100, 200), cov = diag(c(0.25, 2500)))`
    #' indicates two distributions with weights 0.25 and 0.75. There are two parameters,
    #' and the means of the first distribution are 0.5 and 100. The means of the
    #' second distribution are 1 and 200. The covariance matrix has diagonal values
    #' of 0.25 and 2500 and off-diagonal values of zero.
    #'
    #' @param limits If limits are specified, each simulated parameter set that
    #' contains a value outside of the limits will be ignored and another set will
    #' be generated.  Four options exist for limits.
    #' 
    #' * The default `NULL` indicates that no limits are to be applied to simulated parameters.
    #' * The second option is to set `limits` to `NA`. This will use the
    #' parameter limits on the primary parameters that are specified in the [PM_model] object.
    #' * The third option is a numeric vector of length 1 or 2, e.g. `limits = 3` or
    #' `limits = c(0.5, 4)`, which specifies what to multiply the columns of the limits in the
    #' model file.  If length 1, then the lower limits will be the same as in the
    #' model file, and the upper limits will be multiplied by value specified.  If
    #' length 2, then the lower and upper limits will be multiplied by the
    #' specified values.  If this option is used, `poppar` must be a
    #' `PM_final` object. 
    #' * The fourth option for limits is a fully
    #' customized matrix of limits for simulated values for each parameter which
    #' will overwrite any limits in the model file.  If specified, it should be a
    #' data frame or matrix with number of rows equal to the number of random
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
    #' in the working directory. If missing, and `poppar` is a [PM_result],
    #' the model within the `$model` field of the [PM_result] object will be used.
    #' If `model` is missing and `poppar` is not a [PM_result], then
    #' Pmetrics will attempt to load a model file in the working directory
    #' called "model.txt" as the default name.
    #'
    #' @param data Either a [PM_data] object or a character vector
    #' with the file name of a Pmetrics data file in the working directory
    #' that contains template regimens and observation times.
    #' The value for outputs can be coded as any number(s) other than -99.  The
    #' number(s) will be replaced in the simulator output with the simulated
    #' values. Outputs equal to -99 will be simulated as missing. If `data` is
    #' missing, and `poppar` is a [PM_result],
    #' the data within the `$data` field of the [PM_result] object will be used.
    #' If `data` is missing and `poppar` is not a [PM_result], then
    #' Pmetrics will attempt to load a data template file in the working directory
    #' called "data.csv" as the default name.
    #'
    #' @param split Boolean operator controlling whether to split an NPAG
    #' [PM_final] object into one distribution per support point, with means
    #' equal to the vector of parameter values for that point, and covariance
    #' equal to the population covariance divided by the number of support points.
    #' Default for NPAG [PM_final] objects is `TRUE`, otherwise
    #' `FALSE`.
    #'
    #' @param include A vector of subject IDs in the `data` to iterate
    #' through, with each subject serving as the source of an independent
    #' simulation.  Default is `NA` and all subjects in the data file will be used.
    #'
    #' @param exclude A vector of subject IDs to exclude in the simulation, e.g.
    #' `exclude = c(4, 6:14, 16:20)`. Default is `NA` and
    #' all subjects in the data file will be used, i.e. none excluded.
    #' Using both `include` and `exclude` criteria may result in conflicts.
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
    #' @param covariate If you are using a [PM_result] or [PM_final] object
    #' as `poppar`, then you can also
    #' simulate with covariates. This argument is a list with the following names.
    #' * `cov` If `poppar` is a [PM_result], Pmetrics will use the `$cov` field
    #' within that object to obtain covariate information and you can skip this
    #' element of the `covariate` list. If `poppar` is
    #' a [PM_final], you will need to supply the name of a [PM_result]
    #' or [PM_cov] object as the value for this element.
    #' Pmetrics will use this covariate object to calculate the correlation
    #' matrix between all covariates and Bayesian posterior parameter values.
    #' * `mean` A named list that allows you to specify a different mean
    #' for one or more of the covariates. Each named item in the list is the name
    #' of a covariate in your data that is to have a different mean. If this
    #' argument is missing then the mean covariate values in the population will
    #' be used for simulation. The same applies to any covariates that are not
    #' named in this list.  Example:
    #' `run1 <- PM_load(1); covariate = list(mean = list(wt = 50))`.
    #' * `sd` This functions just as the mean list argument does - allowing you to
    #' specify different standard deviations for covariates in the simulation. If
    #' it, or any covariate in the sd list is missing, then the standard
    #' deviations of the covariates in the population are used. Example:
    #' `covariate = list(sd = list(wt = 10))`.
    #' * `limits` This is a bit different than the limits for population
    #' parameters above. Here,
    #' `limits` is similar to `mean` and `sd` for covariates in
    #' that it is a named list with the minimum and maximum allowable simulated
    #' values for each covariate.  If it is missing altogether, then no limits
    #' will apply.  If it is specified, then named covariates will have the
    #' indicated limits, and covariates not in the list will have limits that are
    #' the same as in the original population.  If you want some to be limited and
    #' some to be unlimited, then specify the unlimited ones as items in this list
    #' with very large ranges.  Example:
    #' `covariate = list(limits = list( wt = c(10, 70)))`.
    #' * `fix` A character vector (not a list) of covariates to fix and not simulate.  In
    #' this case values in the template data file will be used and not simulated.
    #' Example: `fix = c("wt", "age")`.  Whether you use the means and
    #' standard deviations in the population or specify your own, the covariance
    #' matrix in `poppar` will be augmented by the covariate covariances for any
    #' non-fixed covariates. The parameter plus covariate means and this augmented
    #' covariance matrix will be used for simulations. In effect, all non-fixed
    #' covariates are moved into the **#Primary** block of the model file to become
    #' parameters that are simulated. In fact, a copy of your model file is made
    #' with a "c" prepended to the model name (e.g. "model.txt" ->
    #' "c_model.txt").
    #'
    #' @param usePost Boolean, default `FALSE`. Only applicable when `poppar` contains an
    #' NPAG [PM_final] object. If `TRUE`, the mean
    #' posterior parameter values and covariances for each subject,
    #' modified by `include` or `exclude`,
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
    #    #' @param ode Ordinary Differential Equation solver log tolerance or stiffness.
    #    #' Default is -4, i.e. 0.0001.  Higher values will result in faster runs, but
    #    #' simulated concentrations may not be as accurate.
    #    
    #' @param noise A named list to add noise to any template data field. 
    #' The default is `NULL`, which means no noise will be added.
    #' The name of each element in the list should correspond to
    #' a column in the data to which you wish to add noise.
    #' Each element in the list should be another list with the following named arguments:
    #' * **.coeff** Mandatory. A vector of up to 4 coefficients for the noise model. They 
    #' correspond to C0, C1, C2, and C3 for the assay noise model (as in [PM_model]). See the 'mode' argument
    #' for details on how these values are used to generate noise. Examples: 
    #' `.coeff = c(0.1, 0.1)` or `.coeff = c(5, 0.15, -0.01, 0.003)`.
    #' * **.filter** Optional. A quoted expression to filter the data. For example, `.filter = "dose > 0.1"` or
    #' `.filter = "outeq == 1 & time < 120"`. 
    #' * **.mode** Optional. The mode of the noise. Default is `add`. Options are `add` or `exp`.
    #'    - `add` An additive noise model. The new value is generated as 
    #'    `value + noise`, where noise is a random number from a normal distribution,
    #'    with mean of 0 and `SD = C0 + C1*value + C2*value^2 + C3*value^3`, and *value* is the 
    #'    original value in each row of the target column.
    #'    - `exp` An exponential noise model. The new values is generated as 
    #'    `value * exp(noise)`, where noise is a random number from a normal distribution,
    #'    with mean of 0 and `SD = C0 + C1*value + C2*value^2 + C3*value^3`, and *value* is the 
    #'    original value in each row of the target column.
    #'  @examples
    #'  \dontrun{
    #'  exDat$makeNoise(list(dose = list(.coeff = c(0.1, 0.1), .filter = "dose > 100 & time < 200", .mode = "add"),
    #'  out = list(.coeff = c(0.01, 0.01), .mode = "exp")))
    #'  }
    #'    
    #'
    #' @param obsNoise The noise added to each simulated concentration for each
    #' output equation, where the noise is randomly drawn from a normal
    #' distribution with mean 0 and SD = C0 + C1\*conc + C2\*conc^2 + C3\*conc^3.
    #' Default values are 0 for all coefficients (i.e.) no noise. If present will
    #' override any other values in the data file or model file. Specify as a
    #' vector of length 4 times the number of output equations, e.g.
    #' `c(0.1,0.1,0,0)` for one output and `c(0.1,0.1,0,0,0.01,0.2,-0.001,0)` for two
    #' output equations. If specified as `NA`, values in the data file will
    #' be used (similar to `limits`, above).  If they are missing, values in
    #' the model file will be used. The default is `NULL`, which is no noise.
    #'
    #' @param doseTimeNoise A vector of length four to specify dose time error
    #' polynomial coefficients.  The default is `NULL`, which is no noise.
    #'
    #' @param doseNoise A vector of length four to specify dose amount error
    #' polynomial coefficients.  The default is `NULL`, which is no noise.
    #'
    #' @param obsTimeNoise A vector of length four to specify observation timing
    #' error polynomial coefficients.  The default is `NULL`, which is no noise.
    #'
    #' @param makecsv A character vector for the name of the single .csv file to be
    #' made for all simulated "subjects".  If missing, no files will be
    #' made. If a `makecsv` filename is supplied, ID numbers will
    #' be of the form nsub.nsim, e.g. 1.001 through 1.1 for the first subject,
    #' 2.001 through 2.1 for the second subject, etc. if 100 simulations are made
    #' from each subject.
    #'
    #' @param outname The name for the output file(s) without an extension.  Numbers
    #' 1 to `nsub` will be appended to the files. The default is
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
    #' @param ... Catch deprecated arguments.
    #' @return A [PM_sim] object.
    #' @author Michael Neely
    #' @examples
    #' \dontrun{
    #' # Load results of NPAG run
    #' run1 <- PM_load(1)
    #'
    #' # Two methods to simulate
    #' # The first uses the population prior, data, and model in run1, with "..."
    #' # as additional parameters passed to the simulator, e.g. limits, nsim, etc.
    #'
    #' sim1 <- run1$sim(...)
    #'
    #' # The second uses the population prior and model in run1, and a new template
    #' # data file in the working directory
    #'
    #' sim2 <- PM_sim$new(poppar = run1, data = "newfile.csv", ...)
    #'
    #' # These methods are entirely interchangeable. The first can accept a different
    #' # data template. The difference is that poppar must be explicitly
    #' # declared when using PM_sim$new. This makes it the method to use when poppar
    #' # is derived from the literature.
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
    initialize = function(poppar, limits = NULL, model, data, split = NULL,
                          include = NULL, exclude = NULL, nsim = 1000, predInt = 0,
                          covariate = NULL, usePost = FALSE,
                          seed = -17, 
                          #ode = -4,
                          noise = NULL,
                          makecsv = NULL, outname = "simout",
                          clean = TRUE, quiet = FALSE,
                          nocheck = FALSE, overwrite = FALSE,...) {
      
      # handle deprecated arguments
      extraArgs <- list(...)
      deprecated_args <- c("obsNoise", "doseTimeNoise", "doseNoise", "obsTimeNoise")
      new_args <- rep("noise", 4)
      which_deprecated <- which(deprecated_args %in% names(extraArgs))
      if (length(which_deprecated) > 0) {
        cli::cli_abort(c(
          "x" = "The following argument{?s} {?has/have} been deprecated: {paste0(deprecated_args[which_deprecated], collapse = ', ')}.",
          "i" = "Instead, use {?this/these instead}: {paste0(new_args[which_deprecated], collapse = ', ')}."
        ))
      }
      
      if (missing(poppar)) {
        cli::cli_abort(c(
          "x" = "The poppar argument is required.",
          "i" = "See ?PM_sim for help."
        ))
      }
      
      # CASE 1 - poppar is PM_result
      
      if (inherits(poppar, "PM_result")) {
        final <- poppar$final$data #PM_final_data
        if (missing(model)) {
          model <- poppar$model
        } else {
          model <- PM_model$new(model)
        }
        if (missing(data)) {
          data <- poppar$data
        } else {
          data <- PM_data$new(data)
        }
        
        # CASE 2 - poppar is PM_final
        
      } else if (inherits(poppar, "PM_final")) {
        final <- poppar$data #PM_final_data
        if (missing(model)) {
          model <- PM_model$new("model.txt")
        }
        if (missing(data)) {
          data <- PM_data$new("data.csv")
        }
        
        # CASE 3 - poppar is old PMsim
        
      } else if (inherits(poppar, "PMsim")) { # from SIMparse as single
        private$populate(poppar, type = "sim")
        return(self) # end, we have loaded a prior sim
        
        # CASE 4 - poppar is old PM_simlist  
        
      } else if (inherits(poppar, "PM_simlist")) { # from SIMparse as list
        private$populate(poppar, type = "simlist")
        return(self) # end, we have loaded a prior sim
        
        # CASE 5 - poppar is PM_sim from R6
        
      } else if (inherits(poppar, "PM_sim")) { # from R6
        private$populate(poppar, type = "R6sim")
        return(self) # end, we have loaded a prior sim
        
        # CASE 6 - poppar is manual list
        
      } else if (inherits(poppar, "list")) {
        final <- poppar # PM_final and PM_sim are lists, so needs to be after those for manual list
        if (missing(model)) {
          model <- PM_model$new("model.txt")
        } else {
          model <- PM_model$new(model)
        }
        if (missing(data)) {
          data <- PM_data$new("data.csv")
        } else {
          data <- PM_data$new(data)
        }
        # not returning because going on to simulate below
        
        # CASE 7 - poppar is filename
        
      } else { 
        if (file.exists(poppar)) {
          if (grepl("rds$", poppar, perl = TRUE)) { # poppar is rds filename
            sim <- readRDS(poppar)
          } else { # poppar is a simout.txt name
            sim <- private$SIMparse(poppar, combine = combine)
          }
          # now populate
          if (inherits(sim, c("PMsim", "PM_sim_data"))) {
            private$populate(sim, type = "sim")
          } else if (inherits(sim, "PM_simlist")) {
            private$populate(sim, type = "simlist")
          } else if (inherits(sim, "PM_sim")) {
            private$populate(sim, type = "R6sim")
          } else {
            cli::cli_abort(c("x" = "{poppar} is not a Pmetrics simulation."))
          }
          return(self) # end, we have loaded a prior sim
        } else {
          cli::cli_abort(c("x" = "{poppar} does not exist in the current working directory."))
        }
      }
      
      # If we reach this point, we are creating a new simulation
      
      # set default  values
      
      if (is.null(split)) {
        if (inherits(poppar, "NPAG")) {
          split <- TRUE
        } else {
          split <- FALSE
        }
      }
      
      if (!is.null(covariate)) {
        # check to make sure covariate argument is list (cov, mean, sd, limits, fix)
        if (!inherits(covariate, "list")) {
          cli::cli_abort(c("x" = "The covariate argument must be a list.",
                           "i" = "See ?PM_sim for help."))
          return(invisible(NULL))
        }
        # check to make sure names are correct
        covArgNames <- names(covariate)
        badNames <- which(!covArgNames %in% c("cov", "mean", "sd", "limits", "fix"))
        if (length(badNames) > 0) {
          cli::cli_abort(c("x" = "The covariate argument must be a named list.",
                           "i" = "See ?PM_sim for help."))
          return(invisible(NULL))
        }
        
        # ensure first element is correct
        if (inherits(poppar, "PM_result")) {
          covariate$cov <- poppar$cov$data
        } else if (is.null(covariate$cov)) {
          cli::cli_abort(c("x" = "When poppar is not a {.fn PM_result}, you must specify a {.fn PM_cov} or {.fn PM_result} object as the first element of the covariate list.",
                           "i"= "See ?PM_sim for help."))
          return(invisible(NULL))
        } else if (inherits(covariate$cov, "PM_result")) {
          covariate$cov <- covariate$cov$cov$data
        } else if (inherits(covariate$cov, "PM_cov")) {
          covariate$cov <- covariate$cov$data
        }
      } else { # missing covariate argument
        covariate <- NULL
      }
      
      # finally, call the simulator, which updates self$data
      
      private$SIMrun(
        poppar = final, limits = limits, model = model,
        data = data, split = split,
        include = include, exclude = exclude, nsim = nsim,
        predInt = predInt,
        covariate = covariate, usePost = usePost,
        seed = seed, ode = ode,
        noise = noise, 
        makecsv = makecsv, outname = outname, clean = clean,
        quiet = quiet,
        nocheck = nocheck, overwrite = overwrite, combine = combine
      )
      
      return(self)
      
    }, # end initialize
    #'
    #' @description
    #' `r lifecycle::badge("stable")`
    #' Save the current PM_sim object into a .rds file.
    #' @param file_name Name of the file to be created, the default is PMsim.rds
    save = function(file_name = "PMsim.rds") {
      saveRDS(self, file_name)
    },
    
    #' @description
    #' `r lifecycle::badge("stable")`
    #' Plot `PM_sim` object.
    #' @param at Index of the PM_sim object to be plotted. Default is 1.
    #' result.
    #' @param ... Arguments passed to [plot.PM_sim].
    plot = function(...) {
      
      plot.PM_sim(self$data, ...)
      
    },
    
    #' @description
    #' `r lifecycle::badge("stable")`
    #' Estimates the Probability of Target Attaintment (PTA), based on the results
    #' of the current Simulation.
    #' @param ... Additional parameters, refer to [makePTA]
    pta = function(...) {
      PM_pta$new(self, ...)
    },
    
    #' @description
    #' `r lifecycle::badge("stable")`
    #' Calculates the AUC of the specified simulation
    #' @param ... Arguments passed to [makeAUC].
    auc = function(...) {
      rlang::try_fetch(makeAUC(self$data, ...),
                       error = function(e) {
                         cli::cli_warn("Unable to generate AUC.", parent = e)
                         return(NULL)
                       }
      )
    },
    
    #' @description
    #' `r lifecycle::badge("stable")`
    #' Summarize simulation
    #' @param ... Parameters passed to [summary.PM_sim].
    summary = function(...){ 
      summary.PM_sim(self$data, ...)
    },
    
    #' @description
    #' `r lifecycle::badge("deprecated")`
    #' Deprecated method to run a simulation. Replaced by `PM_sim$new()` to be
    #' consistent with R6.
    #' @param ... Not used.
    #' @keywords internal
    run = function(...) {
      lifecycle::deprecate_warn("2.1.0", "PM_sim$run()", details = "PM_sim$run() is deprecated. Please use PM_sim$new() instead.")
    },
    
    #' @description
    #' `r lifecycle::badge("deprecated")`
    #' Deprecated method to load a prior simulation. Replaced by `PM_sim$new()` to be
    #' consistent with R6.
    #' @param ... Not used.
    #' @keywords internal
    load = function(...) {
      lifecycle::deprecate_warn("2.1.0", "PM_sim$load()", details = "PM_sim$load() is deprecated. Please use PM_sim$new() instead.")
    }
  ), # end public
  active = list(
    #' @field obs Same as `obs` element in the `data` field.
    obs = function(){
      self$data$obs
    },
    #' @field amt Same as `amt` element in the `data` field.
    amt = function(){
      self$data$amt
    },
    #' @field parValues Same as `parValues` element in the `data` field.
    parValues = function(){
      self$data$parValues
    },
    #' @field totalSets Same as `totalSets` element in the `data` field.
    totalSets = function(){
      self$data$totalSets
    },
    #' @field totalMeans Same as `totalMeans` element in the `data` field.
    totalMeans = function(){
      self$data$totalMeans
    },
    #' @field totalCov Same as `totalCov` element in the `data` field.
    totalCov = function(){
      self$data$totalCov
    }
  ), # end active
  private = list(
    # run the simulator
    SIMrun = function(poppar, limits, model, data, split,
                      include, exclude, nsim, predInt,
                      covariate, usePost,
                      seed, ode,
                      noise,
                      obsNoise, doseTimeNoise,
                      doseNoise, obsTimeNoise,
                      makecsv, outname, clean, quiet,
                      nocheck, overwrite, combine) {
      
      # DATA PROCESSING AND VALIDATION ------------------------------------------
      
      
      ###### POPPAR
      
      if (inherits(poppar, "PM_final_data")) {
        npar <- nrow(poppar$popCov)
      } else {
        npar <- nrow(poppar[[3]])
      }
      
      
      
      ###### MODEL    
      
      # get information from model
      mod_list <- model$model_list
      mod_npar <- sum(map(mod_list$pri, \(x) x$mode == "ab") %>% unlist()) # number of random parameters
      mod_nfix <- sum(map(mod_list$pri, \(x) x$mode == "constant") %>% unlist()) # number of constant parameters
      mod_nranfix <- sum(map(mod_list$pri, \(x) x$mode == "fixed") %>% unlist()) # number of random but fixed parameters
      mod_numeqt <- length(mod_list$out)
      mod_asserr <- map(mod_list$out, \(x) x$err$assay$coefficients)
      
      
      ###### DATA  
      
      # get information from data
      template <- data$standard_data
      template_ncov <- getCov(template)$ncov  #in PMutilities
      template_covnames <-  getCov(template)$covnames  
      template_numeqt <- max(template$outeq, na.rm = T)
      
      # handle include/exclude
      template <- includeExclude(template, include, exclude)
      toInclude <- unique(template$id)
      nsub <- length(toInclude)
      
      ###### TEMPLATE / MODEL CROSSCHECKS
      
      if(nsub == 0){
        cli::cli_abort(c("x" = "No subjects to simulate."))
      }
      
      if(template_numeqt != length(mod_list$out)){
        cli::cli_abort(c("x" = "Number of output equations in model and data do not match."))
      }
      
      if (!identical(tolower(template_covnames), map_chr(mod_list$cov, \(x) tolower(x$covariate)))){
        cli::cli_abort(c("x" = "Covariate names in model and data do not match."))
      }
      
      
      # SEED --------------------------------------------------------------------
      
      
      if (length(seed) < nsub) seed <- rep(seed, nsub)
      seed <- floor(seed) # ensure that seed is a vector of integers
      
      
      # PARAMETER LIMITS --------------------------------------------------------
      
      if (all(is.null(limits))) { # limits are omitted altogether
        parLimits <- NULL
        
      } else if (!any(is.na(limits)) & is.vector(limits)) { # no limit is NA and specified as vector of length 1 or 2
        # so first check to make sure poppar is a PM_final_data object
        if (!inherits(poppar, "PM_final_data")){
          cli::cli_abort(c("x" = "Error in {.arg limits}.",
                           "i" = "{.arg poppar} must be a {.fn PM_final} or {.fn PM_result} object when multiplicative limits are specified."))
        } 
        orig_lim <- poppar$ab
        if (length(limits) == 1) { # e.g. limits = 3, multiply upper...
          limits <- c(1, limits) # ...and set lower multiplier to 1
        }
        parLimits <- t(apply(orig_lim, 1, function(x) x * limits))
        
      } else if (length(limits) == 1 && is.na(limits)) { # limits specified as NA (use limits in model)
        parLimits <- matrix(c(map(mod_list$pri[1:npar], \(x) x$min) %>% unlist(),
                              map(mod_list$pri[1:npar], \(x) x$max) %>% unlist()), ncol = 2)
        
      } else if (any(is.na(limits))){ # some NAs, causes error
        cli::cli_abort(c("x" = "The {.arg limits} argument is malformed.", 
                         "i" = "It must only contain {.code NA} as a single value, e.g. {.code limits = NA} to indicate that limits in the model should be used."))
        
      } else { # limits specified as a full matrix
        parLimits <- limits
      }
      
      # parLimits should always have a value
      if (nrow(parLimits) != npar){
        cli::cli_abort(c("x" = "The number of rows in {.arg limits} must match the number of parameters in the model."))
      }
      
      # COVARIATES ----------------------------------------------------
      
      # if covariate is not null and simulating more than 1 new subject, 
      # augment prior with covariate and modify model file
      
      # 
      # THIS MIGHT NO LONGER BE A PROBLEM
      if (!is.null(covariate) && nsim > 1) {
        if (length(postToUse) > 0){
          cli::cli_abort(c("x" = "Conflicting simulation arguments.",
                           "i" = "You cannot simulate from posteriors while simulating covariates."))
        } 
        
        simWithCov <- TRUE
        
        # get mean of each covariate and Bayesian posterior parameter
        CVsum <- covariate$cov$summary(icen = "mean")
        # take out fixed covariates not to be simulated
        if (length(covariate$fix) > 0) {
          CVsum <- CVsum %>% select(.cols = -!!covariate$fix)
        }
        # remove covariates that are missing because they have all the same value
        # this also drops id and icen columns
        CVsum <- CVsum %>% select(where(~n_distinct(.) > 1)) %>% select(-id)
        
        # get correlation matrix
        corCV <- suppressWarnings(cor(CVsum))
        
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
          corMat <- rbind(corMat, corMat2)
        }
        
        # get SD of covariates
        covSD <- apply(CVsum, 2, sd, na.rm = T)[1:nsimcov]
        
        # set SDs of named variables, and use population values for others
        if (length(covariate$sd) > 0) {
          badNames <- which(!names(covariate$sd) %in% names(covSD))
          if (length(badNames) > 0) {
            cli::cli_abort(c("x" = "The {.arg sd} element of {.arg covariate} must be a list with parameter names.",
                             "i" = "See {.fn PM_sim} for help."))
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
        covMean <- apply(CVsum, 2, mean, na.rm = T)[1:nsimcov]
        
        # set means of named variables, and use population values for others
        if (length(covariate$mean) > 0) {
          badNames <- which(!names(covariate$mean) %in% names(covMean))
          if (length(badNames) > 0) {
            cli::cli_abort(c("x" = "The {.arg mean} element of {.arg covariate} must be a list with parameter names.",
                             "i" = "See {.fn PM_sim} for help."))
          }
          covMean[which(names(covMean) %in% names(covariate$mean))] <- covariate$mean
          covMean <- unlist(covMean)
        }
        
        
        meanVector <- c(poppar$popMean, covMean)
        # get the covariate limits
        # get min of original population covariates
        covMin <- apply(CVsum, 2, min, na.rm = T)[1:nsimcov]
        # and get max of original population covariates
        covMax <- apply(CVsum, 2, max, na.rm = T)[1:nsimcov]
        
        orig_covlim <- cbind(covMin, covMax)
        
        if (length(covariate$limits) == 0) {
          # limits are omitted altogether
          covLimits <- orig_covlim 
          
        } else {
          # covariate limits are supplied as named list
          badNames <- which(!names(covariate$limits) %in% names(covMean))
          if (length(badNames) > 0) {
            cli::cli_abort(c("x" = "The {.arg limit} element of {.arg covariate} must be a list with parameter names.",
                             "i" = "E.g. {.code limits = list(wt = c(40, 80), age = c(10, 50))}. See {.fn PM_sim} for help."))
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
        }
        
        limits <- rbind(parLimits, covLimits)
        
        # names of covariates to simulate
        covs2sim <- dimnames(covLimits)[[1]]
        
        
        # add simulated covariates to primary block of model object
        new_pri <- map(1:nsimcov, \(x) ab(covLimits[x, 1], covLimits[x, 2]))
        names(new_pri) <- covs2sim
        mod_list$pri <- c(mod_list$pri, new_pri)
        
        # remove them from the covariate block of model object
        model_covs <- tolower(purrr::map_chr(mod_list$cov, \(x) x$covariate))
        covs_to_remove <- which(model_covs %in% covs2sim)
        mod_list$cov <- mod_list$cov[-covs_to_remove]
        
        # also remove them from data template
        template <- template[, -which(names(template) %in% covs2sim)]
        
        
        # remake both objects
        mod_list <- PM_model$new(mod_list)$model_list
        template <- PM_data$new(template)$standard_data
        
        # remake poppar
        poppar$popMean <- meanVector
        poppar$popCov <- covMat
        
        
        limits <- rbind(parLimits, covLimits)
        
        
        # if split is true, then remake (augment) popPoints by adding mean covariate prior to each point
        if (split) {
          popPoints <- poppar$popPoints
          covToAdd <- covMean
          npoints <- nrow(popPoints)
          prob <- popPoints$prob
          covDF <- matrix(covToAdd, nrow = 1)
          covDF <- matrix(covDF[rep(1, npoints), ], ncol = length(covToAdd))
          covDF <- data.frame(covDF)
          names(covDF) <- names(covToAdd)
          popPoints <- cbind(popPoints[, 1:npar], covDF)
          popPoints$prob <- prob
          poppar$popPoints <- popPoints
        }
        
      } else {
        simWithCov <- FALSE
        limits <- parLimits
      }
      # end if (covariate) block
      
      # regardless of covariates or not, 'limits' is the final variable for 
      # limits on parameters
      
      
      # POSTERIORS ----------------------------------------------
      
      # check if simulating with the posteriors and if so, get all subject IDs
      if (usePost) {
        
        if (length(poppar$postMean) == 0){
          cli::cli_abort(c("x" = "Posterior parameters not found.",
                           "i" = "Please remake your model."))
        } else {
          
          postToUse <- unique(poppar$postMean$id) #get the id for each posterior mean
          if (split) {
            split <- FALSE
            cli::cli_inform(c("i" = "{.arg split} set to {.val FALSE} for simulations from posteriors."))
          }
          # now, filter by included IDs
          postToUse <- postToUse[postToUse %in% toInclude] # subset the posteriors if applicable
          if (length(postToUse) > 0 && length(postToUse) != nsub){
            cli::cli_abort(c("x" = "You have {length(postToUse)} posteriors and {nsub} selected subjects in the data file.  These must be equal."))
          } 
        }
      } else {
        
        postToUse <- NULL
      }      
      # NOISE -------------------------------------------------------------------
      
      if (!all(is.null(noise))){
        
        # will ignore obs noise for now but add after simulation
        if("out" %in% names(noise)){
          noise1 <- noise %>% purrr::list_assign(out = rlang::zap())
          noise2 <- noise["out"]
        } else {
          noise1 <- noise
          noise2 <- NULL
        }

        if(length(noise1)>0){
          template <- private$makeNoise(template, noise1)
        }
      } else {
        noise1 <- noise2 <- NULL
      }
      
      
      # PRED INT ----------------------------------------------------------------
      template <- if(!all(is.null(predInt))) {private$makePredInt(template, predInt)}
      
      # CALL SIMULATOR ----------------------------------------------------------------
      template <- PM_data$new(template, quiet = TRUE)
      mod <- PM_model$new(mod_list, quiet = TRUE)

      if (length(postToUse) > 0) {
        # simulating from posteriors, each posterior matched to a subject
        # need to set theta as the posterior mean or median for each subject
        # code here...
        
      } else {
        # set theta as nsim rows drawn from prior
        thisPrior <- private$getSimPrior(i = 1, 
                                         poppar = poppar, 
                                         split = split, 
                                         postToUse = NULL, 
                                         limits = limits, 
                                         seed = seed[1])
        thetas <- thisPrior$thetas %>% select(-prob) %>% as.matrix()
        sim_res <- mod$simulate_all(template, thetas)
        sim_res$.id <- template$data$id[match(sim_res$id, template$data$id)]
        sim_res <- sim_res %>% rename(comp = state_index, nsim = spp_index, amt = state) %>%
          mutate(across(c(outeq, comp, nsim), \(x) x = x + 1)) %>%
          arrange(.id, comp, nsim, time, outeq) %>%
          select(-.id)
        
        obs <- sim_res  %>%
          select(id, nsim, time, out, outeq)
        
        amt <- sim_res  %>%
          select(id, nsim, time, out = amt, comp)
        
        #add output noise if specified
        if(!all(is.null(noise2))){
          obs <- private$makeNoise(obs, noise2)
          #now need to convert amts but now way to do this in R since we
          #don't know which term is volume
     
          
        }
        
        self$data <- list(
          obs = obs,
          amt = amt,
          parValues = thisPrior$thetas %>% select(-prob),
          totalSets = thisPrior$total_nsim,
          totalMeans = thisPrior$total_means,
          totalCov = thisPrior$total_cov,
          template = template,
          model = mod)
        
        
        class(self$data) <- c("PM_sim_data", class(self$data)) # add PM_sim_data class to data
        
      }
      
      
      # MAKE CSV ----------------------------------------------------------------
      
      
      
      if (is.null(makecsv)) {
        makecsv <- 0
      } else {
        if (nsim > 50) {
          ans <- readline("Warning: creating a csv file with more than 50 simulations can take a very long time.\nDo you wish to proceed (y/n)?")
          if (ans == "n") {
            cat("\nAborting simulation...\n")
            return()
          }
        }
        if (makecsv == "simdata.csv") cli::cli_abort(c("x" = "{.var simdata.csv} is reserved. Use another name for {.var makecsv}."))
        if (file.exists(makecsv)) file.remove(makecsv)
        orig.makecsv <- makecsv
        makecsv <- c("1", "abcde.csv")
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
            parValues <- private$SIMparse(file = paste(outname, n, ".txt", sep = ""), quiet = TRUE)$parValues
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
        
        if (any(unlist(lapply(as.character(unique(temp$id)), function(x) nchar(x) > 11)))) {
          cli::cli_abort(c("x" = "Shorten all template id values to 6 characters or fewer."))
        }
        if (nsub > 1) {
          for (j in 2:nsub) {
            curTemp <- PMreadMatrix(paste("abcde", j, ".csv", sep = ""), quiet = T)
            simnum <- unlist(lapply(curTemp$id, function(x) substr(gsub("[[:space:]]", "", x), 9 - trunc, 8)))
            curTemp$id <- paste(toInclude[j], simnum, sep = "_")
            if (simWithCov) {
              curTemp <- getBackCov(curTemp, j)
            }
            if (any(unlist(lapply(as.character(unique(curTemp$id)), function(x) nchar(x) > 11)))) {
              cli::cli_abort(c("x" = "Shorten all template id values to 6 characters or fewer."))
            }
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
      
      if (clean) {
        # what files need to be removed after RUST simulation?
        to_remove <- c(
          "simmodel.txt",
          "simdata.csv"
        )
        purrr::walk(to_remove, \(x){
          tryCatch(supressWarnings(file.remove(x)), error = function(e) NULL)
        })
      }
      
      return(self)
      
    }, # end of SIMrun
    
    # get prior density
    getSimPrior = function(i, poppar, split, postToUse, limits, seed) {
      # get prior density
      if (inherits(poppar, "NPAG")) {
        if (split) {
          popPoints <- poppar$popPoints
          pop_weight <- popPoints$prob
          pop_mean <- popPoints %>% select(-prob)
          pop_cov <- poppar$popCov
          ndist <- nrow(popPoints)
        } else { # not split
          if (length(postToUse) == 0) { # not simulating from posteriors
            pop_weight <- 1
            pop_mean <- poppar$popMean
            pop_cov <- poppar$popCov
            ndist <- 1
          } else { # simulating from posteriors
            thisPost <- which(poppar$postMean$id == toInclude[i])
            pop_weight <- 1
            pop_mean <- poppar$postMean[thisPost, -1]
            pop_cov <- poppar$postCov[, , thisPost]
            ndist <- 1
          }
        }
        
      } else { # manually specified prior or from IT2B
        pop_weight <- poppar[[1]]
        ndist <- length(pop_weight)
        if (inherits(poppar[[2]], "numeric")) {
          pop_mean <- data.frame(t(poppar[[2]]))
        } else {
          pop_mean <- data.frame(poppar[[2]])
        }
        pop_cov <- data.frame(poppar[[3]])
      }
      
      # override covariance matrix to zero if nsim = 1
      if (nsim == 1) {
        pop_cov <- diag(0, nrow(pop_cov))
      }
      
      # check to make sure pop_cov (within 15 sig digits, which is in file) is pos-def and fix if necessary
      posdef <- eigen(signif(pop_cov, 15))
      if (any(posdef$values < 0)) {
        cli::cli_warn(c("!" = "Warning: your covariance matrix is not positive definite. This is typically due to small population size."))
        ans <- readline("\nChoose one of the following:\n1) end simulation\n2) fix covariance\n3) set covariances to 0\n ")
        if (ans == 1) {
          cli::cli_inform("Aborting.")
          return(invisible(NULL))
        }
        if (ans == 2) {
          # eigendecomposition to fix the matrix
          for (j in 1:5) { # try up to 5 times
            eigen_values <- eigen(pop_cov)$values
            eigen_vectors <- eigen(pop_cov)$vectors
            pop_cov <- eigen_vectors %*% diag(pmax(eigen_values, 0)) %*% t(eigen_vectors)
            posdef <- eigen(signif(pop_cov, 15))
            if (all(posdef$values >= 0)) { # success, break out of loop
              break
            }
          }
          posdef <- eigen(signif(pop_cov, 15)) # last check
          if (any(posdef$values < 0)) {
            cli::cli_abort(c("x" = "Unable to fix covariance."))
        
        }
        if (ans == 3) {
          pop_cov2 <- diag(0, nrow(pop_cov))
          diag(pop_cov2) <- diag(as.matrix(pop_cov))
          pop_cov2 <- data.frame(pop_cov2)
          names(pop_cov2) <- names(pop_cov)
          pop_cov <- pop_cov2
        }
      }
      
      if (nsim <= 1 && simWithCov) {
        # can't simulate from each point with covariate sim
        cli::cli_inform(c("i" = "You cannot simulate covariates with nsim <= 1. Each subject supplies only one set of relevant covariates."))
        simWithCov <- FALSE
      }
      
      
      # generate random samples from multivariate, multimodal normal distribution
      generate_multimodal_samples <- function(num_samples, weights, means, cov_matrix) {
        means <- split(means, 1:nrow(means))
        if (length(weights) != length(means)) {
          cli::cli_abort("Weights and means must have the same length.")
        }
        
        # Determine number of samples from each mode
        samples_per_mode <- stats::rmultinom(1, size = num_samples, prob = weights)
        
        # Generate samples for each mode
        samples <- do.call(rbind, lapply(1:length(weights), function(i) {
          tryCatch(suppressWarnings(MASS::mvrnorm(n = samples_per_mode[i], mu = as.matrix(means[[i]], nrow = 1), Sigma = cov_matrix)),
                   error = function(e) NULL )
        })) %>%
          tibble::as_tibble(.name_repair = "unique") %>%
          dplyr::mutate(prob = 1 / dplyr::n())
        
        return(samples)
      }
      
      # generate samples for theta
      
      set.seed(seed[i])
      thetas <- generate_multimodal_samples(nsim, pop_weight, pop_mean, pop_cov)
      
      # cycle through samples, moving any row with any parameter outside limits
      # into a second tibble, and replacing that row with a new sample
      
      # returns true if any parameter in row i is outside limits
      outside_check <- function(x, i) {
        any(x[i, ] - limits[, 1] < 0) | # any parameter < lower limit
          any(x[i, ] - limits[, 2] > 0) # any parameter > upper limit
      }
      discarded <- NULL
      if(!all(is.null(limits))){
        
        for (i in 1:nrow(thetas)) {
          cycle_num <- 0
          outside <- outside_check(thetas %>% dplyr::select(-prob), i)
          while (outside && cycle_num < 10) {
            new_sample <- generate_multimodal_samples(1, pop_weight, pop_mean, pop_cov)
            cycle_num <- cycle_num + 1
            outside <- outside_check(new_sample %>% dplyr::select(-prob), 1)
          }
          if (outside) {
            cli::cli_abort(c("x" = "Unable to generate simulated parameters within limits after 10 attempts per row."))
          }
          if (cycle_num > 0) {
            discarded <- rbind(discarded, thetas[i, ])
            thetas[i, ] <- new_sample
          }
          thetas$prob <- 1 / nrow(thetas)
        } # end loop to fix thetas out of range
        
      }
      
      
      total_means <- apply(rbind(thetas, discarded), 2, mean)[1:ncol(pop_cov)]
      total_cov <- bind_rows(thetas,discarded) %>% select(-prob) %>% cov()
      total_nsim <- nrow(thetas) + nrow(discarded)
      
      return(list(thetas = thetas, total_means = total_means, 
                  total_cov = total_cov, 
                  total_nsim = total_nsim))
      
    }, # end getSimPrior function'
    
    # Create new simulation objects with results of simulation
    populate = function(simout, type) {
      if (type == "sim") {
        self$obs <- simout$obs
        self$amt <- simout$amt
        self$parValues <- simout$parValues
        self$totalMeans <- simout$totalMeans
        self$totalCov <- simout$totalCov
        self$data <- simout
        class(self$data) <- c("PM_sim_data", "list")
      } else if (type == "simlist") {
        N <- length(simout) #number of templates
        nsim <- max(simout[[1]]$obs$id)
        obs <- purrr::list_rbind(map(1:N, \(x) pluck(simout, x, 1)), names_to = "id2") %>% rename(nsim = id, id = id2)
        amt <- purrr::list_rbind(map(1:N, \(x) pluck(simout, x, 2)), names_to = "id2") %>% rename(nsim = id, id = id2)
        parValues <- purrr::list_rbind(map(1:N, \(x) pluck(simout, x, 3)), names_to = "id2") %>% rename(nsim = id, id = id2)
        totalSets <- map(1:N, \(x) simout[[x]]$totalSets)
        totalMeans <- map(1:N, \(x) pluck(simout, x, 5))
        totalCov <- map(1:N, \(x) data.frame(pluck(simout, x, 6)))
        self$data <- list(
          obs = obs,
          amt = amt,
          parValues = parValues,
          totalSets = totalSets,
          totalMeans = totalMeans,
          totalCov = totalCov
        )
        
        
        
      } else if (type == "R6sim") {
        self$obs <- simout$obs
        self$amt <- simout$amt
        self$parValues <- simout$parValues
        self$totalMeans <- simout$totalMeans
        self$totalCov <- simout$totalCov
        if (inherits(simout$data, "PM_simlist")) {
          purrr::map(1:length(simout$data), \(x){
            class(simout$data[[x]]) <- c("PM_sim_data", "list") # ensure class is correct
          })
        } else {
          class(simout$data) <- c("PM_sim_data", "list") # ensure class is correct
        }
        self$data <- simout$data
      }
      return(self)
    }, # end populate
    
    makeNoise = function(template, noise){
 
      if(!is.list(noise)){
        cli::cli_warn(c("!" = "Noise arguments should be a list.",
                        "i" = "See ?PM_data for details on how to add noise."))
        return(invisible(template))
      }
      
      for(i in 1:length(noise)){
        this <- noise[[i]]
        this$.col <- names(noise)[i]
        this$.coeff <- this[[1]]
        if(is.null(this$.mode)){
          this$.mode <- "add"
        }
        
        # add zeros to coefficients if needed to make up to length 4
        if (length(this$.coeff) < 4){
          this$.coeff <- c(this$.coeff, rep(0, 4 - length(this$.coeff)))
        }
        
        # Ensure target is a column in standard_data
        if (!this$.col %in% names(template)) {
          cli::cli_abort(c("x" = "{.arg {this$.col}} is not a column in your data.",
                           "i" = "Example: {.code noise = list(dose = list(.coeff = c(0.1, 0.1)))}"))
        }
        
        # make temporary row index to preserve order later
        template$index_ <- 1:nrow(template)
        
        # Dynamically apply the filter
        if(!is.null(this$.filter)){
          filter_status <- "filtered"
          filter_exprs <- rlang::parse_expr(this$.filter)
          filtered_data <- template %>%
            filter(!!filter_exprs)
          # Keep the rest
          remaining_data <- template %>%
            filter(magrittr::not(!!!filter_exprs))
        } else {
          filter_status <- ""
          filtered_data <- template
          remaining_data <- NULL
        }
        
        # Get the target
        target_col <- filtered_data %>% select(all_of(this$.col))
        
        # Remove temp row index
        template <- template %>% select(-index_)
        
        # Add noise
        new_target <- data.frame(1:nrow(target_col))
        names(new_target) <- this$.col
        set.seed(-17)
        if(this$.mode == "add"){
          target_col <- target_col %>% mutate(noisy = ifelse(!is.na(out),
                                               out + rnorm(1, mean = 0, 
                                                           sd = this$.coeff[[1]] + 
                                                             this$.coeff[[2]] * target_col[i,] +
                                                             this$.coeff[[3]] * target_col[i,]^2 +
                                                             this$.coeff[[4]] * target_col[i,]^3),
                                               NA))                        
        } else if(this$.mode == "exp"){
          target_col <- target_col %>% mutate(noisy = ifelse(!is.na(out),
                                               out * exp(rnorm(1, mean = 0, 
                                                           sd = this$.coeff[[1]] + 
                                                             this$.coeff[[2]] * target_col[i,] +
                                                             this$.coeff[[3]] * target_col[i,]^2 +
                                                             this$.coeff[[4]] * target_col[i,]^3)),
                                               NA)) 
        } else {
          cli::cli_abort("x" = "Mode must be 'add' or 'exp'.")
        }
        
        #put back the new noisy column
        filtered_data[[this$.col]] <- target_col$noisy
        
        combined <- bind_rows(filtered_data, remaining_data) %>% arrange(index_) %>% select(-index_) 
        #Fix initial times to be 0 in case they were mutated
        combined[!duplicated(combined$id), "time"] <- 0
        
        template <- combined

      } # end for loop for each noise element
      
      return(template)
      
    }, # end makeNoise function
    
    makePredInt = function(template, predInt){
      
      predTimes <- NA
      if (is.list(predInt)) {
        # predInt is a list of (start,end,interval)
        if (any(sapply(predInt, length) != 3)) {
          cli::cli_abort(c("x" = "If a list, each element of predInt must be of the form {.code c(start, end, interval)}."))
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
            predTimes <- rep(seq(0, ceiling(max(template$time, na.rm = T)), predInt)[-1], each = numeqt)
          }
          # it was 0 so do nothing
        } else {
          # predInt is a single vector of c(start,stop,interval)
          if (length(predInt) == 3) {
            predTimes <- rep(seq(predInt[1], predInt[2], predInt[3]), each = numeqt)
          } else {
            cli::cli_abort(c(
              "x" = "{.var predInt} is misspecified.",
              "i" = "See help for {.fn PM_sim}."
            ))
          }
        }
      }
      
      # first, add temporary index to ensure id order remains the same
      dat2 <- template %>% 
        mutate(.id = dense_rank(id))
      
      # second, add predInt if necessary
      if (!is.na(predTimes[1])) {
        predTimes <- predTimes[predTimes > 0] # remove predictions at time 0
        dat3 <- dat2 %>% 
          group_by(.id) %>% 
          group_map( ~ {
            theseTimes <- predTimes[!predTimes %in% .x$time[.x$evid == 0]] # remove prediction times at times that are specified in template
            numPred <- length(theseTimes)
            newPred <- data.frame(matrix(NA, nrow = numPred, ncol = 1+ncol(.x)))
            names(newPred) <- c(".id", names(.x))
            newPred[, 1] <- .y # .id
            newPred[, 2] <- .x$id[1] # original id
            newPred[, 3] <- 0 # evid
            newPred[, 4] <- theseTimes # time
            newPred[, 10] <- 1 # out
            newPred[, 11] <- rep(1:numeqt, numPred / numeqt) # outeq
            newPred
          }) %>% bind_rows() 
        new_dat <- bind_rows(dat2, dat3) %>% 
          arrange(.id, time, outeq) %>% 
          select(-.id)
      } else { # predInt was not specified
        new_dat <- template #the original data without .id
      }
      new_dat <- new_dat %>% mutate(out = ifelse(evid == 0, -1, NA)) #replace all obs with -1 since simulating
      return(new_dat)
      
    } # end makePredInt function
  ) # end private
) # end PM_sim


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
#' @param include `r template("include")`. 
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
#'     - `fill` Fill color between quantile lines. Can be specified in several ways:
#'        * `FALSE` (the default) will not fill between lines.
#'        * `TRUE` will fill between lines with a default color of "dodgerblue", opacity 0.2.
#'        * A list with the following elements:
#'          - `color` Fill color name. Default is "dodgerblue", e.g., `fill = list(color = "red")`.
#'          - `opacity` Fill opacity. Default is 0.2 e.g., `fill = list(opacity = 0.3)`.
#'          - `probs` Vector of paired quantiles to fill between. Default is the minimum and maximum
#'          quantile specified in `probs`, or `fill = list(probs = c(0.05, 0.95))` if not specified.
#'          This can be pairs of values to fill between, e.g., `fill = list(probs = c(0.25, 0.5, 0.75, 0.95))`,
#'          which will color between 0.25 and 0.5, and again between 0.75 and 0.95.
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
#' @param ... `r template("dotsPlotly")`
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
                        include,
                        exclude,
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
    if (!is.null(purrr::pluck(line, "fill"))) {
      if (is.logical(line$fill && line$fill)) {
        line$fill <- list(color = "dodgerblue", opacity = 0.2, probs = c(0.05, 0.95))
      } else if (is.logical(line$fill && !line$fill)) {
        line$fill <- FALSE
      } else if (is.list(line$fill)) {
        
      } else {
        
      }
    }
    
    
    
    # lineList$color <- rep(line$color, nprobs)[1:nprobs]
    # } else {
    #   lineList$fill <- FALSE
    # }
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
  
  # process x
  if (inherits(x, "PM_simlist")) {
    if (missing(simnum)) {
      cli::cli_inform("Plotting first object in the PM_simlist.\nUse simnum argument to choose different PM_sim object in PM_simlist.\n")
      simout <- x[[1]]
    } else {
      simout <- x[[simnum]]
    }
  } else {
    simout <- x
  }
  
  
  
  
  
  if (!inherits(simout, c("PM_sim", "PM_sim_data", "PMsim"))) {
    cli::cli_abort(c(
      "x" = "Use {.fn PM_sim$run} to make a {.cls PM_sim} object.",
      "i" = "See help for {.fn PM_sim}."
    ))
  }
  
  # include/exclude template ids
  if (missing(include)) include <- unique(simout$obs$id)
  if (missing(exclude)) exclude <- NA
  simout$obs <- simout$obs %>% includeExclude(include, exclude)
  
  if (!missing(obs)) {
    if (!inherits(obs, c("PM_result", "PM_op"))) {
      cli::cli_abort(c("x" = "Supply a {.cls PM_result} or {.cls PM_op} object for the {.code obs} argument."))
    }
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

# SUMMARY -----------------------------------------------------------------

#' @title Summarize Pmetrics Simulation Objects
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Summarize simulation
#' @details
#' Summarizes simulated observations, compartment amounts or parameter values.
#' Can be ungrouped (i.e. the entire simulation), average values grouped by simulated id,
#' outeq, or both, as well as summary statistics for each individual.
#' Default statistics reported
#' are mean, sd, median, min, max, but could include individual quantiles. See
#' `statistics` below.
#' @method summary PM_sim
#' @param object The simulation object to summarize
#' @param at If `object` is a `PM_simlist`, which simulation to use. Default is 1.
#' @param field Quoted character value, one of
#' * **obs** for simulated observations (the default)
#' * **amt** for simulated amounts in each compartment
#' * **parValues** for simulated parameter values
#' @param group Optional quoted values to group by, e.g. `group = "outeq"` or
#' `group = c("id", "outeq")`.
#' @param statistics The summary statistics to report.
#' Default is  `c("mean", "sd", "median", "min", "max")`, but
#' can be any subset of these and can also include specific quantiles, e.g. `c(25, "median", 75)`.
#' @param digits Integer, used for number of digits to print.
#' @param ... Not used.
#' @return If `by` is omitted, a data frame with rows for each data element except ID,
#' and columns labeled according to the selected `statistics`. If `by` is specified,
#' return will be a list with named elements according to the selected `statistics`,
#' each containing a data frame with the summaries for each group in `by`.
#' @author Michael Neely
#' @examples
#' library(PmetricsData)
#' simEx$summary() # preferred
#' summary(simEx) # alternative
#' simEx$summary(at = 2, field = "amt", group = "comp") # group amounts by compartment
#' @seealso [PM_sim]
#' @export
summary.PM_sim <- function(object, include, exclude, field = "obs", group = NULL,
                           statistics = c("mean", "sd", "median", "min", "max"),
                           digits = max(3, getOption("digits") - 3), ...) {
  # get the right data
  if (inherits(object, "PM_sim")) {
    if (inherits(object$data, "PM_simlist")) {
      dat <- object$data[[at]][[field]]
    } else {
      dat <- object$data[[field]]
    }
  } else if (inherits(object, "PM_sim_data")) {
    # include/exclude template ids
    if (missing(include)) include <- unique(object[[field]]$id)
    if (missing(exclude)) exclude <- NA
    dat <- object[[field]] %>% includeExclude(include, exclude)
  } else {
    cli::cli_abort(c("x" = "Object does not appear to be a simulation."))
  }
  
  
  # loop through all the statistics
  summ <- purrr::map(
    statistics,
    \(x) {
      if (!is.na(suppressWarnings(as.numeric(x)))) {
        dplyr::summarize(dat,
                         dplyr::across(-nsim, \(y) quantile(y, probs = as.numeric(x) / 100)),
                         .by = all_of(!!group)
        )
      } else {
        dplyr::summarize(dat,
                         dplyr::across(-nsim, \(y) suppressWarnings(do.call(x, args = list(y, na.rm = TRUE)))),
                         .by = all_of(!!group)
        )
      }
    }
  )
  
  
  names(summ) <- as.character(statistics)
  if(!"id" %in% group){
    summ <- purrr::map(summ, \(x) {x$id <- NULL; x})
  }
  summ <- summ %>%
    list_rbind(names_to = "stat") %>%
    mutate(across(where(is.numeric), \(x) round(x, digits)))
  if (length(group) > 0) {
    attr(summ, "group") <- group
  }
  class(summ) <- c("summary.PM_sim", "data.frame")
  
  return(summ)
}


# PRINT SUMMARY -----------------------------------------------------------


#' @title Print Summary of Simulations
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Print a Pmetrics Observed vs. Predicted Summary Object
#'
#' @details
#' Print a summary of simulations
#' made by [summary.PM_sim]. Users do not normally need to call this print
#' function directly, as it will be the default method to display the object.
#'
#' @title Print Summary of Simulations
#' @method print summary.PM_sim
#' @param x An object made by [summary.PM_sim].
#' @param ... Not used.
#' @return A printed object.
#' @author Michael Neely
#' @seealso [summary.PM_sim]
#' @examples
#' library(PmetricsData)
#' simEx$summary()
#' @export

print.summary.PM_sim <- function(x, ...) {
  print.data.frame(x)
  if (!is.null(attr(x, "group"))) {
    cat("\n", "Grouped by:", paste(crayon::blue(attr(x, "group")), collapse = ", "), "\n")
  }
}

