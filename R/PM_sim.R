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
    #' * **parValues**  A data frame with retained simulated parameter
    #' values after discarding any due to truncation limits. The data frame has these columns:
    #'   - *id* This column is only present if `usePost = TRUE`, since in that case
    #'   the `nsim` profiles for each template are created by sampling from a different prior
    #'   joint parameter probability distribution for each template. When `usePost = FALSE`,
    #'   the same prior is used for every template, so there is no `id` column.
    #'   - *nsim* The simulation number, from 1 to the value for `nsim` specified when the simulation was run.
    #'   - a column for each random parameter in the model with the simulated values
    #' * **totalSets** When `usePost = FALSE`, the number of all simulated parameter values needed to obtain the
    #' requested number of simulated sets within any `limits` specified. When `usePost = TRUE`,
    #' a data frame with the same number for each template in the `data` file, since each template
    #' is simulated from a different prior distribution (see `parValues:id` above).
    #' * **totalMeans** If `usePost = FALSE`, this is a vector with the means of
    #' all simulated parameter values, including those discarded for being outside
    #' `limits`. If `usePost = TRUE`, this is a data frame of vectors, one for each template in the `data` file,
    #' and an `id` column to identify the template in the `data` source.
    #' This can be useful to check against the original means in `poppar`, since the
    #' mean of the `parValues` may be different due to truncation.
    #' * **totalCov** Similar to `totalMeans`, either a single covariance matrix
    #' for all simulated parameter values when `usePost = FALSE` . If `usePost = TRUE`,
    #' this is a data frame of such matrices, one for each template in the `data` file,
    #' and an `id` column to identify the template in the `data` source.
    #' Again, this can be useful as a check against the original covariance in `poppar`.
    data = NULL,
    
    #' @description
    #' This function simulates outputs from given inputs and a model.
    #' It can be called directly
    #' or via the `$sim` method for [PM_result] objects.
    #' @details
    #'
    #' The Monte Carlo simulator in Pmetrics generates randomly sampled sets of
    #' parameters from the *PRIMARY* block of
    #' a model according to a prior distribution and calculates outputs based upon
    #' a template data file. It is a powerful tool for parametric or
    #' semi-parametric sampling.  There are three ways to execute the simulator.
    #'
    #' * **PM_result$sim()**
    #' * **PM_model$sim()**
    #' * **PM_sim$new()**
    #'
    #' They return fully parsed simulator output as [PM_sim] objects in R.
    #' [PM_result] or [PM_final] objects can easily be used as
    #' the prior distributions for sampling. Prior distributions
    #' may also be manually
    #' specified, useful when simulating from literature values.  
    #' Prior distributions may be unimodal-multivariate (parametric
    #' sampling), or multimodal-multivariate (semi-parametric sampling). For 
    #' [PM_result] or [PM_final] priors, this can be accomplished with the `split` argument.
    #' For manual priors, the `weights` argument in the `poppar` list
    #' specifies the weights for each distribution.
    #'
    #' It is also possible to simulate with covariates if they are included as part
    #' of the model. By specifying a covariate list argument, Pmetrics will first
    #' calculate the correlation matrix between the covariates and if possible the Bayesian
    #' posterior parameter values for each subject in the population model.  Using
    #' either the mean and standard deviation of each covariate in the population,
    #' or a user-specified mean and/or standard deviation, Pmetrics will then
    #' calculate an augmented covariance matrix to be used in simulations.  Pmetrics
    #' will make a copy of the model file with all covariates moved into the primary
    #' block as parameters to be simulated.
    #'
    #' Noise can be applied to most columns in the data template, typically
    #' simulated observations, observation times, dose times, or dose amounts.
    #'
    #' Limits on the simulated parameter sets can also be specified using the limits
    #' on primary parameters in the model file or by specifying them manually as an
    #' argument. Limits can also be applied to simulated covariates.
    #'
    #' The same model and data structures are used for the simulator as for any
    #' other Pmetrics functions.  In this case, the data object will serve as the
    #' template for the information regarding dosing, covariate values, and
    #' observations.  Template data may have more than one subject in them, in
    #' which case the simulator will use each subject specified by the
    #' `include` argument (default is all subjects) to generate `nsim`
    #' parameter sets and corresponding observations.
    #'
    #' Simulator output is returned as a [PM_sim] object.
    #' Output may also be directed to a new Pmetrics .csv data file
    #' using the `makecsv` argument.
    #'
    #' @param poppar One of four things:
    #' 
    #' 1. A [PM_result] object containing the final population parameter
    #' distribution from a model run, a model object, and a data object. 
    #' The model object may be replaced by a different [PM_model], as
    #' long as the primary parameters are the same as the original model.
    #' The data object may also be replaced (and often is) by a different [PM_data]
    #' object compatible with the model.
    #' 
    #'     ```
    #'     run1 <- PM_load(1) # load the PM_result object
    #'     sim1 <- run1$sim(...) # replace model and data in run1 if desired; 
    #'     #must be compatible with model and data in run1
    #' 
    #'     mod <- PM_model$new("model.txt") # or use a model object
    #'     sim2 <- mod$sim(poppar = run1, data = "newdata.csv", ...) 
    #'     # poppar and data necessary, model obtained from mod
    #'     ```
    #' 2. Population prior parameters as a [PM_final] object found in
    #' `PM_result$final`. 
    #' 
    #'     ```
    #'     run1 <- PM_load(1) # load the PM_result object
    #'     sim1 <- PM_sim$new(poppar = run1$final, model = newmodel, data = newdata, ...) 
    #'     # model and data necessary
    #' 
    #'     mod <- PM_model$new("model.txt") # or use a model object
    #'     sim2 <- mod$sim(poppar = run1$final, data = "newdata.csv", ...) 
    #'     # poppar and data necessary, model obtained from mod
    #'     ```
    #' 3. The name of a previously saved simulation via the `$save` method. The
    #' file will be loaded. This filename should have the ".rds" extension, e.g. `sim1 <- PM_sim$new("sim.rds")`.
    #'
    #' 4. A manually specified prior as a list containing the following named items:
    #'     * **wt** vector of weights (probabilities) of sampling from each distribution. If missing, assumed to be 1.
    #'     * **mean** a list of mean parameter values. Each element of the list should be named with the parameter name and be a 
    #' vector of length equal to the number of distributions. See details below.
    #'     * **sd** an optional named list of overall standard deviations for each parameter, considering parameters as unimodally distributed,
    #' i.e. there should only be one value for each parameter, regardless of the number of distributions. 
    #' `sd` is only needed if a correlation matrix is specified, which will be converted to a covariance matrix. 
    #'     * **ONE** of the following matrices:
    #'        1. **cor** A square matrix of the overall correlations between parameters, again 
    #' considered as unimodally distributed, i.e. there should only be one correlation matrix regardless of the number of distributions.
    #' If a correlation matrix is specified, the `sd` element is required to calculate the covariance matrix.
    #'        2. **cov** A square matrix of the overall covariances between parameters, again
    #' considered as unimodally distributed, i.e. there should only be one covariance matrix regardless of the number of distributions.
    #' If a covariance matrix is specified, the `sd` element is unnecessary, since the diagonals of the covariance matrix are the variances
    #' or squared standard deviations.
    #' 
    #' 
    #'     If only one distribution is to be specified the
    #'     `wt` vector can be ommitted or should be `wt = 1`. If multiple
    #'     distributions are to be sampled, the `wt` vector should be of
    #'     length equal to the number of distributions in `mean` and the values of `wt` should sum to 1,
    #'     e.g. `wt = c(0.25, 0.05, 0.7)`.  The `mean` element should be a list
    #'     of elements, named for the parameters, with vectors of values equal to the number of terms in `wt`. 
    #'     If `cor` is used,
    #'     Pmetrics will use the `sd` element to calculate the covariance matrix. The
    #'     covariance matrix will be divided by the number of distributions, i.e. `length(wt)`,
    #'     and applied to each distribution.
    #' 
    #'     Examples:
    #'     * Single distribution: 
    #'     ```
    #'     poppar = list(wt = 1, 
    #'                   mean = list(ke = 0.5, v = 100), 
    #'                   cov = matrix(c(0.04, 2.4, 2.8, 400), nrow = 2))  # sd not required because cov specified 
    #'     ```
    #'     * Multiple distributions: 
    #'     ```
    #'     poppar = list(wt = c(0.1, 0.15, 0.75), # 3 distributions that sum to 1
    #'                   mean = list(ke = c(2, 0.5, 1), v = c(50, 100, 200)), # 3 values for each parameter
    #'                   sd = list(ke = 0.2, v = 20), # overall sd, ignoring multiple distributions
    #'                   cor = matrix(c(1, 0.6, 0.7, 1), nrow = 2)) # sd required because cor specified
    #'     ```
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
    #' that contains **template** regimens and observation times.
    #' The value for outputs can be coded as any number(s) other than -99.  The
    #' number(s) will be replaced in the simulator output with the simulated
    #' values. Outputs equal to -99 will be simulated as missing. If `data` is
    #' missing, and `poppar` is a [PM_result],
    #' the data within the `$data` field of the [PM_result] object will be used.
    #' If `data` is missing and `poppar` is not a [PM_result], then
    #' Pmetrics will attempt to load a data template file in the working directory
    #' called "data.csv" as the default name.
    #'
    #' @param limits If limits are specified, each simulated parameter set that
    #' contains a value outside of the limits will be ignored and another set will
    #' be generated.  Four options exist for limits.
    #'
    #' * The default `NULL` indicates that no limits are to be applied to simulated parameters.
    #' * The second option is to set `limits` to `NA`. This will use the
    #' parameter limits on the primary parameters that are specified in the [PM_model] object.
    #' * The third option is a numeric vector of length 1 or 2, e.g. `limits = 3` or
    #' `limits = c(0.5, 4)`, which specifies what to multiply the columns of the parameter limits in the
    #' model file.  If length 1, then the lower limits will be the same as in the
    #' model file, and the upper limits will be multiplied by value specified.  If
    #' length 2, then the lower and upper limits will be multiplied by the
    #' specified values.  If this option is used, `poppar` must be a
    #' `PM_final` object.
    #' * The fourth option for limits is a fully
    #' customized data frame or list of limits for simulated values for each parameter which
    #' will overwrite any limits in the model file.  If specified, it should be a
    #' data frame or list with columns or elements, respectively, "par", "min", "max".
    #' For example, use a PM_final$ab object, or a code it like
    #' `limits = list(par = c("Ka", "Ke", "V"), min = c(0.1, 0.1, 10), max = c(5, 5, 200))` or
    #' `limits = data.frame(par = c("Ka", "Ke", "V"), min = c(0.1, 0.1, 10), max = c(5, 5, 200))` or
    #' `limits = tibble::tibble(par = c("Ka", "Ke", "V"), min = c(0.1, 0.1, 10), max = c(5, 5, 200))`.
    #' Each of these specifies custom limits for 3 parameters named Ka, Ke, and V,
    #' with limits of (0.1, 5), (0.1, 5) and (10, 200), respectively.  The last example uses tibbles, the
    #' tidyverse equivalent of data frames.
    #' 
    #' Means and covariances of the total number of simulated sets will be returned to
    #' verify the simulation, but only those sets within the specified limits will
    #' be used to generate output(s) and the means and covariances of the retained
    #' sets may (and likely will be) different than those specified by
    #' `poppar`.
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
    #' @param covariate Pmetrics can simulate values for some/all covariates
    #' declared in the `cov` block of the [PM_model].  
    #' This argument is a list with the following named elements.
    #' 
    #' * **cov** Optional if `poppar` is a [PM_result] object, but required if
    #' `poppar` is a [PM_final] object or a manually specified prior, e.g., with values
    #' obtained from the literature.
    #' * **mean** Required only if `poppar` is a manually specified prior, optional otherwise.
    #' * **sd** Required only if `poppar` is a manually specified prior, optional otherwise.
    #' * **limits** Optional in all cases.
    #' * **fix** Optional in all cases.
    #' 
    #' The simplest example is when simulating covariates from a [PM_result]:  
    #' ```
    #' run1 <- PM_load(1)
    #' run1$sim(..., covariate = list())`
    #' ```
    #' 
    #' Details on each element are below.
    #' 
    #' `cov` 
    #' 
    #' This element specifies the source of the correlation matrix for 
    #' covariate values and if possible model primary parameters, i.e., those
    #' in the `pri` block of the model. 
    #' In the first two cases below, Pmetrics will use this `covariate$cov` 
    #' object to calculate the correlation
    #' matrix between all covariates and Bayesian posterior parameter values.
    #' In the third case, there is no way to calculate the correlations
    #' between parameters and covariates, so Pmetrics only calculates the covariate correlations.
    #' 
    #' * **Case 1**.  If `poppar` is a [PM_result], Pmetrics will use the `$cov` field
    #' within that object to obtain covariate means, standard deviations (sd), and 
    #' correlations among covariates and parameter values. In this case, you can omit this
    #' element of the `covariate` list. See the example above.
    #' * **Case 2**.  If `poppar` is a [PM_final], you will need to supply the name of a [PM_result]
    #' or [PM_cov] object as the value for this element so that Pmetrics can calculate covariate
    #' means, sd, and correlations.
    #' 
    #'     ```
    #'     run1 <- PM_load(1)
    #'     sim1 <- PM_sim$new(poppar = run1$final, covariate = list(cov = run1$cov), model = run1$model, data = "newdata.csv")
    #'     ```
    #' 
    #' * **Case 3**.  If `poppar` is a manually specified prior, or you wish to simulate covariates
    #' not in the original model, you must provide a
    #' covariance or correlation matrix between the covariates. In this case, it is only possible to 
    #' calculate correlations between covariates from the matrix and not between parameters and correlations,
    #' since they are unknown. The `$mean` and optionally the 
    #' `$sd` elements of the `covariate` list specified below are also required to complete the necessary
    #' information for simulation. Similar to `poppar`, if `$sd` is missing, the the `cov` object is treated as a covariance matrix,
    #' otherwise it is treated as a correlation matrix.
    #'     ```
    #'     corMat <- matrix(c(1, .98, .98, 1), nrow = 2) # correlation matrix for age and wt, for example
    #'     covariate <- list(cov = corMat, mean = list(age = 9, wt = 32), sd = list(age = 5.5, wt = 18.8)) # note the named lists for mean and sd, and cov is treated as a correlation matrix
    #' 
    #'     covMat <- matrix(c(30.25, 101.33, 101.33, 353.44), nrow = 2)
    #'     covariate <- list(cov = covMat, mean = list(age = 9, wt = 32)) # equivalent covariance matrix, and sd is not required
    #'     ```
    #' 
    #' 
    #' `mean` 
    #' 
    #' A named list that specifies the mean
    #' for one or more of the covariates in your model. If you are simulating in Case 1 or 2
    #' above, `mean` is optional and allows you to use a different mean value than was in your
    #' model-building population. For example, the population may have had a mean weight of 
    #' 30 kg, but `covariate = list(..., mean = list(wt = 70))` allows you to simulate
    #' weight with a mean of 70. If this
    #' argument is missing then the mean covariate values in the population will
    #' be used for simulation. The same applies to any covariates that are not
    #' named in this list.  
    #' 
    #' In Case 3, `mean` is required and must be a named list with the names
    #' of the covariates in the correlation matrix, and the values as the mean values for
    #' those covariates. See the example in `cov` above under Case 3.
    #' 
    #' Examples:
    #' * Using a [PM_result] as poppar: `PM_sim$new(poppar = run1, covariate = list())`.
    #' Here we don't need to specify `cov` because it is already in the [PM_result] `run1`. We are 
    #' not re-centering or otherwise modifying the covariates, so `covariate` can be an empty list.
    #' * Using a [PM_final] as poppar: `PM_sim$new(poppar = run1$final, covariate = list(cov = run1$cov, mean = list(wt = 50))`.
    #' Here we need to specify `cov` because it is not in the [PM_final] object. Futhermore, we want to recenter the 
    #' mean values, so we add the `$mean` element.
    #' * Using a manually specified covariate correlation matrix:
    #'     ```
    #'     corMat <- matrix(c(1, .98, .98, 1, nrow = 2) # correlation matrix for age and wt
    #'     covariate <- list(cov = corMat, mean = list(age = 9, wt = 32), sd = list(age = 5.5, wt = 18.8)) # mean and sd are required
    #'     PM_sim$new(poppar = poppar , covariate = covariate) # covariates will be added to poppar for simulation regardless of the source of poppar
    #'    ```
    #' 
    #' 
    #' `sd` 
    #' 
    #' This functions just as the `$mean`` argument does, but for standard deviations.
    #' 
    #' 
    #' `limits` This is a bit different than the limits for population
    #' parameters above. Here,
    #' `limits` is similar to `mean` and `sd` for covariates in
    #' that it is a named list with the minimum and maximum allowable simulated
    #' values for each covariate.  If it is missing altogether, then no limits
    #' will apply.  If it is specified, then named covariates will have the
    #' indicated limits, and covariates not in the list will have limits that are
    #' the same as in the original population.  If you want some to be limited and
    #' some to be unlimited, then specify the unlimited ones as items in this list
    #' with very large ranges. In the examples below, assume that the covariates
    #' age and wt are being simulated.
    #' * `covariate = list(..., limits = list( wt = c(10, 70)))` will limit wt to between 10 and 70 kg.
    #' Since age is also being simulated, it will have the same limits as in the population
    #' under Cases 1 and 2 above. Under Case 3, there is no population value for wt or age, so 
    #' wt will be limited and age will be unlimited.
    #' * `covariate = list(..., limits = list( wt = c(10, 70), age = c(0, 200)))` will limit wt to between 10 and 70 kg
    #' and age to between 0 and 200 years, which is effectively no limit. This would only be necessary under 
    #' Cases 1 or 2 when age was a covariate in the data and model.
    #' 
    #' 
    #' `fix` 
    #' 
    #' A character vector (not a list) of model covariates to fix and not simulate.  
    #' Values in the template data will be used and not simulated.
    #' Example: `covariate = list(..., fix = c("wt", "age"))`.  
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
    #'
    #' @param noise A named list to add noise to most template data fields,
    #' including covariates.
    #' The default is `NULL`, which means no noise will be added.
    #' The name of each element in the list should correspond to
    #' a column in the data to which you wish to add noise, typically
    #' `time`, `dose`, or `out`. Note that noise is added to the `out` column
    #' *after* simulation but *before* simulation for all other columns. Noise
    #' on the `out` column is best thought of as measurement error on the true,
    #' simulated value. Thus, in the simulated output, the `amt` values for a given
    #' template id, simulation number, time, and output equation will no longer be
    #' exactly related to the corresponding `out` value by the volume term in the model.
    #'
    #' These columns may not have noise added: `id`, `evid`, `addl`, `ii`, `input`, `outeq`, `c0`,
    #' `c1`, `c2`, and `c3`. See [PM_data] for further details on these columns.
    #'
    #' Each element in the `noise` list should be another list with the following arguments.
    #' The `coeff` argument is mandatory, and should be the first argument. It can be named or
    #' unnamed. The `filter` and `mode` arguments are optional and should always be named
    #' in the list.
    #'
    #' * **coeff** Mandatory. A vector of up to 4 coefficients for the noise model. They
    #' correspond to *C0*, *C1*, *C2*, and *C3* for the assay noise model (as in [PM_model]).
    #' See the 'mode' argument for details on how these values are used to generate noise.
    #' Examples:
    #' `noise = list(out = list(coeff = c(0.1, 0.1))` or `noise = list(dose = list(coeff = c(5, 0.15, -0.01, 0.003)))`.
    #' * **filter** Optional. A quoted expression to filter the data. For example,
    #' `noise = list(dose = list(c(0.1, 0.1), filter = "dose > 0.1"))` or
    #' `noise = list(out = list(c(0.05, 0.15), filter = "outeq == 1 & time < 120"))`.
    #' * **mode** Optional. The mode (method) of the noise. Default is `add`. Options are `add` or `exp`.
    #'    - `add` An additive noise model. The new value is generated as
    #'    `value + noise`, where noise is a random number from a normal distribution,
    #'    with mean of 0 and `SD = C0 + C1*value + C2*value^2 + C3*value^3`, and *value* is the
    #'    original value in each row of the target column.
    #'    - `exp` An exponential noise model. The new values is generated as
    #'    `value * exp(noise)`, where noise is a random number from a normal distribution,
    #'    with mean of 0 and `SD = C0 + C1*value + C2*value^2 + C3*value^3`, and *value* is the
    #'    original value in each row of the target column.
    #'    Example:
    #'     ```
    #'     exDat$makeNoise(list(dose = list(coeff = c(0.1, 0.1), filter = "dose > 100 & time < 200", mode = "add"),
    #'     out = list(c(0.1, 0.001), mode = "exp")))
    #'    ```
    #'  
    #'
    #' @param makecsv A character vector for the name of the single .csv file to be
    #' made for all simulated "subjects".  If no file extension is included, ".csv"
    #' will be added, e.g. "simout" will become "simout.csv". If an extension is included,
    #' Pmetrics will use it, e.g. "simout.ssv" will save under that name.
    #' If missing, no file will be
    #' made. If a `makecsv` filename is supplied, ID numbers will
    #' be of the form *id_nsim*, e.g. 1_1 through 1_10 through for the first
    #' data template id,
    #' 2_1 through 2_10 for the second template id, etc. if 10 simulations are made
    #' from each subject in the data template.
    #'
    #' @param quiet Boolean operator controlling whether a model summary report is
    #' given.  Default is `FALSE`.
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
    #' poppar <- list(weights = weights, mean = mean, mat = cov)
    #'
    #' # run simulation, assuming temp1.csv and model.txt are in working directory
    #'
    #' sim1 <- PM_sim$new(poppar, "temp1.csv",
    #'   nsim = 15, model = "model.txt", include = 1:4, limits = NA,
    #'   noise = list(out = list(coeff = c(0.02, 0.1, 0, 0)))
    #' )
    #' 
    #' # alternatively, load the model first
    #' 
    #' mod <- PM_model$new("model.txt")
    #' 
    #' # and then simulate
    #' 
    #' sim2 <- mod$sim(poppar = poppar, data = "temp1.csv",
    #'    nsim = 15, include = 1:4, limits = NA,
    #'    noise = list(out = list(coeff = c(0.02, 0.1, 0, 0)))
    #' )
    #' 
    #' }
    initialize = function(poppar, model, data,
      limits = NULL,
      split = NULL,
      include = NULL, exclude = NULL,
      nsim = 1000,
      predInt = 0,
      covariate = NULL,
      usePost = FALSE,
      seed = -17,
      noise = NULL,
      makecsv = NULL,
      quiet = FALSE,
      ...) {
        # handle deprecated arguments
        
        dots <- list(...)
        old_noise <- c("obsNoise", "obsTimeNoise", "doseNoise", "doseTimeNoise") %in% names(dots)
        msg <- NULL
        
        if (any(old_noise)) {
          cli::cli_abort(c(
            "x" = "{.arg {c('obsNoise', 'obsTimeNoise', 'doseNoise', 'doseTimeNoise')[old_noise]}} {?is/are} deprecated.",
            "i" = "Instead, use {.arg noise}. See {.help PM_sim}."
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
          case <- 1
          final <- poppar$final$data # PM_final_data
          msg <- c(msg, "Prior obtained from {.arg PM_result}.")
          if (missing(model)) {
            model <- poppar$model
            msg <- c(msg, "Model obtained from {.arg PM_result}.")
          } else {
            model <- PM_model$new(model, compile = FALSE) # compile later
          } 
          
          if (missing(data)) {
            data <- poppar$data
            msg <- c(msg, "Data obtained from {.arg PM_result}.")
          } else {
            data <- PM_data$new(data, quiet = quiet)
          }
          
          # CASE 2 - poppar is PM_final
        } else if (inherits(poppar, "PM_final")) {
          case <- 2
          final <- poppar$data # PM_final_data
          
          
          # CASE 3 - poppar is PM_final_data
        } else if (inherits(poppar, "PM_final_data")) {
          case <- 3
          final <- poppar # PM_final_data
          
          
          ### The following cases are for handling old simulation objects
          
          
          # CASE 4 - poppar is old PMsim
        } else if (inherits(poppar, "PMsim")) { # from SIMparse as single
          private$populate(poppar, type = "sim")
          return(self) # end, we have loaded a prior sim
          
          # CASE 5 - poppar is old PM_simlist
        } else if (inherits(poppar, "PM_simlist")) { # from SIMparse as list
          private$populate(poppar, type = "simlist")
          return(self) # end, we have loaded a prior sim
          
          # CASE 6 - poppar is PM_sim from R6
        } else if (inherits(poppar, "PM_sim")) { # from R6
          private$populate(poppar, type = "R6sim")
          return(self) # end, we have loaded a prior sim
          
          ### This creates a new simulation from a manual list
          
          # CASE 7 - poppar is manual list
        } else if (inherits(poppar, "list")) { # PM_final and PM_sim are lists, so needs to be after those for manual list
          case <- 7
          
          # parse poppar list
          poppar_elements <- names(poppar) %>% purrr::discard(~ .x == "wt")
          
          if (length(poppar_elements) == 2 && all(c("mean", "sd") %in% poppar_elements)) { # mean and SD only
            poppar$cov <- diag(poppar$sd^2) # make covariance matrix
          } else if (length(poppar_elements) == 2 && all(c("mean", "cov") %in% poppar_elements)) { # mean, covariance/correlation matrix, and weights
            poppar <- poppar # all good
          } else if (length(poppar_elements) == 3 && all(c("mean", "sd", "cor") %in% poppar_elements)) { # mean, SD, covariance/correlation matrix, and weights
            poppar$cov <- cor2cov(poppar$cor, poppar$sd) # convert correlation matrix to covariance matrix
          } else {
            cli::cli_abort(c(
              "x" = "{.arg poppar} is malformed with elements {.val {poppar_elements}}.",
              "i" = "See ?PM_sim for help on constructing {.arg poppar}."
            ))
          }
          
          # add missing wt if needed
          if (!"wt" %in% names(poppar)) {
            poppar$wt <- 1 # default weight
          } 
          
          # check to ensure wt and mean are aligned
          if (!all(map_lgl(poppar$mean, \(x) length(x) == length(poppar$wt)))) {
            cli::cli_abort(c(
              "x" = "The length of each vector in {.arg poppar$mean} must be equal to the length of {.arg poppar$wt}.",
              "i" = "See ?PM_sim for help."
            ))
          }
          
          # get final into right format
          final <- list(
            popWeight = poppar$wt,
            popMean = tibble::as_tibble(do.call(cbind, poppar$mean)),
            popCov = data.frame(poppar$cov)
          ) 
          
          # not returning because going on to simulate below
          
          ### This is for loading a saved simulation from file
          
          # CASE 8 - last option, poppar is filename
        } else {
          if (file.exists(poppar)) {
            if (grepl("rds$", poppar, perl = TRUE)) { # poppar is rds filename
              sim <- readRDS(poppar)
            } else { # poppar is a simout.txt name
              cli::cli_abort(c(
                "x" = "{poppar} is not a compatible file.",
                "i" = "Please remake your simulation."
              ))
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
        } # end if poppar is filename
        
        # If we reach this point, we are creating a new simulation
   
        # check model and data
        if(case %in% c(2, 3, 7)) { # need model and data if not from PM_result
          if (missing(model)) { model <- "model.txt" } # try the default
          if (!inherits(model, "PM_model")) {model <- PM_model$new(model, compile = FALSE)} # compile later
          
          if (missing(data)) { data <- "data.csv" } # try the default
          if (!inherits(data, "PM_data")) {data <- PM_data$new(data, quiet = quiet)} # will abort if can't make PM_data
        }
        
        
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
            cli::cli_abort(c(
              "x" = "The covariate argument must be a list.",
              "i" = "See ?PM_sim for help."
            ))
          }
          
          # check to ensure first element is correct
          if (length(covariate) == 0 || !"cov" %in% names(covariate)) {
            if (inherits(poppar, "PM_result")) { 
              covariate$cov <- poppar$cov 
              msg <- c(msg, "Covariate statistics obtained from {deparse(substitute(poppar))}}$cov}.")
            } else {
              cli::cli_abort(c(
                "x" = "The {.arg covariate$cov} argument must be supplied if `poppar` is not a {.fn PM_result} object.",
                "i" = "See ?PM_sim for help."
              ))
            }
          }
          
          
          # check to make sure names are correct
          covArgNames <- names(covariate)
          badNames <- which(!covArgNames %in% c("cov", "mean", "sd", "limits", "fix"))
          if (length(badNames) > 0) {
            cli::cli_abort(c(
              "x" = "The covariate argument must be a named list.",
              "i" = "See ?PM_sim for help."
            ))
          }
          
          
          
          # check if user specified covariate with posterior simulation?
          if (usePost) {
            cli::cli_abort(c(
              "x" = "Conflicting simulation arguments.",
              "i" = "You cannot simulate from posteriors while simulating covariates since each subject has their own covariates."
            ))
          }
          
          # check if covariate$cov is a matrix and if so, ensure $mean and $sd are present
          if (inherits(covariate$cov, "matrix")) {
            if (!("mean" %in% names(covariate))) { # covariate$cov is a matrix, with mean but no sd
              cli::cli_abort(c(
                "x" = "When {.arg covariate$cov} is a matrix, {.arg covariate$mean} must be present.",
                "i" = "See ?PM_sim for help."
              ))
            }
            
            if ("sd" %in% names(covariate)) { # covariate$cov is assumed to be a correlation matrix, since sd is present
              covariate$cov <- cor2cov(covariate$cov, covariate$sd) # convert correlation matrix to covariance matrix
              msg <- c(msg, "Covariate correlation matrix converted to covariance matrix.")
            }
            
          } 
          
          
          # OK, all checks passed, so we can proceed with covariates
          
        } # end if !is.null(covariate)
        
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
          nocheck = nocheck, overwrite = overwrite, msg = msg
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
      #' @param ... Additional parameters, refer to [PM_pta].
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
    summary = function(...) {
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
    obs = function() {
      self$data$obs
    },
    #' @field amt Same as `amt` element in the `data` field.
    amt = function() {
      self$data$amt
    },
    #' @field parValues Same as `parValues` element in the `data` field.
    parValues = function() {
      self$data$parValues
    },
    #' @field totalSets Same as `totalSets` element in the `data` field.
    totalSets = function() {
      self$data$totalSets
    },
    #' @field totalMeans Same as `totalMeans` element in the `data` field.
    totalMeans = function() {
      self$data$totalMeans
    },
    #' @field totalCov Same as `totalCov` element in the `data` field.
    totalCov = function() {
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
      makecsv, outname, clean, quiet,
      nocheck, overwrite, msg) {
        # DATA PROCESSING AND VALIDATION ------------------------------------------
        
        
        ###### POPPAR
        
        npar <- nrow(poppar$popCov)
        
        
        ###### MODEL
        
        # get information from model
        arg_list <- model$arg_list
        mod_list <- model$model_list
        mod_npar <- length(mod_list$parameters) # number of random parameters
        # mod_nfix <- sum(map(mod_list$pri, \(x) x$mode == "constant") %>% unlist()) # number of constant parameters
        # mod_nranfix <- sum(map(mod_list$pri, \(x) x$mode == "fixed") %>% unlist()) # number of random but fixed parameters
        mod_numeqt <- mod_list$n_out
        mod_asserr <- map(mod_list$err, \(x) x$coeff)
        
        
        ###### DATA
        
        # get information from data
        template <- data$standard_data
        
        template_ncov <- getCov(template)$ncov # in PMutilities
        template_covnames <- getCov(template)$covnames
        template_numeqt <- max(template$outeq, na.rm = T)
        
        # handle include/exclude
        template <- includeExclude(template, include, exclude)
        toInclude <- unique(template$id)
        nsub <- length(toInclude)
        
        ###### TEMPLATE / MODEL CROSSCHECKS
        
        if (nsub == 0) {
          cli::cli_abort(c("x" = "No subjects to simulate."))
        }
        
        if (template_numeqt != mod_numeqt) {
          cli::cli_abort(c("x" = "Number of output equations in model and data do not match."))
        }
        
        # if (!identical(sort(template_covnames), sort(mod_list$cov))) {
        #   cli::cli_abort(c("x" = "Covariate names in model and data do not match."))
        # }
        
        # POSTERIORS ----------------------------------------------
        
        # check if simulating with the posteriors and if so, get all subject IDs
        if (usePost) {
          if (length(poppar$postMean) == 0) {
            cli::cli_abort(c(
              "x" = "Posterior parameters not found.",
              "i" = "Please remake your model."
            ))
          } else {
            postToUse <- unique(poppar$postMean$id) # get the id for each posterior mean
            if (split) {
              split <- FALSE
              cli::cli_inform(c("i" = "{.arg split} set to {.val FALSE} for simulations from posteriors."))
            }
            # now, filter by included IDs
            postToUse <- postToUse[postToUse %in% toInclude] # subset the posteriors if applicable
            if (length(postToUse) > 0 && length(postToUse) != nsub) {
              cli::cli_abort(c("x" = "You have {length(postToUse)} posteriors and {nsub} selected subjects in the data file.  These must be equal."))
            }
          }
        } else {
          postToUse <- NULL
        }
        
        
        # SEED --------------------------------------------------------------------
        
        
        seed <- floor(seed) # ensure that seed is a vector of integers
        
        
        # PARAMETER LIMITS --------------------------------------------------------
        
        if (all(is.null(limits))) { # limits are omitted altogether
          parLimits <- tibble::tibble(par = 1:npar , min = rep(-Inf, npar), max = rep(Inf, npar))
        } else if (!any(is.na(limits)) & is.vector(limits)) { # no limit is NA and specified as vector of length 1 or 2
          # so first check to make sure poppar is a PM_final_data object
          if (!inherits(poppar, "PM_final_data")) {
            cli::cli_abort(c(
              "x" = "Error in {.arg limits}.",
              "i" = "{.arg poppar} must be a {.fn PM_final} or {.fn PM_result} object when multiplicative limits are specified."
            ))
          }
          orig_lim <- poppar$ab
          if (length(limits) == 1) { # e.g. limits = 3, multiply upper...
            limits <- c(1, limits) # ...and set lower multiplier to 1
          }
          parLimits <- orig_lim %>% mutate(
            min = min * limits[1],
            max = max * limits[2]
          )
        } else if (length(limits) == 1 && is.na(limits)) { # limits specified as NA (use limits in model)
          parLimits <- poppar$ab
        } else if (any(is.na(limits))) { # some NAs, causes error
          cli::cli_abort(c(
            "x" = "The {.arg limits} argument is malformed.",
            "i" = "It must only contain {.code NA} as a single value, e.g. {.code limits = NA} to indicate that limits in the model should be used."
          ))
        } else { # limits specified as a list or data frame
          parLimits <- tibble::as_tibble(limits)
          if (!all(c("par", "min", "max") %in% names(parLimits))) {
            cli::cli_abort(c("x" = "A manual {.arg limits} argument must be a data frame or list with elements {.val par}, {.val min}, and {.val max}."))
          }
        }
        
        if (!is.null(parLimits) && nrow(parLimits) != npar) {
          cli::cli_abort(c("x" = "The number of rows in {.arg limits} must match the number of parameters in the model."))
        }
        
        
        
        
        # COVARIATES ----------------------------------------------------
        
        # if covariate is not null and simulating more than 1 new subject,
        # augment prior with covariate correlations and modify model file
        
        simWithCov <- FALSE # default is no covariates
        
        if(!is.null(covariate)) {
          simWithCov <- TRUE
          
          # get mean of each covariate and Bayesian posterior parameter
          if(is.matrix(covariate$cov)){ # simulating covariates not in the data
            if(inherits(poppar, "PM_final_data")) {
              pars <- poppar$postMean # get the parameter values
            } else { # manual poppar prior, so simulate arbitrary number of parameters
              
              poppar <- poppar %>% purrr::set_names(c("wt", "popMean", "popCov")) %>% c(list(popCor = cov2cor(.$popCov))) # keeps it consistent with PM_final_data
              
              weights <- poppar$wt
              means <- bind_rows(poppar$popMean)
              cov_matrix <- pos_def(poppar$popCov) # ensure covariance matrix is positive definite, function in PMutilities
              if (length(cov_matrix) == 1 && cov_matrix == 1){
                return(invisible(NULL)) #quietly abort simulation
              } else if (length(cov_matrix) == 1 && cov_matrix == -1){
                cli::cli_abort(c(
                  "x" = "Population parameter covariance matrix cannot be made positive definite.",
                  "i" = "Please check your covariance matrix."
                ))
              }
              samples_per_mode <- stats::rmultinom(1, size = 50, prob = weights)
              
              # Generate samples for each mode
              pars <- map(1:length(weights), \(i) {
                tryCatch(suppressWarnings(TruncatedNormal::rtmvnorm(n = samples_per_mode[i], mean = means[i,], sigma = cov_matrix, lb = rep(0, ncol(means[i,])))),
                error = function(e) NULL
              ) %>% as.data.frame()
            }) %>%
            list_rbind() %>%
            rlang::set_names(names(means)) 
            
          }
          # Generate samples for each covariate
          means <- covariate$mean
          cov_matrix <- pos_def(covariate$cov) # ensure covariance matrix is positive definite, function in PMutilities
          if (length(cov_matrix) == 1 && cov_matrix == 1){
            return(invisible(NULL)) #quietly abort simulation
          } else if (length(cov_matrix) == 1 && cov_matrix == -1){
            cli::cli_abort(c(
              "x" = "Covariate parameter covariance matrix cannot be made positive definite.",
              "i" = "Please check your covariance matrix."
            ))
          }
          samples <- tryCatch(TruncatedNormal::rtmvnorm(n = nrow(pars), mean = means, sigma = cov_matrix, lb = rep(0, length(means))), error = function(e) NULL) %>%
          tibble::as_tibble(.name_repair = "minimal") %>%
          rlang::set_names(names(means)) 
          
          # in either case, combine the pars and covariates and proceed
          CVsum <- bind_cols(pars, samples, .name_repair = "minimal") %>% mutate(icen = "mean")
          
        } else { # we had a PM_final_data as covariate$cov
          CVsum <- covariate$cov$summary(icen = "mean")
        }
        
        # take out fixed covariates not to be simulated
        if (length(covariate$fix) > 0) {
          CVsum <- CVsum %>% select(.cols = -!!covariate$fix)
        }
        # remove covariates that are missing because they have all the same value
        # this also drops id and icen columns
        CVsum <- CVsum %>%
        select(where(~ dplyr::n_distinct(.) > 1)) %>%
        select(-id)
        
        # get correlation matrix
        corCV <- suppressWarnings(cor(CVsum))
        
        nsimcov <- ncol(corCV) - npar
        
        # augment poppar correlation matrix
        bind_bottom_right <- function(A, B, n) {
          m <- nrow(A)
          if (nrow(B) != m + n || ncol(B) != m + n)
          stop("B must be of size (m + n) x (m + n)")
          
          # Extract blocks from B
          B_right  <- B[1:m,   (m+1):(m+n), drop = FALSE]  # top-right block
          B_bottom <- B[(m+1):(m+n), 1:m,   drop = FALSE]  # bottom-left block
          B_corner <- B[(m+1):(m+n), (m+1):(m+n), drop = FALSE]  # bottom-right block
          
          # Assemble full matrix
          top    <- cbind(A, B_right)
          bottom <- cbind(B_bottom, B_corner)
          rbind(top, bottom)
        }
        
        corMat <- bind_bottom_right(
          as.matrix(poppar$popCor), 
          as.matrix(corCV), 
          n = nsimcov
        )  
        
        
        # get SD of covariates
        covSD <- CVsum %>% summarize(across(last_col(offset = nsimcov - 1):last_col(), sd, na.rm = TRUE))
        
        # grab their names
        covs2sim <- names(covSD)
        
        # set SDs of named variables, and use population values for others
        if (length(covariate$sd) > 0) {
          badNames <- which(!names(covariate$sd) %in% names(covSD))
          if (length(badNames) > 0) {
            cli::cli_abort(c(
              "x" = "The {.arg sd} element of {.arg covariate} must be a list with parameter names.",
              "i" = "See {.fn PM_sim} for help."
            ))
          }
          covSD[which(names(covSD) %in% names(covariate$sd))] <- covariate$sd
          covSD <- unlist(covSD)
        }
        # augmented correlation matrix to covariance
        covMat <- cor2cov(corMat, unlist(c(poppar$popSD, covSD[1:nsimcov])))
        dimnames(covMat) <- dimnames(corMat)
        
        # get means of covariates
        covMean <- CVsum %>% summarize(across(covs2sim, mean, na.rm = TRUE))
        
        # set means of named variables, and use population values for others
        if (length(covariate$mean) > 0) {
          badNames <- which(!names(covariate$mean) %in% names(covMean))
          if (length(badNames) > 0) {
            cli::cli_abort(c(
              "x" = "The {.arg mean} element of {.arg covariate} must be a list with parameter names.",
              "i" = "See {.fn PM_sim} for help."
            ))
          }
          covMean[which(names(covMean) %in% names(covariate$mean))] <- covariate$mean
          covMean <- unlist(covMean)
        }
        
        
        meanVector <-  poppar$popMean %>% tibble::add_column(!!!as.list(covMean))
        # get the covariate limits
        # get min of original population covariates
        covMin <- CVsum %>% summarize(across(covs2sim, min, na.rm = TRUE))
        # and get max of original population covariates
        covMax <- CVsum %>% summarize(across(covs2sim, max, na.rm = TRUE))
        
        orig_covlim <- tibble::tibble(par = covs2sim, min = unlist(covMin), max = unlist(covMax))
        covLimits <- orig_covlim
        if (length(covariate$limits) > 0) {
          
          # covariate limits are supplied as named list
          badNames <- which(!names(covariate$limits) %in% names(covMean))
          if (length(badNames) > 0) {
            cli::cli_abort(c(
              "x" = "The {.arg limit} element of {.arg covariate} must be a list with parameter names.",
              "i" = "E.g. {.code limits = list(wt = c(40, 80), age = c(10, 50))}. See {.fn PM_sim} for help."
            ))
          }
      
          #  figure out which covariates have different limits and change them
          
          covUpdates <- tibble::enframe(covariate$limits, name = "par", value = "rng") %>%
          tidyr::unnest_wider(rng, names_sep = "") %>%
          dplyr::rename(min = rng1, max = rng2)
      
          covLimits <- dplyr::rows_update(covLimits, covUpdates, by = "par")
          
          # goodNames <- which(names(covMean) %in% names(covariate$limits))
          # if (length(goodNames) > 0) {
          #   covLimits[goodNames, ] <- t(sapply(1:length(goodNames), function(x) {
          #     covariate$limits[[x]]
          #   }))
          # }
        }
        # dimnames(covLimits) <- list(covs2sim, c("lower", "upper"))
        # covLimits <- data.frame(covLimits)
        
        
        limits <- rbind(parLimits, covLimits)
        
        # add simulated covariates to primary block of model object
        new_pri <- map(1:nsimcov, \(x) ab(covLimits$min[x], covLimits$max[x]))
        names(new_pri) <- covs2sim
        arg_list$pri <- c(arg_list$pri, new_pri)
        
        # remove them from the covariate block of model object
        
        model_covs <- mod_list$covariates
        covs_to_remove <- which(model_covs %in% covs2sim)
        arg_list$cov <- arg_list$cov[-covs_to_remove]
        if (length(arg_list$cov) == 0) {
          arg_list$cov <- NULL # remove covariates if none left
        }
        
        # also remove them from data template
        template <- template[, -which(names(template) %in% covs2sim)]
        
        
        # remake both objects
        
        arg_list <- PM_model$new(arg_list, compile = FALSE)$arg_list # compile later
        template <- PM_data$new(template, quiet = TRUE)$standard_data
        
        
        # remake poppar
        poppar$popMean <- meanVector
        poppar$popCov <- covMat
        
        
        
        
        # if split is true, then remake (augment) popPoints by adding mean covariate prior to each point
        if (split) {
          
          add_vector_columns <- function(df, v) {
            v_df <- as_tibble(as.list(v))  # convert named vector to one-row tibble
            df %>% bind_cols(v_df[rep(1, nrow(df)), ])  # replicate the row to match df
          }
          
          poppar$popPoints <- poppar$popPoints %>% add_vector_columns(covMean) %>% select(-prob, everything(), prob)
          
        }
      } else {
        simWithCov <- FALSE
        limits <- parLimits
      } # end if (covariate) block
      
      
      # regardless of covariates or not, 'limits' is the final variable for
      # limits on parameters
      
      
      
      # NOISE -------------------------------------------------------------------
      
      if (!all(is.null(noise))) {
        # will ignore obs noise for now but add after simulation
        if ("out" %in% names(noise)) {
          noise1 <- noise %>% purrr::list_assign(out = rlang::zap())
          noise2 <- noise["out"]
        } else {
          noise1 <- noise
          noise2 <- NULL
        }
        
        if (length(noise1) > 0) {
          template <- private$makeNoise(template, noise1)
        }
      } else {
        noise1 <- noise2 <- NULL
      }
      
      
      # PRED INT ----------------------------------------------------------------
      
      template <- if (!all(is.null(predInt))) {
        private$makePredInt(template, predInt)
      }
      
      # CALL SIMULATOR ----------------------------------------------------------------
      
      
      template <- PM_data$new(template, quiet = TRUE)
      mod <- PM_model$new(arg_list) # now we compile
      
      if (length(postToUse) > 0) {
        # simulating from posteriors, each posterior matched to a subject
        # need to set theta as the posterior mean or median for each subject
        ans <- NULL
        data_list <- list()
        for(i in 1:nsub){
          # get the prior for this subject
          thisPrior <- private$getSimPrior(
            i = i,
            poppar = poppar,
            split = split,
            postToUse = postToUse[i],
            limits = limits,
            seed = seed[1],
            nsim = nsim,
            toInclude = toInclude, msg = msg
          )
          # get the template for this subject
          sub_template <- PM_data$new(template$standard_data %>% filter(id == toInclude[i]), quiet = TRUE)
          
          # add the simulated values to the list
          data_list <- append(data_list, list(private$getSim(thisPrior, sub_template, mod, noise2, msg = msg)))
          ans <- thisPrior$ans
        }
        
        # combine the output
        obs <- purrr::list_rbind(map(data_list, \(x) x$obs))
        amt <- purrr::list_rbind(map(data_list, \(x) x$amt))
        
        parValues <- purrr::list_rbind(map(data_list, \(x) x$parValues)) %>%
        mutate(id = rep(toInclude, each = !!nsim), nsim = rep(1:!!nsim, nsub)) %>%
        relocate(id, nsim)
        total_means <- dplyr::bind_rows(map(data_list, \(x) x$totalMeans)) %>%
        mutate(id = toInclude) %>%
        relocate(id)
        total_cov <- dplyr::bind_rows(map(data_list, \(x) data.frame(x$totalCov, row.names = NULL))) %>%
        mutate(
          id = rep(toInclude, each = npar),
          par = rep(names(poppar$popMean), !!nsub)
        ) %>%
        relocate(id, par)
        total_nsim <- tibble::tibble(id = toInclude, n = purrr::map_dbl(data_list, \(x) x$totalSets))
        
        ret <- list(
          obs = obs,
          amt = amt,
          parValues = parValues,
          totalSets = total_nsim,
          totalMeans = total_means,
          totalCov = total_cov,
          template = template,
          model = mod
        )
        
        
        class(ret) <- c("PM_sim_data", "list")
        self$data <- ret
        
      } else { # postToUse is false
        
        # set theta as nsim rows drawn from prior
        thisPrior <- private$getSimPrior(
          i = 1,
          poppar = poppar,
          split = split,
          postToUse = NULL,
          limits = limits,
          seed = seed[1],
          nsim = nsim,
          toInclude = toInclude,
          msg = msg
        )
        
        self$data <- private$getSim(thisPrior, template, mod, noise2, msg = msg)
      }
      
      
      # MAKE CSV ----------------------------------------------------------------
      
      if (!is.null(makecsv)) {
        if (nsub * nsim > 100) {
          # cli_ask is in PMutilities
          ans <- cli_ask("Creating a csv file with {nsub} templates * {nsim} simulations/template = {nsub * nsim} subjects can take a very long time. Do you wish to proceed (y/n)?")
          if (tolower(ans) == "n") {
            cat("\nAborting simulation...\n")
            return()
          }
        }
        
        if (file.exists(makecsv)) {
          file.remove(makecsv)
        }
        
        # cycle through template and nsims
        
        csv <- list()
        for (i in unique(template$standard_data$id)) {
          this_template <- template$standard_data %>% filter(id == i)
          for (j in 1:nsim) {
            this_sim <- self$obs %>% filter(id == i, nsim == j)
            this_template$out[this_template$evid == 0] <- this_sim$out
            this_template$id <- paste(i, j, sep = "_")
            if(simWithCov){ #add back simulated covariate values
              
              this_template <- this_template %>%
              mutate(!!!set_names(self$parValues %>% select(!!covs2sim) %>% slice(j), covs2sim))
            }
            csv <- append(csv, list(this_template))
          }
        }
        
        csv <- PM_data$new(list_rbind(csv), quiet = TRUE)
        
        if (!stringr::str_detect(makecsv, "\\..{3}$")) {
          makecsv <- paste0(makecsv, ".csv")
        }
        csv$save(makecsv)
        
        cli::cli_inform("The file {.file {makecsv}} was saved in {getwd()}.")
      }
      
      
      # FINAL RETURN ------------------------------------------------------------
      if (length(msg) > 0) {
        cli::cli_alert_info("Simulation messages:")
        purrr::walk(msg, \(m) cli::cli_bullets(c("*" = m)))
        return(invisible(NULL))
      }
      return(self)
      
    }, # end of SIMrun
    
    # get prior density
    getSimPrior = function(i, poppar, split, postToUse, limits, seed, nsim, toInclude, msg = NULL) {
      # get prior density
      
      
      
      
      if (inherits(poppar, "NPAG")) {
        
        if(nsim == 0){ # simulate each support point once
          thetas  <- poppar$popPoints %>% mutate(prob = 1/n())
          total_means <- poppar$popMean
          total_cov <- poppar$popCov
          total_nsim <- 0
          
          
          return(list(
            thetas = thetas, total_means = total_means,
            total_cov = total_cov,
            total_nsim = total_nsim
          ))
          
        }
        
        if (nsim < 2*nrow(poppar$popPoints)){
          split <- FALSE
          msg <- c(msg, " {.arg split} set to {.code FALSE} for {.code nsim} less than 2 * number of support points.")
        }
        
        if (split) {
          popPoints <- poppar$popPoints
          pop_weight <- popPoints$prob
          pop_mean <- popPoints %>% select(-prob)
          ndist <- nrow(popPoints)
          pop_cov <- poppar$popCov / ndist
        } else { # not split
          if (is.null(postToUse)) { # not simulating from posteriors
            pop_weight <- 1
            pop_mean <- poppar$popMean
            pop_cov <- poppar$popCov
            ndist <- 1
          } else { # simulating from posteriors
            pop_weight <- 1
            pop_mean <- poppar$postMean[postToUse, ] %>% select(-id)
            pop_cov <- poppar$postCov[[postToUse]]
            ndist <- 1
          }
        }
      } else { # manually specified prior 
        pop_weight <- poppar$popWeight
        ndist <- length(pop_weight)
        if (nsim < 2 * ndist) {
          cli::cli_abort(c("x" = "The {.arg nsim} argument must be at least twice the number of modes in the prior."))
        }
        pop_mean <- poppar$popMean
        pop_cov <- poppar$popCov / ndist
      }
      
      # override covariance matrix to zero if nsim = 1
      if (nsim == 1) {
        pop_cov <- diag(0, nrow(pop_cov))
      }
      
      pop_cov <- pos_def(pop_cov) # pos_def is in PMutilities
      if (length(pop_cov)==1 && pop_cov == 1){
        return(invisible(NULL)) #quietly abort simulation
      } else if (length(pop_cov)==1 && pop_cov == -1){
        msg <- if (!is.null(postToUse)) {glue::glue("Unable to fix covariance for template {.code id = {toInclude[i]}}.")} else {"Unable to make population covariance positive definite."}
        cli::cli_abort(c(
          "x" = msg,
          "i" = "Please check your data and covariance matrix."
        ))
      }
      
      # generate samples for theta
      set.seed(seed)
      thetas <- generate_multimodal_samples(nsim, pop_weight, pop_mean, pop_cov, toInclude[i], limits)
      
      
      return(thetas)
      
    }, # end getSimPrior function'
    
    # call simulator and process results
    getSim = function(thisPrior, template, mod, noise2, msg = NULL) {
      
      thetas <- thisPrior$thetas %>%
      select(-prob) %>%
      as.matrix()
      mod$compile() # check if compiled and if not, do so
      sim_res <- mod$sim(template, thetas)
      sim_res$.id <- template$standard_data$id[match(sim_res$id, template$standard_data$id)]
      sim_res <- sim_res %>%
      rename(comp = state_index, nsim = spp_index, amt = state) %>%
      mutate(across(c(outeq, comp, nsim), \(x) x <- x + 1)) %>%
      arrange(.id, comp, nsim, time, outeq) %>%
      select(-.id)
      
      obs <- sim_res %>% filter(comp == 1) %>% # obs are duplicated in every compartment
      select(id, nsim, time, out, outeq)
      
      amt <- sim_res %>%
      select(id, nsim, time, out = amt, comp)
      
      # add output noise if specified
      if (!all(is.null(noise2))) {
        obs <- private$makeNoise(obs, noise2)
      }
      
      ret <- list(
        obs = obs,
        amt = amt,
        parValues = thisPrior$thetas %>% select(-prob) %>%
        mutate(nsim = 1:n()) %>% relocate(nsim),
        totalSets = thisPrior$total_nsim,
        totalMeans = thisPrior$total_means,
        totalCov = thisPrior$total_cov,
        template = template,
        model = mod
      )
      
      
      class(ret) <- c("PM_sim_data", "list") # add PM_sim_data class to data
      return(ret)
    }, # end .sim function
    
    # Create new simulation objects with results of simulation
    populate = function(simout, type) {
      if (type == "sim") {
        # self$obs <- simout$obs
        # self$amt <- simout$amt
        # self$parValues <- simout$parValues
        # self$totalMeans <- simout$totalMeans
        # self$totalCov <- simout$totalCov
        self$data <- simout
        class(self$data) <- c("PM_sim_data", "list")
      } else if (type == "simlist") {
        N <- length(simout) # number of templates
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
        # self$obs <- simout$data$obs
        # self$amt <- simout$data$amt
        # self$parValues <- simout$data$parValues
        # self$totalMeans <- simout$data$totalMeans
        # self$totalCov <- simout$data$totalCov
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
    
    makeNoise = function(template, noise) {
      if (!is.list(noise)) {
        cli::cli_warn(c(
          "!" = "Noise arguments should be a list.",
          "i" = "See ?PM_data for details on how to add noise."
        ))
        return(invisible(template))
      }
      
      for (i in 1:length(noise)) {
        this <- noise[[i]]
        this$.col <- names(noise)[i]
        if (this$.col %in% c("id", "evid", "addl", "ii", "input", "outeq", "c0", "c1", "c2", "c3")) {
          cli::cli_abort(c(
            "x" = "{.arg {this$.col}} is a reserved column name.",
            "i" = "Please choose another column to add noise."
          ))
        }
        this$coeff <- this[[1]]
        if (is.null(this$mode)) {
          this$mode <- "add"
        }
        
        # add zeros to coefficients if needed to make up to length 4
        if (length(this$coeff) < 4) {
          this$coeff <- c(this$coeff, rep(0, 4 - length(this$coeff)))
        }
        
        # Ensure target is a column in standard_data
        if (!this$.col %in% names(template)) {
          cli::cli_abort(c(
            "x" = "{.arg {this$.col}} is not a column in your data.",
            "i" = "Example: {.code noise = list(dose = list(coeff = c(0.1, 0.1)))}"
          ))
        }
        
        # make temporary row index to preserve order later
        template$index_ <- 1:nrow(template)
        
        # Dynamically apply the filter
        if (!is.null(this$filter)) {
          filter_status <- "filtered"
          filter_exprs <- rlang::parse_expr(this$filter)
          filtered_data <- template %>%
          filter(!!filter_exprs)
          # Keep the rest
          remaining_data <- template %>%
          filter(magrittr::not(!!filter_exprs))
        } else {
          filter_status <- ""
          filtered_data <- template
          remaining_data <- NULL
        }
        
        # Get the target
        target_col <- filtered_data %>% select(id, raw = all_of(this$.col))
        
        
        # Remove temp row index
        template <- template %>% select(-index_)
        
        # Add noise
        new_target <- data.frame(1:nrow(target_col))
        names(new_target) <- this$.col
        if (this$mode == "add") {
          target_col <- target_col %>%
          rowwise() %>%
          mutate(noisy = raw + suppressWarnings(rnorm(1,
            mean = 0,
            sd = this$coeff[[1]] +
            this$coeff[[2]] * raw +
            this$coeff[[3]] * raw^2 +
            this$coeff[[4]] * raw^3
          ))) %>%
          ungroup()
        } else if (this$mode == "exp") {
          target_col <- target_col %>%
          rowwise() %>%
          mutate(noisy = raw * exp(suppressWarnings(rnorm(1,
            mean = 0,
            sd = this$coeff[[1]] +
            this$coeff[[2]] * raw +
            this$coeff[[3]] * raw^2 +
            this$coeff[[4]] * raw^3
          )))) %>%
          ungroup()
        } else {
          cli::cli_abort("x" = "Mode must be 'add' or 'exp'.")
        }
        
        # put back the new noisy column
        filtered_data[[this$.col]] <- target_col$noisy
        
        combined <- bind_rows(filtered_data, remaining_data) %>%
        arrange(index_) %>%
        select(-index_)
        # Fix initial times to be 0 in case they were mutated
        combined[!duplicated(combined$id), "time"] <- 0
        
        template <- combined
      } # end for loop for each noise element
      
      return(template)
    }, # end makeNoise function
    
    makePredInt = function(template, predInt) {
      predTimes <- NA
      numeqt <- max(template$outeq, na.rm = TRUE)
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
      mutate(.id = dplyr::dense_rank(id))
      
      # second, add predInt if necessary
      if (!is.na(predTimes[1])) {
        predTimes <- predTimes[predTimes > 0] # remove predictions at time 0
        dat3 <- dat2 %>%
        group_by(.id) %>%
        group_map(~ {
          theseTimes <- predTimes[!predTimes %in% .x$time[.x$evid == 0]] # remove prediction times at times that are specified in template
          numPred <- length(theseTimes)
          newPred <- data.frame(matrix(NA, nrow = numPred, ncol = 1 + ncol(.x)))
          names(newPred) <- c(".id", names(.x))
          newPred[, 1] <- .y # .id
          newPred[, 2] <- .x$id[1] # original id
          newPred[, 3] <- 0 # evid
          newPred[, 4] <- theseTimes # time
          newPred[, 10] <- 1 # out
          newPred[, 11] <- rep(1:numeqt, numPred / numeqt) # outeq
          newPred
        }) %>%
        bind_rows()
        new_dat <- bind_rows(dat2, dat3) %>%
        arrange(.id, time, outeq) %>%
        select(-.id)
      } else { # predInt was not specified
        new_dat <- template # the original data without .id
      }
      new_dat <- new_dat %>% mutate(out = ifelse(evid == 0, -1, NA)) # replace all obs with -1 since simulating
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
#' @param exclude `r template("exclude")`.
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
#'     defaults above, i.e., `c(0.05, 0.5, and 0.95)`
#'     Example: `line = list(probs = c(0.25, 0.5, 0.75))`.
#'     - `color` Vector of color names whose order corresponds to `probs`.
#'     If shorter than `probs`, will be recycled. Default is "dodgerblue", but if
#'     median is present (`prob = 0.5`), that line will be "red".
#'     Examples: `line = list(color = "red")` or `line = list(color = c("red", "blue"))`.
#'     - `fill` Fill color between quantile lines. Can be specified in several ways:
#'        * `FALSE` (the default) will not fill between lines.
#'        * `TRUE` will fill between lines with a default color of "dodgerblue", opacity 0.2.
#'        * A list with the following elements:
#'          - `color` Fill color name. Default is "dodgerblue", e.g., `fill = list(color = "red")`.
#'          - `opacity` Fill opacity. Default is 0.2 e.g., `fill = list(opacity = 0.3)`.
#'          - `probs` Vector of paired quantiles to fill between. Default is the minimum and maximum
#'          quantile specified in `probs`, or `fill = list(probs = c(0.05, 0.95))` if not specified.
#'          Including `probs` in fill which are not in `probs` above will result in an error.
#'     - `width` Vector of widths in pixels, as for `color`. Default is 1.
#'     Example: `line = list(width = 2)`.
#'     - `dash` Vector of dash types, as for color. Default is "solid".
#'     See `plotly::schema()`, traces > scatter > attributes > line > dash > values.
#'     Example: `line = list(dash = "dashdot")`.
#' @param marker `r template("marker")` Formatting will only be applied to observations
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
#' @param quiet If `TRUE`, suppresses the message about simulation report generation, defaults to `FALSE`.
#' @param legend `r template("legend")` Default is `FALSE`
#' @param log `r template("log")` Default is `TRUE`.
#' @param grid `r template("grid")` Default is `FALSE`
#' @param xlab `r template("xlab")` Default is "Time".
#' @param ylab `r template("ylab")` Default is "Output".
#' @param title `r template("title")` Default is to have no title.
#' @param xlim `r template("xlim")`
#' @param ylim `r template("ylim")`
#' @param print If `TRUE`, will print the plotly object and return it. If `FALSE`, will only return the plotly object.
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
#' \dontrun{
#' simEx$plot()
#' simEx$plot(log = FALSE, line = list(color = "orange"))
#' }

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
  print = TRUE, ...) {
    if (all(is.na(line))) {
      line <- list(probs = NA)
    } # standardize
    if (is.logical(line)) {
      if (line) {
        lineList <- list(
          probs = c(0.05, 0.5, 0.95),
          color = c("dodgerblue", "red", "dodgerblue"),
          width = rep(1, 3),
          dash = rep("solid", 3)
        ) # line = T
      } else { # line = F
        lineList <- amendLine(FALSE)
        lineList$probs <- NULL # line was FALSE, so probs = NULL -> no join lines or quantiles
        lineList$width <- 0
      }
    } else { # line was not T/F
      if (!is.list(line) & length(line) > 1) {
        cli::cli_abort(c(
          "x" = "{.arg Line} is misspecified.",
          "i" = "If you want to specify quantiles, use {.code line = list(probs = c(...), ...)}."
        ))
      }
      if (!is.null(purrr::pluck(line, "probs"))) {
        lineList <- list(probs = line$probs)
      } else {
        lineList <- list(probs = c(0.05, 0.5, 0.95))
      }
      nprobs <- length(lineList$probs)
      if (!is.null(purrr::pluck(line, "color"))) {
        lineList$color <- rep(line$color, nprobs)[1:nprobs]
      } else {
        lineList$color <- rep("dodgerblue", nprobs)
        #color median red if present
        medProb <- which(lineList$probs == 0.5)
        if (length(medProb) == 0) {
          lineList$color[medProb] <- "red"
        }
      }
      if (!is.null(purrr::pluck(line, "fill"))) {
        if (is.logical(line$fill && line$fill)) {
          lineList$fill <- list(color = "dodgerblue", opacity = 0.2, probs = range(lineList$probs))
        } else if (is.logical(line$fill && !line$fill)) {
          lineList$fill <- NULL
        } else if (is.list(line$fill)) {
          if (is.null(purrr::pluck(line$fill, "color"))) {
            line$fill$color <- "dodgerblue"
          }
          if (is.null(purrr::pluck(line$fill, "opacity"))) {
            line$fill$opacity <- 0.2
          }
          if (is.null(purrr::pluck(line$fill, "probs"))) {
            line$fill$probs <- range(lineList$probs)
          }
          lineList$fill <- line$fill
          
        } else {
          cli::cli_abort(c(
            "x" = "{.arg fill} is misspecified.",
            "i" = "If you want to specify fill between quantiles, use {.code line = list(fill = list(...), ...)}."
          ))
          
        }
      } 
      
      if(!is.null(lineList$fill$probs) && !all(lineList$fill$probs %in% lineList$probs)) {
        cli::cli_abort(c(
          "x" = "{.arg fill} quantiles must be a subset of {.arg probs}.",
          "i" = "Please check your {.code line} argument."
        ))
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
    
    # process x
    if (inherits(x, "PM_sim")) {
      simout <- x$data
    } else if (inherits(x, "PM_sim_data")) {
      simout <- x
    } else {
      cli::cli_abort(c(
        "x" = "{.cls PM_sim} or {.cls PM_sim_list} object required.",
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
          name = "CI",
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
      
      # add fill if specified
      if(!is.null(lineList$fill)) {
        if(ci > 0){
          cli::cli_inform(c(
            "i" = "Consider {.code ci = 0} for cleaner plot.",
            " " = "See {.help PM_sim$plot()} for details."
          ))
        }
        fill_area <- sim_quant_df %>% filter(quantile %in% lineList$fill$probs) %>% select(time, quantile, value) %>%
        tidyr::pivot_wider(names_from = quantile, values_from = value) %>%
        rename(lower = 2, upper = 3)
        
        p <- p %>% 
        plotly::add_ribbons(data = fill_area, x = ~time, ymin = ~lower, ymax = ~upper, 
          name = "CI",
          line = NULL,
          inherit = FALSE,
          fillcolor = lineList$fill$color,
          opacity = lineList$fill$opacity)
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
    p <- sim %>% mutate(id2 = paste(id, nsim, sep = "_")) %>%
    group_by(id2) %>%
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
  
  
  if (print) print(p)
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
#' @param include `r template("include")`.
#' @param exclude `r template("exclude")`.
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
#' \dontrun{
#' simEx$summary() # preferred
#' summary(simEx) # alternative
#' simEx$summary(include = 2, field = "amt", group = "comp") # group amounts by compartment
#' }

#' @seealso [PM_sim]
#' @export
summary.PM_sim <- function(object, include, exclude, field = "obs", group = NULL,
statistics = c("mean", "sd", "median", "min", "max"),
digits = max(3, getOption("digits") - 3), ...) {
  # get the right data
  if (inherits(object, "PM_sim")) {
    dat <- object$data[[field]]
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
  if (!"id" %in% group) {
    summ <- purrr::map(summ, \(x) {
      x$id <- NULL
      x
    })
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
#' \dontrun{
#' simEx$summary()
#' }

#' @export

print.summary.PM_sim <- function(x, ...) {
  print.data.frame(x)
  if (!is.null(attr(x, "group"))) {
    cat("\n", "Grouped by:", paste(crayon::blue(attr(x, "group")), collapse = ", "), "\n")
  }
}





# generate random samples from multivariate, multimodal normal distribution
generate_multimodal_samples <- function(num_samples, weights, means, cov_matrix, i, limits) {
  
  
  # turn means into a list of vectors if needed
  means <- split(means, 1:nrow(means))
  if (length(weights) != length(means)) {
    cli::cli_abort("Weights and means must have the same length.")
  }
  
  # handle malformed covariance
  if (any(is.na(cov_matrix))) {
    cov_matrix[is.na(cov_matrix)] <- 0
    cli::cli_warn(c(
      "!" = "Covariance for {.code id = {i}} is undefined.", # this is the only place i is used
      "i" = "Values were set to 0, resulting in identical simulations."
    ))
  }
  
  # Determine number of samples from each mode
  
  
  samples_per_mode <- stats::rmultinom(1, size = num_samples, prob = weights)
  
  # function used later to check if any parameters are outside their limits
  outside_check <- function(x) {
    any(x - limits$min < 0) | # any parameter < lower limit
    any(x - limits$max > 0) # any parameter > upper limit
  }
  
  #Generate samples bounded by limits for each mode
  all_samples <- map(1:length(weights), function(j) {
    samples <- tryCatch(suppressWarnings(MASS::mvrnorm(n = samples_per_mode[j,], mu = as.matrix(means[[j]], nrow = 1), Sigma = cov_matrix)), error = function(e) NULL)
    
    # replace any outside their limits
    if(!is.null(samples)){
      if(!is.matrix(samples)){
        samples <- as.data.frame(as.list(samples))
        names(samples) <- names(means[[j]])
      }
      
      discarded <- NULL
      if (!all(is.null(limits))) {
        for (k in 1:nrow(samples)) {
          cycle_num <- 0
          outside <- outside_check(samples[k, ])
          while (outside && cycle_num < 20) {
            new_sample <- tryCatch(suppressWarnings(MASS::mvrnorm(n = 1, mu = as.matrix(means[[j]], nrow = 1), Sigma = cov_matrix)), error = function(e) NULL)
            cycle_num <- cycle_num + 1
            outside <- outside_check(new_sample)
          }
          if (outside) {
            cli::cli_abort(c("x" = "Unable to generate simulated parameters within limits after 20 attempts per row."))
          }
          if (cycle_num > 0) {
            discarded <- rbind(discarded, samples[k, ])
            samples[k, ] <- new_sample
          }
          
        } # end loop to fix thetas out of range
      }
      
      
      list(keep = samples, discard = discarded) # the final set of samples for this mode
      
    }
    
    
  }
)


retained <- all_samples %>% map( \(x) x$keep) %>% do.call(rbind, .) %>%
tibble::as_tibble(.name_repair = "minimal") %>%
mutate(prob = 1 / dplyr::n())

discarded <- all_samples %>% map( \(x) x$discard) %>% do.call(rbind, .) %>%
tibble::as_tibble(.name_repair = "minimal") %>%
mutate(prob = 1 / dplyr::n())

total_means <- apply(rbind(retained, discarded), 2, mean)[1:ncol(cov_matrix)]
total_cov <- bind_rows(retained, discarded) %>%
select(-prob) %>% cov()
total_nsim <- sum(nrow(retained), nrow(discarded)) # sum will ignore NULL values

return(list(
  thetas = retained, total_means = total_means,
  total_cov = total_cov,
  total_nsim = total_nsim
))

}