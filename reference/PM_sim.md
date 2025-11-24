# Object to contain results of simulation

**\[stable\]**

This object is created after a successful run of the simulator.

## Details

There are two methods of creating a PM_sim object.

- **PM_result\$sim()**

- **PM_sim\$new()**

They return fully parsed simulator output as PM_sim objects in R.
Details can be found in the documentation for `$new` method below.

## Author

Michael Neely

## Public fields

- `data`:

  A list of class *PM_sim* that contains the following elements.

  - **obs** Observations for all simulated templates as a data frame
    with these columns:

    - *id* Template ID, corresponding to the ID in the template data
      file

    - *nsim* The simulated profile number, from 1 to the value for
      `nsim` specified when the simulation was run.

    - *time* Time of the simulated observation.

    - *out* The simulated observation.

    - *outeq* The output equation number.

  - **amt** Compartment amounts for each simulated template as a data
    frame with these columns:

    - *id* As for `obs`.

    - *nsim* As for `obs`.

    - *time* As for `obs`.

    - *out* The simulated amount.

    - *comp* The compartment number that contains the `out` amount.

  - **parValues** A data frame with retained simulated parameter values
    after discarding any due to truncation limits. The data frame has
    these columns:

    - *id* This column is only present if `usePost = TRUE`, since in
      that case the `nsim` profiles for each template are created by
      sampling from a different prior joint parameter probability
      distribution for each template. When `usePost = FALSE`, the same
      prior is used for every template, so there is no `id` column.

    - *nsim* The simulation number, from 1 to the value for `nsim`
      specified when the simulation was run.

    - a column for each random parameter in the model with the simulated
      values

  - **totalSets** When `usePost = FALSE`, the number of all simulated
    parameter values needed to obtain the requested number of simulated
    sets within any `limits` specified. When `usePost = TRUE`, a data
    frame with the same number for each template in the `data` file,
    since each template is simulated from a different prior distribution
    (see `parValues:id` above).

  - **totalMeans** If `usePost = FALSE`, this is a vector with the means
    of all simulated parameter values, including those discarded for
    being outside `limits`. If `usePost = TRUE`, this is a data frame of
    vectors, one for each template in the `data` file, and an `id`
    column to identify the template in the `data` source. This can be
    useful to check against the original means in `poppar`, since the
    mean of the `parValues` may be different due to truncation.

  - **totalCov** Similar to `totalMeans`, either a single covariance
    matrix for all simulated parameter values when `usePost = FALSE` .
    If `usePost = TRUE`, this is a data frame of such matrices, one for
    each template in the `data` file, and an `id` column to identify the
    template in the `data` source. Again, this can be useful as a check
    against the original covariance in `poppar`.

## Active bindings

- `obs`:

  Same as `obs` element in the `data` field.

- `amt`:

  Same as `amt` element in the `data` field.

- `parValues`:

  Same as `parValues` element in the `data` field.

- `totalSets`:

  Same as `totalSets` element in the `data` field.

- `totalMeans`:

  Same as `totalMeans` element in the `data` field.

- `totalCov`:

  Same as `totalCov` element in the `data` field.

## Methods

### Public methods

- [`PM_sim$new()`](#method-PM_sim-new)

- [`PM_sim$save()`](#method-PM_sim-save)

- [`PM_sim$plot()`](#method-PM_sim-plot)

- [`PM_sim$pta()`](#method-PM_sim-pta)

- [`PM_sim$auc()`](#method-PM_sim-auc)

- [`PM_sim$summary()`](#method-PM_sim-summary)

- [`PM_sim$run()`](#method-PM_sim-run)

- [`PM_sim$load()`](#method-PM_sim-load)

- [`PM_sim$clone()`](#method-PM_sim-clone)

------------------------------------------------------------------------

### Method `new()`

This function simulates outputs from given inputs and a model. It can be
called directly or via the `$sim` method for
[PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
objects.

#### Usage

    PM_sim$new(
      poppar,
      model,
      data,
      limits = NULL,
      split = NULL,
      include = NULL,
      exclude = NULL,
      nsim = 1000,
      predInt = 0,
      covariate = NULL,
      usePost = FALSE,
      seed = -17,
      noise = NULL,
      makecsv = NULL,
      quiet = FALSE,
      ...
    )

#### Arguments

- `poppar`:

  One of four things:

  1.  A
      [PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
      object containing the final population parameter distribution from
      a model run, a model object, and a data object. The model object
      may be replaced by a different
      [PM_model](https://lapkb.github.io/Pmetrics_rust/reference/PM_model.md),
      as long as the primary parameters are the same as the original
      model. The data object may also be replaced (and often is) by a
      different
      [PM_data](https://lapkb.github.io/Pmetrics_rust/reference/PM_data.md)
      object compatible with the model.

          run1 <- PM_load(1) # load the PM_result object
          sim1 <- run1$sim(...) # replace model and data in run1 if desired;
          #must be compatible with model and data in run1

          mod <- PM_model$new("model.txt") # or use a model object
          sim2 <- mod$sim(poppar = run1, data = "newdata.csv", ...)
          # poppar and data necessary, model obtained from mod

  2.  Population prior parameters as a
      [PM_final](https://lapkb.github.io/Pmetrics_rust/reference/PM_final.md)
      object found in `PM_result$final`.

          run1 <- PM_load(1) # load the PM_result object
          sim1 <- PM_sim$new(poppar = run1$final, model = newmodel, data = newdata, ...)
          # model and data necessary

          mod <- PM_model$new("model.txt") # or use a model object
          sim2 <- mod$sim(poppar = run1$final, data = "newdata.csv", ...)
          # poppar and data necessary, model obtained from mod

  3.  The name of a previously saved simulation via the `$save` method.
      The file will be loaded. This filename should have the ".rds"
      extension, e.g. `sim1 <- PM_sim$new("sim.rds")`.

  4.  A manually specified prior as a list containing the following
      named items:

      - **wt** vector of weights (probabilities) of sampling from each
        distribution. If missing, assumed to be 1.

      - **mean** a list of mean parameter values. Each element of the
        list should be named with the parameter name and be a vector of
        length equal to the number of distributions. See details below.

      - **sd** an optional named list of overall standard deviations for
        each parameter, considering parameters as unimodally
        distributed, i.e. there should only be one value for each
        parameter, regardless of the number of distributions. `sd` is
        only needed if a correlation matrix is specified, which will be
        converted to a covariance matrix.

      - **ONE** of the following matrices:

        1.  **cor** A square matrix of the overall correlations between
            parameters, again considered as unimodally distributed, i.e.
            there should only be one correlation matrix regardless of
            the number of distributions. If a correlation matrix is
            specified, the `sd` element is required to calculate the
            covariance matrix.

        2.  **cov** A square matrix of the overall covariances between
            parameters, again considered as unimodally distributed, i.e.
            there should only be one covariance matrix regardless of the
            number of distributions. If a covariance matrix is
            specified, the `sd` element is unnecessary, since the
            diagonals of the covariance matrix are the variances or
            squared standard deviations.

      If only one distribution is to be specified the `wt` vector can be
      ommitted or should be `wt = 1`. If multiple distributions are to
      be sampled, the `wt` vector should be of length equal to the
      number of distributions in `mean` and the values of `wt` should
      sum to 1, e.g. `wt = c(0.25, 0.05, 0.7)`. The `mean` element
      should be a list of elements, named for the parameters, with
      vectors of values equal to the number of terms in `wt`. If `cor`
      is used, Pmetrics will use the `sd` element to calculate the
      covariance matrix. The covariance matrix will be divided by the
      number of distributions, i.e. `length(wt)`, and applied to each
      distribution.

      Examples:

      - Single distribution:

          poppar = list(wt = 1,
                        mean = list(ke = 0.5, v = 100),
                        cov = matrix(c(0.04, 2.4, 2.8, 400), nrow = 2))  # sd not required because cov specified

      - Multiple distributions:

          poppar = list(wt = c(0.1, 0.15, 0.75), # 3 distributions that sum to 1
                        mean = list(ke = c(2, 0.5, 1), v = c(50, 100, 200)), # 3 values for each parameter
                        sd = list(ke = 0.2, v = 20), # overall sd, ignoring multiple distributions
                        cor = matrix(c(1, 0.6, 0.7, 1), nrow = 2)) # sd required because cor specified

- `model`:

  Name of a suitable
  [PM_model](https://lapkb.github.io/Pmetrics_rust/reference/PM_model.md)
  object or a model file template in the working directory. If missing,
  and `poppar` is a
  [PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md),
  the model within the `$model` field of the
  [PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
  object will be used. If `model` is missing and `poppar` is not a
  [PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md),
  then Pmetrics will attempt to load a model file in the working
  directory called "model.txt" as the default name.

- `data`:

  Either a
  [PM_data](https://lapkb.github.io/Pmetrics_rust/reference/PM_data.md)
  object or a character vector with the file name of a Pmetrics data
  file in the working directory that contains **template** regimens and
  observation times. The value for outputs can be coded as any number(s)
  other than -99. The number(s) will be replaced in the simulator output
  with the simulated values. Outputs equal to -99 will be simulated as
  missing. If `data` is missing, and `poppar` is a
  [PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md),
  the data within the `$data` field of the
  [PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
  object will be used. If `data` is missing and `poppar` is not a
  [PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md),
  then Pmetrics will attempt to load a data template file in the working
  directory called "data.csv" as the default name.

- `limits`:

  If limits are specified, each simulated parameter set that contains a
  value outside of the limits will be ignored and another set will be
  generated. Four options exist for limits.

  - The default `NULL` indicates that no limits are to be applied to
    simulated parameters.

  - The second option is to set `limits` to `NA`. This will use the
    parameter limits on the primary parameters that are specified in the
    [PM_model](https://lapkb.github.io/Pmetrics_rust/reference/PM_model.md)
    object.

  - The third option is a numeric vector of length 1 or 2, e.g.
    `limits = 3` or `limits = c(0.5, 4)`, which specifies what to
    multiply the columns of the parameter limits in the model file. If
    length 1, then the lower limits will be the same as in the model
    file, and the upper limits will be multiplied by value specified. If
    length 2, then the lower and upper limits will be multiplied by the
    specified values. If this option is used, `poppar` must be a
    `PM_final` object.

  - The fourth option for limits is a fully customized data frame or
    list of limits for simulated values for each parameter which will
    overwrite any limits in the model file. If specified, it should be a
    data frame or list with columns or elements, respectively, "par",
    "min", "max". For example, use a PM_final\$ab object, or a code it
    like
    `limits = list(par = c("Ka", "Ke", "V"), min = c(0.1, 0.1, 10), max = c(5, 5, 200))`
    or
    `limits = data.frame(par = c("Ka", "Ke", "V"), min = c(0.1, 0.1, 10), max = c(5, 5, 200))`
    or
    `limits = tibble::tibble(par = c("Ka", "Ke", "V"), min = c(0.1, 0.1, 10), max = c(5, 5, 200))`.
    Each of these specifies custom limits for 3 parameters named Ka, Ke,
    and V, with limits of (0.1, 5), (0.1, 5) and (10, 200),
    respectively. The last example uses tibbles, the tidyverse
    equivalent of data frames.

  Means and covariances of the total number of simulated sets will be
  returned to verify the simulation, but only those sets within the
  specified limits will be used to generate output(s) and the means and
  covariances of the retained sets may (and likely will be) different
  than those specified by `poppar`.

- `split`:

  Boolean operator controlling whether to split an NPAG
  [PM_final](https://lapkb.github.io/Pmetrics_rust/reference/PM_final.md)
  object into one distribution per support point, with means equal to
  the vector of parameter values for that point, and covariance equal to
  the population covariance divided by the number of support points.
  Default for NPAG
  [PM_final](https://lapkb.github.io/Pmetrics_rust/reference/PM_final.md)
  objects is `TRUE`, otherwise `FALSE`.

- `include`:

  A vector of subject IDs in the `data` to iterate through, with each
  subject serving as the source of an independent simulation. Default is
  `NA` and all subjects in the data file will be used.

- `exclude`:

  A vector of subject IDs to exclude in the simulation, e.g.
  `exclude = c(4, 6:14, 16:20)`. Default is `NA` and all subjects in the
  data file will be used, i.e. none excluded. Using both `include` and
  `exclude` criteria may result in conflicts.

- `nsim`:

  The number of simulated profiles to create, per subject. Default
  is 1000. Entering 0 will result in one profile being simulated from
  each point in the non-parametric prior (for NPAG final objects only).

- `predInt`:

  The interval in fractional hours for simulated predicted outputs at
  times other than those specified in the template `data`. The default
  is 0, which means there will be simulated outputs only at times
  specified in the data file (see below). Values greater than 0 result
  in simulated outputs at the specified value, e.g. every 15 minutes for
  `predInt = 0.25` from time 0 up to the maximal time in the template
  file, per subject if nsub \> 1. You may also specify `predInt` as a
  vector of 3 values, e.g. `predInt = c(1, 4, 1)`, similar to the R
  command [seq](https://rdrr.io/r/base/seq.html), where the first value
  is the start time, the second is the stop time, and the third is the
  step value. Finally, you can have multiple such intervals by
  specifying `predInt` as a list of such vectors, e.g.
  `predInt = list(c(0, 24, 1), c(72, 96, 1))`. Outputs for times
  specified in the template file will also be simulated. To simulate
  outputs *only* at the output times in the template data (i.e. EVID=0
  events), use `predInt = 0`, which is the default. Note that the
  maximum number of predictions is 594, so the prediction interval must
  be sufficiently long to accommodate this for a given number of output
  equations and total time to simulate over. If `predInt` is set so that
  this cap is exceeded, predictions will be truncated.

- `covariate`:

  Pmetrics can simulate values for some/all covariates declared in the
  `cov` block of the
  [PM_model](https://lapkb.github.io/Pmetrics_rust/reference/PM_model.md).
  This argument is a list with the following named elements.

  - **cov** Optional if `poppar` is a
    [PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
    object, but required if `poppar` is a
    [PM_final](https://lapkb.github.io/Pmetrics_rust/reference/PM_final.md)
    object or a manually specified prior, e.g., with values obtained
    from the literature.

  - **mean** Required only if `poppar` is a manually specified prior,
    optional otherwise.

  - **sd** Required only if `poppar` is a manually specified prior,
    optional otherwise.

  - **limits** Optional in all cases.

  - **fix** Optional in all cases.

  The simplest example is when simulating covariates from a
  [PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md):

      run1 <- PM_load(1)
      run1$sim(..., covariate = list())`

  Details on each element are below.

  `cov`

  This element specifies the source of the correlation matrix for
  covariate values and if possible model primary parameters, i.e., those
  in the `pri` block of the model. In the first two cases below,
  Pmetrics will use this `covariate$cov` object to calculate the
  correlation matrix between all covariates and Bayesian posterior
  parameter values. In the third case, there is no way to calculate the
  correlations between parameters and covariates, so Pmetrics only
  calculates the covariate correlations.

  - **Case 1**. If `poppar` is a
    [PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md),
    Pmetrics will use the `$cov` field within that object to obtain
    covariate means, standard deviations (sd), and correlations among
    covariates and parameter values. In this case, you can omit this
    element of the `covariate` list. See the example above.

  - **Case 2**. If `poppar` is a
    [PM_final](https://lapkb.github.io/Pmetrics_rust/reference/PM_final.md),
    you will need to supply the name of a
    [PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
    or
    [PM_cov](https://lapkb.github.io/Pmetrics_rust/reference/PM_cov.md)
    object as the value for this element so that Pmetrics can calculate
    covariate means, sd, and correlations.

        run1 <- PM_load(1)
        sim1 <- PM_sim$new(poppar = run1$final, covariate = list(cov = run1$cov), model = run1$model, data = "newdata.csv")

  - **Case 3**. If `poppar` is a manually specified prior, or you wish
    to simulate covariates not in the original model, you must provide a
    covariance or correlation matrix between the covariates. In this
    case, it is only possible to calculate correlations between
    covariates from the matrix and not between parameters and
    correlations, since they are unknown. The `$mean` and optionally the
    `$sd` elements of the `covariate` list specified below are also
    required to complete the necessary information for simulation.
    Similar to `poppar`, if `$sd` is missing, the the `cov` object is
    treated as a covariance matrix, otherwise it is treated as a
    correlation matrix.

        corMat <- matrix(c(1, .98, .98, 1), nrow = 2) # correlation matrix for age and wt, for example
        covariate <- list(cov = corMat, mean = list(age = 9, wt = 32), sd = list(age = 5.5, wt = 18.8)) # note the named lists for mean and sd, and cov is treated as a correlation matrix

        covMat <- matrix(c(30.25, 101.33, 101.33, 353.44), nrow = 2)
        covariate <- list(cov = covMat, mean = list(age = 9, wt = 32)) # equivalent covariance matrix, and sd is not required

  `mean`

  A named list that specifies the mean for one or more of the covariates
  in your model. If you are simulating in Case 1 or 2 above, `mean` is
  optional and allows you to use a different mean value than was in your
  model-building population. For example, the population may have had a
  mean weight of 30 kg, but
  `covariate = list(..., mean = list(wt = 70))` allows you to simulate
  weight with a mean of 70. If this argument is missing then the mean
  covariate values in the population will be used for simulation. The
  same applies to any covariates that are not named in this list.

  In Case 3, `mean` is required and must be a named list with the names
  of the covariates in the correlation matrix, and the values as the
  mean values for those covariates. See the example in `cov` above under
  Case 3.

  Examples:

  - Using a
    [PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
    as poppar: `PM_sim$new(poppar = run1, covariate = list())`. Here we
    don't need to specify `cov` because it is already in the
    [PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
    `run1`. We are not re-centering or otherwise modifying the
    covariates, so `covariate` can be an empty list.

  - Using a
    [PM_final](https://lapkb.github.io/Pmetrics_rust/reference/PM_final.md)
    as poppar:
    `PM_sim$new(poppar = run1$final, covariate = list(cov = run1$cov, mean = list(wt = 50))`.
    Here we need to specify `cov` because it is not in the
    [PM_final](https://lapkb.github.io/Pmetrics_rust/reference/PM_final.md)
    object. Futhermore, we want to recenter the mean values, so we add
    the `$mean` element.

  - Using a manually specified covariate correlation matrix:

        corMat <- matrix(c(1, .98, .98, 1, nrow = 2) # correlation matrix for age and wt
        covariate <- list(cov = corMat, mean = list(age = 9, wt = 32), sd = list(age = 5.5, wt = 18.8)) # mean and sd are required
        PM_sim$new(poppar = poppar , covariate = covariate) # covariates will be added to poppar for simulation regardless of the source of poppar

  `sd`

  This functions just as the \`\$mean“ argument does, but for standard
  deviations.

  `limits` This is a bit different than the limits for population
  parameters above. Here, `limits` is similar to `mean` and `sd` for
  covariates in that it is a named list with the minimum and maximum
  allowable simulated values for each covariate. If it is missing
  altogether, then no limits will apply. If it is specified, then named
  covariates will have the indicated limits, and covariates not in the
  list will have limits that are the same as in the original population.
  If you want some to be limited and some to be unlimited, then specify
  the unlimited ones as items in this list with very large ranges. In
  the examples below, assume that the covariates age and wt are being
  simulated.

  - `covariate = list(..., limits = list( wt = c(10, 70)))` will limit
    wt to between 10 and 70 kg. Since age is also being simulated, it
    will have the same limits as in the population under Cases 1 and 2
    above. Under Case 3, there is no population value for wt or age, so
    wt will be limited and age will be unlimited.

  - `covariate = list(..., limits = list( wt = c(10, 70), age = c(0, 200)))`
    will limit wt to between 10 and 70 kg and age to between 0 and 200
    years, which is effectively no limit. This would only be necessary
    under Cases 1 or 2 when age was a covariate in the data and model.

  `fix`

  A character vector (not a list) of model covariates to fix and not
  simulate. Values in the template data will be used and not simulated.
  Example: `covariate = list(..., fix = c("wt", "age"))`.

- `usePost`:

  Boolean, default `FALSE`. Only applicable when `poppar` contains an
  NPAG
  [PM_final](https://lapkb.github.io/Pmetrics_rust/reference/PM_final.md)
  object. If `TRUE`, the mean posterior parameter values and covariances
  for each subject, modified by `include` or `exclude`, in `poppar` will
  be used to simulate rather than the population prior. The number of
  subjects in the template `data` file must be the same. Normally one
  uses the same data file as used to make the model final parameter
  distribution in `poppar`, but if different templates are desired, the
  number must be equivalent to the number of included subjects from whom
  the posteriors are obtained.

- `seed`:

  The seed for the random number generator. For `nsub` \> 1, should be a
  vector of length equal to `nsub`. Shorter vectors will be recycled as
  necessary. Default is -17.

- `noise`:

  A named list to add noise to most template data fields, including
  covariates. The default is `NULL`, which means no noise will be added.
  The name of each element in the list should correspond to a column in
  the data to which you wish to add noise, typically `time`, `dose`, or
  `out`. Note that noise is added to the `out` column *after* simulation
  but *before* simulation for all other columns. Noise on the `out`
  column is best thought of as measurement error on the true, simulated
  value. Thus, in the simulated output, the `amt` values for a given
  template id, simulation number, time, and output equation will no
  longer be exactly related to the corresponding `out` value by the
  volume term in the model.

  These columns may not have noise added: `id`, `evid`, `addl`, `ii`,
  `input`, `outeq`, `c0`, `c1`, `c2`, and `c3`. See
  [PM_data](https://lapkb.github.io/Pmetrics_rust/reference/PM_data.md)
  for further details on these columns.

  Each element in the `noise` list should be another list with the
  following arguments. The `coeff` argument is mandatory, and should be
  the first argument. It can be named or unnamed. The `filter` and
  `mode` arguments are optional and should always be named in the list.

  - **coeff** Mandatory. A vector of up to 4 coefficients for the noise
    model. They correspond to *C0*, *C1*, *C2*, and *C3* for the assay
    noise model (as in
    [PM_model](https://lapkb.github.io/Pmetrics_rust/reference/PM_model.md)).
    See the 'mode' argument for details on how these values are used to
    generate noise. Examples:
    `noise = list(out = list(coeff = c(0.1, 0.1))` or
    `noise = list(dose = list(coeff = c(5, 0.15, -0.01, 0.003)))`.

  - **filter** Optional. A quoted expression to filter the data. For
    example,
    `noise = list(dose = list(c(0.1, 0.1), filter = "dose > 0.1"))` or
    `noise = list(out = list(c(0.05, 0.15), filter = "outeq == 1 & time < 120"))`.

  - **mode** Optional. The mode (method) of the noise. Default is `add`.
    Options are `add` or `exp`.

    - `add` An additive noise model. The new value is generated as
      `value + noise`, where noise is a random number from a normal
      distribution, with mean of 0 and
      `SD = C0 + C1*value + C2*value^2 + C3*value^3`, and *value* is the
      original value in each row of the target column.

    - `exp` An exponential noise model. The new values is generated as
      `value * exp(noise)`, where noise is a random number from a normal
      distribution, with mean of 0 and
      `SD = C0 + C1*value + C2*value^2 + C3*value^3`, and *value* is the
      original value in each row of the target column. Example:

        exDat$makeNoise(list(dose = list(coeff = c(0.1, 0.1), filter = "dose > 100 & time < 200", mode = "add"),
        out = list(c(0.1, 0.001), mode = "exp")))

- `makecsv`:

  A character vector for the name of the single .csv file to be made for
  all simulated "subjects". If no file extension is included, ".csv"
  will be added, e.g. "simout" will become "simout.csv". If an extension
  is included, Pmetrics will use it, e.g. "simout.ssv" will save under
  that name. If missing, no file will be made. If a `makecsv` filename
  is supplied, ID numbers will be of the form *id_nsim*, e.g. 1_1
  through 1_10 through for the first data template id, 2_1 through 2_10
  for the second template id, etc. if 10 simulations are made from each
  subject in the data template.

- `quiet`:

  Boolean operator controlling whether a model summary report is given.
  Default is `FALSE`.

- `...`:

  Catch deprecated arguments.

#### Details

The Monte Carlo simulator in Pmetrics generates randomly sampled sets of
parameters from the *PRIMARY* block of a model according to a prior
distribution and calculates outputs based upon a template data file. It
is a powerful tool for parametric or semi-parametric sampling. There are
three ways to execute the simulator.

- **PM_result\$sim()**

- **PM_model\$sim()**

- **PM_sim\$new()**

They return fully parsed simulator output as PM_sim objects in R.
[PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
or
[PM_final](https://lapkb.github.io/Pmetrics_rust/reference/PM_final.md)
objects can easily be used as the prior distributions for sampling.
Prior distributions may also be manually specified, useful when
simulating from literature values. Prior distributions may be
unimodal-multivariate (parametric sampling), or multimodal-multivariate
(semi-parametric sampling). For
[PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
or
[PM_final](https://lapkb.github.io/Pmetrics_rust/reference/PM_final.md)
priors, this can be accomplished with the `split` argument. For manual
priors, the `weights` argument in the `poppar` list specifies the
weights for each distribution.

It is also possible to simulate with covariates if they are included as
part of the model. By specifying a covariate list argument, Pmetrics
will first calculate the correlation matrix between the covariates and
if possible the Bayesian posterior parameter values for each subject in
the population model. Using either the mean and standard deviation of
each covariate in the population, or a user-specified mean and/or
standard deviation, Pmetrics will then calculate an augmented covariance
matrix to be used in simulations. Pmetrics will make a copy of the model
file with all covariates moved into the primary block as parameters to
be simulated.

Noise can be applied to most columns in the data template, typically
simulated observations, observation times, dose times, or dose amounts.

Limits on the simulated parameter sets can also be specified using the
limits on primary parameters in the model file or by specifying them
manually as an argument. Limits can also be applied to simulated
covariates.

The same model and data structures are used for the simulator as for any
other Pmetrics functions. In this case, the data object will serve as
the template for the information regarding dosing, covariate values, and
observations. Template data may have more than one subject in them, in
which case the simulator will use each subject specified by the
`include` argument (default is all subjects) to generate `nsim`
parameter sets and corresponding observations.

Simulator output is returned as a PM_sim object. Output may also be
directed to a new Pmetrics .csv data file using the `makecsv` argument.

#### Returns

A PM_sim object.

#### Examples

    \dontrun{
    # Load results of NPAG run
    run1 <- PM_load(1)

    # Two methods to simulate
    # The first uses the population prior, data, and model in run1, with "..."
    # as additional parameters passed to the simulator, e.g. limits, nsim, etc.

    sim1 <- run1$sim(...)

    # The second uses the population prior and model in run1, and a new template
    # data file in the working directory

    sim2 <- PM_sim$new(poppar = run1, data = "newfile.csv", ...)

    # These methods are entirely interchangeable. The first can accept a different
    # data template. The difference is that poppar must be explicitly
    # declared when using PM_sim$new. This makes it the method to use when poppar
    # is derived from the literature.

    # An example of a manual prior
    # make 1 lognormal distribution for each parameter
    weights <- 1
    mean <- log(c(0.7, 0.05, 100))
    cov <- matrix(rep(0, length(mean)**2), ncol = length(mean))
    diag(cov) <- (c(0.15, 0.15, 0.15) * mean)**2
    # make the prior for the simulation
    poppar <- list(weights = weights, mean = mean, mat = cov)

    # run simulation, assuming temp1.csv and model.txt are in working directory

    sim1 <- PM_sim$new(poppar, "temp1.csv",
      nsim = 15, model = "model.txt", include = 1:4, limits = NA,
      noise = list(out = list(coeff = c(0.02, 0.1, 0, 0)))
    )

    # alternatively, load the model first

    mod <- PM_model$new("model.txt")

    # and then simulate

    sim2 <- mod$sim(poppar = poppar, data = "temp1.csv",
       nsim = 15, include = 1:4, limits = NA,
       noise = list(out = list(coeff = c(0.02, 0.1, 0, 0)))
    )

    }

------------------------------------------------------------------------

### Method [`save()`](https://rdrr.io/r/base/save.html)

**\[stable\]** Save the current PM_sim object into a .rds file.

#### Usage

    PM_sim$save(file_name = "PMsim.rds")

#### Arguments

- `file_name`:

  Name of the file to be created, the default is PMsim.rds

------------------------------------------------------------------------

### Method [`plot()`](https://rdrr.io/r/graphics/plot.default.html)

**\[stable\]** Plot `PM_sim` object.

#### Usage

    PM_sim$plot(...)

#### Arguments

- `...`:

  Arguments passed to
  [plot.PM_sim](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_sim.md).

- `at`:

  Index of the PM_sim object to be plotted. Default is 1. result.

------------------------------------------------------------------------

### Method `pta()`

**\[stable\]** Estimates the Probability of Target Attaintment (PTA),
based on the results of the current Simulation.

#### Usage

    PM_sim$pta(...)

#### Arguments

- `...`:

  Additional parameters, refer to
  [PM_pta](https://lapkb.github.io/Pmetrics_rust/reference/PM_pta.md).

------------------------------------------------------------------------

### Method `auc()`

**\[stable\]** Calculates the AUC of the specified simulation

#### Usage

    PM_sim$auc(...)

#### Arguments

- `...`:

  Arguments passed to
  [makeAUC](https://lapkb.github.io/Pmetrics_rust/reference/makeAUC.md).

------------------------------------------------------------------------

### Method [`summary()`](https://rdrr.io/r/base/summary.html)

**\[stable\]** Summarize simulation

#### Usage

    PM_sim$summary(...)

#### Arguments

- `...`:

  Parameters passed to
  [summary.PM_sim](https://lapkb.github.io/Pmetrics_rust/reference/summary.PM_sim.md).

------------------------------------------------------------------------

### Method `run()`

**\[deprecated\]** Deprecated method to run a simulation. Replaced by
`PM_sim$new()` to be consistent with R6.

#### Usage

    PM_sim$run(...)

#### Arguments

- `...`:

  Not used.

------------------------------------------------------------------------

### Method [`load()`](https://rdrr.io/r/base/load.html)

**\[deprecated\]** Deprecated method to load a prior simulation.
Replaced by `PM_sim$new()` to be consistent with R6.

#### Usage

    PM_sim$load(...)

#### Arguments

- `...`:

  Not used.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PM_sim$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
## ------------------------------------------------
## Method `PM_sim$new`
## ------------------------------------------------

if (FALSE) { # \dontrun{
# Load results of NPAG run
run1 <- PM_load(1)

# Two methods to simulate
# The first uses the population prior, data, and model in run1, with "..."
# as additional parameters passed to the simulator, e.g. limits, nsim, etc.

sim1 <- run1$sim(...)

# The second uses the population prior and model in run1, and a new template
# data file in the working directory

sim2 <- PM_sim$new(poppar = run1, data = "newfile.csv", ...)

# These methods are entirely interchangeable. The first can accept a different
# data template. The difference is that poppar must be explicitly
# declared when using PM_sim$new. This makes it the method to use when poppar
# is derived from the literature.

# An example of a manual prior
# make 1 lognormal distribution for each parameter
weights <- 1
mean <- log(c(0.7, 0.05, 100))
cov <- matrix(rep(0, length(mean)**2), ncol = length(mean))
diag(cov) <- (c(0.15, 0.15, 0.15) * mean)**2
# make the prior for the simulation
poppar <- list(weights = weights, mean = mean, mat = cov)

# run simulation, assuming temp1.csv and model.txt are in working directory

sim1 <- PM_sim$new(poppar, "temp1.csv",
  nsim = 15, model = "model.txt", include = 1:4, limits = NA,
  noise = list(out = list(coeff = c(0.02, 0.1, 0, 0)))
)

# alternatively, load the model first

mod <- PM_model$new("model.txt")

# and then simulate

sim2 <- mod$sim(poppar = poppar, data = "temp1.csv",
   nsim = 15, include = 1:4, limits = NA,
   noise = list(out = list(coeff = c(0.02, 0.1, 0, 0)))
)

} # }
```
