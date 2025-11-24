# Optimal Sample Times

**\[stable\]**

Contains optimal sampling times for a given model and dosage regimen.

## Details

This object contains the methods to create and results from optimal
sampling algorithms. Currently the only option multiple-model
optimization. This algorithm calculates the requested number of sample
times where the concentration time profiles are the most separated,
thereby minimizing the risk of choosing the incorrect Bayesian posterior
for an individual. Future updates will add D-optimal sampling times.

## References

Bayard, David S. & and Neely, Michael. (2017). Experiment Design for
Nonparametric Models Based on Minimizing Bayes Risk: Application to
Voriconazole. *Journal of Pharmacokinetics and Pharmacodynamics*,
**44**(2): 95–111. <https://doi.org/10.1007/s10928-016-9498-5>.

## Author

Michael Neely

## Public fields

- `sampleTime`:

  The optimal sample times, based on requested `type` argument to `$new`
  creation method.

- `bayesRisk`:

  Only present for MM-optimal sampling. The Bayesian risk of
  mis-classifying a subject based on the sample times. This is more
  useful for comparisons between sampling strategies, with minimization
  the goal.

- `simdata`:

  A [PM_sim](https://lapkb.github.io/Pmetrics_rust/reference/PM_sim.md)
  object with the simulated profiles

- `mmInt`:

  A list with the `mmInt` values, `NULL` if `mmInt` argument to `$new`
  is missing.

## Methods

### Public methods

- [`PM_opt$new()`](#method-PM_opt-new)

- [`PM_opt$plot()`](#method-PM_opt-plot)

- [`PM_opt$print()`](#method-PM_opt-print)

- [`PM_opt$clone()`](#method-PM_opt-clone)

------------------------------------------------------------------------

### Method `new()`

**\[stable\]**

Determine optimal sample times which are the most informative about the
model parameters.

#### Usage

    PM_opt$new(
      poppar,
      model,
      data,
      nsamp = 1,
      weight = list(none = 1),
      predInt = 0.5,
      mmInt,
      algorithm = "mm",
      outeq = 1,
      ...
    )

#### Arguments

- `poppar`:

  There are several choices for the population parameters.

  - A
    [PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
    loaded with
    [PM_load](https://lapkb.github.io/Pmetrics_rust/reference/PM_load.md),
    in which case the `$final` field will be used, e.g.
    `run1 <- PM_load(1)` and `poppar = run1`.

  - A
    [PM_final](https://lapkb.github.io/Pmetrics_rust/reference/PM_final.md)
    object, typically as a field in a
    [PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md),
    e.g., `poppar = run1$final`.

  - A list containing three items in this order, but of any name: vector
    of weights, vector of mean parameter values, and a covariance
    matrix. If only one distribution is to be specified the `weights`
    vector should be of length 1 and contain a 1. If multiple
    distributions are to be sampled, the `weights` vector should be of
    length equal to the number of distributions and its values should
    sum to 1, e.g. `c(0.25,0.05,0.7)`. The means matrix may be a vector
    for a single distribution, or a matrix with `length(weights)` rows
    and number of columns equal to the number of parameters.

- `model`:

  One of three choices:

  - [PM_model](https://lapkb.github.io/Pmetrics_rust/reference/PM_model.md)
    object

  - Character vector with the filename of a Pmetrics model in the
    working directory.

  - If `model` is missing, there are two possibilities: \*\* When
    `poppar` is a
    [PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
    with a valid `$model` field, the model in `poppar` will be used.
    \*\* In the absence of a `poppar` with a valid model field, look for
    a file called "model.txt" in the working directory.

- `data`:

  One of three choices:

  - [PM_data](https://lapkb.github.io/Pmetrics_rust/reference/PM_data.md)
    object

  - Character vector with the filename of a Pmetrics data in the working
    directory.

  - If `data` is missing, there are two possibilities: \*\* When
    `poppar` is a
    [PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
    with a valid `$data` field, the data in `poppar` will be used. \*\*
    In the absence of a `poppar` with a valid data field, look for a
    file called "data.csv" in the working directory. In either choice,
    the value for outputs can be coded as any number(s) other than -99.
    The number(s) will be replaced in the simulator output with the
    simulated values.

- `nsamp`:

  The number of MM-optimal sample times to compute; default is 1, but
  can be any number. Values \>4 will take an exponentially longer time.

- `weight`:

  List whose names indicate the type of weighting, and values indicate
  the relative weight. Values should sum to 1. Names can be any of the
  following:

  - **none** The default. MMopt times will be chosen to maximally
    discriminate all responses at all times.

  - **AUC** MMopt times will be chosen to maximally discriminate AUC,
    regardless of the shape of the response profile.

  - **max** MMopt times will be chosen to maximally discriminate
    maximum, regardless of the shape of the response profile.

  - **min** MMopt times will be chosen to maximally discriminate
    minimum, regardless of the shape of the response profile.

  Any combination of AUC, max, and min can be chosen. If "none" is
  specified, other weight types will be ignored and the relative value
  will be set to 1. For example,`list(auc = 0.5,max = 0.5)` or
  `list(auc = 0.2, min = 0.8)`. The default is `list(none = 1)`.

- `predInt`:

  The interval in fractional hours for simulated predicted outputs at
  times other than those specified in the template `data`. The default
  is 0.5, which means there will be simulated outputs every 30 minutes
  from time 0 up to the maximal time in the template file. You may also
  specify `predInt` as a vector of 3 values, e.g. `c(1,4,1)`, similar to
  the R command [seq](https://rdrr.io/r/base/seq.html), where the first
  value is the start time, the second is the stop time, and the third is
  the step value. Outputs for times specified in the template file will
  also be simulated. To simulate outputs *only* at the output times in
  the template data (i.e. EVID=0 events), use `predInt = 0`. Note that
  the maximum number of predictions total is 594, so the interval must
  be sufficiently large to accommodate this for a given number of output
  equations and total time to simulate over. If `predInt` is set so that
  this cap is exceeded, predictions will be truncated.

- `mmInt`:

  Specify the time intervals from which MMopt times can be selected.
  These should only include simulated times specified by `predInt`.

- `algorithm`:

  Optimal sampling algorithm. Currently not modifiable and the only
  option is "mm".

- `outeq`:

  Output equation to optimize

- `...`:

  Other parameters to pass to
  [PM_sim](https://lapkb.github.io/Pmetrics_rust/reference/PM_sim.md)\$new().
  Most are not necessary, but `usePost = TRUE` can be used to calculate
  individual MMopt times. In this case, the number of posterior
  distributions contained in `poppar$final$postPoints` needs to match
  the number of subjects in `data`. You can also pass `include` and
  `exclude` to limit the subjects used in `data`. This will work whether
  `usePost` is `TRUE` or `FALSE`. Note that the following arguments to
  [PM_sim](https://lapkb.github.io/Pmetrics_rust/reference/PM_sim.md)\$new
  cannot be modified.

  - `nsim` is zero

  - `outname` is "MMsim"

  - `combine` is `TRUE`

- `clean`:

  Boolean parameter to specify whether temporary files made in the
  course of the simulation run should be deleted. Defaults to `TRUE`.
  This is primarily used for debugging.

#### Details

Currently, the only option is the multiple-model optimization algorithm.

------------------------------------------------------------------------

### Method [`plot()`](https://rdrr.io/r/graphics/plot.default.html)

Plot method

#### Usage

    PM_opt$plot(...)

#### Arguments

- `...`:

  Arguments passed to
  [plot.PM_opt](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_opt.md)

#### Details

See
[plot.PM_opt](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_opt.md).

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method

#### Usage

    PM_opt$print()

#### Returns

Prints the optimal sampling times and Bayes Risk.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PM_opt$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
