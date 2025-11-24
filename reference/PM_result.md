# Results of a Pmetrics run

**\[stable\]**

This object contains all of the results after a Pmetrics runs. It is
created by using the
[PM_load](https://lapkb.github.io/Pmetrics_rust/reference/PM_load.md)
function.

## Details

After a run completes, results are stored on your hard drive. They are
loaded back into R with
[PM_load](https://lapkb.github.io/Pmetrics_rust/reference/PM_load.md) to
create the PM_result object, which contains both the results and
functions to analyze or plot the result.

## Author

Michael Neely, Julian Otalvaro

## Public fields

- `pop`:

  A [PM_pop](https://lapkb.github.io/Pmetrics_rust/reference/PM_pop.md)
  object

- `post`:

  A
  [PM_post](https://lapkb.github.io/Pmetrics_rust/reference/PM_post.md)
  object

- `final`:

  A
  [PM_final](https://lapkb.github.io/Pmetrics_rust/reference/PM_final.md)
  object

- `cycle`:

  A
  [PM_cycle](https://lapkb.github.io/Pmetrics_rust/reference/PM_cycle.md)
  object

- `op`:

  A [PM_op](https://lapkb.github.io/Pmetrics_rust/reference/PM_op.md)
  object

- `cov`:

  A [PM_cov](https://lapkb.github.io/Pmetrics_rust/reference/PM_cov.md)
  object

- `data`:

  [PM_data](https://lapkb.github.io/Pmetrics_rust/reference/PM_data.md)
  object representing the original .csv data file used in the run. The
  predictions contained in the `$data` fields from `$pop` and `$post`
  will be added to this
  [PM_data](https://lapkb.github.io/Pmetrics_rust/reference/PM_data.md)
  object to permit easy addition of such predictions to raw data plots.
  See
  [plot.PM_data](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_data.md)
  for more details.

- `model`:

  text string representing the original model file used in the run

- `errfile`:

  Name of error file if it exists

- `success`:

  Boolean if successful run

- `valid`:

  If the `$validate` method has been executed after a run, this field
  will contain the information required to plot visual predictive checks
  and normalized prediction error discrepancies via the npde code
  developed by Comets et al. Use the `$save` method on the augmented
  `PM_result` object to save it with the new validation results.

- `opt_samp`:

  If the `$opt` method has been executed after a run, this field will
  contain a
  [PM_opt](https://lapkb.github.io/Pmetrics_rust/reference/PM_opt.md)
  object which has optimal sampling times and methods to plot them. Use
  the `$save` method on the augmented `PM_result` object to save it with
  the new optimal sampling results.

## Methods

### Public methods

- [`PM_result$new()`](#method-PM_result-new)

- [`PM_result$fit()`](#method-PM_result-fit)

- [`PM_result$plot()`](#method-PM_result-plot)

- [`PM_result$summary()`](#method-PM_result-summary)

- [`PM_result$auc()`](#method-PM_result-auc)

- [`PM_result$nca()`](#method-PM_result-nca)

- [`PM_result$report()`](#method-PM_result-report)

- [`PM_result$sim()`](#method-PM_result-sim)

- [`PM_result$save()`](#method-PM_result-save)

- [`PM_result$validate()`](#method-PM_result-validate)

- [`PM_result$step()`](#method-PM_result-step)

- [`PM_result$opt()`](#method-PM_result-opt)

- [`PM_result$load()`](#method-PM_result-load)

- [`PM_result$clone()`](#method-PM_result-clone)

------------------------------------------------------------------------

### Method `new()`

Create new object populated with data from previous run

#### Usage

    PM_result$new(out, path = ".", quiet = TRUE)

#### Arguments

- `out`:

  The parsed output from
  [PM_load](https://lapkb.github.io/Pmetrics_rust/reference/PM_load.md),
  which is automatically generated. This is not a user-modifiable.

- `path`:

  include Path to the folder containing the raw results of the run.
  Default is the current working directory. .

- `quiet`:

  Quietly validate. Default is `FALSE`.

#### Details

Creation of new `PM_result` objects is via
[PM_load](https://lapkb.github.io/Pmetrics_rust/reference/PM_load.md).

------------------------------------------------------------------------

### Method [`fit()`](https://lapkb.github.io/Pmetrics_rust/reference/fit.md)

Fit the model to the data \#' @details This method is used to fit the
model in the PM_result object to data. It calls the `$fit` method of the
model stored in the `model` field.

#### Usage

    PM_result$fit(data, ...)

#### Arguments

- `data`:

  Optional data to fit. If not provided, the data stored in the `data`
  field of the PM_result object will be used. This can be useful to
  continue a prior run that did not converge before the maximum number
  of cycles, e.g. `run2 <- run1$fit(cycles = 10000, prior = 1)`

- `...`:

  Additional arguments passed to the model's `$fit` method.

#### Returns

Returns an invisible PM_result.

------------------------------------------------------------------------

### Method [`plot()`](https://rdrr.io/r/graphics/plot.default.html)

Plot generic function based on type

#### Usage

    PM_result$plot(type, ...)

#### Arguments

- `type`:

  Type of plot based on class of object

- `...`:

  Plot-specific arguments

------------------------------------------------------------------------

### Method [`summary()`](https://rdrr.io/r/base/summary.html)

Summary generic function based on type

#### Usage

    PM_result$summary(type, ...)

#### Arguments

- `type`:

  Type of summary based on class of object

- `...`:

  Summary-specific arguments

------------------------------------------------------------------------

### Method `auc()`

AUC generic function based on type

#### Usage

    PM_result$auc(src, ...)

#### Arguments

- `src`:

  Source of AUC, one of "op", "pop", "post", or "sim"

- `...`:

  Summary-specific arguments

------------------------------------------------------------------------

### Method `nca()`

Perform non-compartmental analysis

#### Usage

    PM_result$nca(...)

#### Arguments

- `...`:

  Arguments passed to
  [makeNCA](https://lapkb.github.io/Pmetrics_rust/reference/makeNCA.md).

#### Details

See
[makeNCA](https://lapkb.github.io/Pmetrics_rust/reference/makeNCA.md).

------------------------------------------------------------------------

### Method `report()`

Re-generate the report

#### Usage

    PM_result$report(...)

#### Arguments

- `...`:

  Parameters passed to
  [PM_report](https://lapkb.github.io/Pmetrics_rust/reference/PM_report.md).
  In particular, pay attention to `path`.

------------------------------------------------------------------------

### Method `sim()`

Calls
[PM_sim](https://lapkb.github.io/Pmetrics_rust/reference/PM_sim.md).
Default is to use the `$final`, `$model`, and `$data` objects within the
PM_result. It is common to supply a different `data` template.
Occasionally it is necessary to use a different `model` with the
`$final` field, or vice versa. If all three are different, use
`PM_sim$new()` instead.

#### Usage

    PM_result$sim(...)

#### Arguments

- `...`:

  Parameters passed to
  [PM_sim](https://lapkb.github.io/Pmetrics_rust/reference/PM_sim.md).
  If using the `$final`, `$model`, and `$data` fields, it is not
  necessary to specify these. Alternates for any of these should be
  specified. Other parameters for
  [PM_sim](https://lapkb.github.io/Pmetrics_rust/reference/PM_sim.md)
  should be passed as named arguments, e.g.
  `$sim(include = 1:2, predInt = 1, limits = NA)`.

------------------------------------------------------------------------

### Method [`save()`](https://rdrr.io/r/base/save.html)

Save the current PM_result object to an .Rdata file.

#### Usage

    PM_result$save(run, file = "PMout.Rdata")

#### Arguments

- `run`:

  The run output folder number to save the revised result. If missing,
  will save in the current working directory. For example, if folder "1"
  is in your current working directory, specify `run = 1` to save the
  result to the "outputs" subfolder of the "1" folder.

- `file`:

  Custom file name. Default is "PMout.Rdata". If `run` is not specified,
  `file` should be the full path and filename.

#### Details

This is useful if you have updated the result in some way, for example
you have run the `$validate()` method on the `PM_result` object, which
returns an internal simulation based validation as a new `valid` field.
To save this validation, use this `$save` method. Note that unless a
`file` name is provided, the changes will overwrite the previous run
results, although unchanged items will be preserved. This is the usual
workflow. However, a custom file name may be useful to share the run
results with someone.

The saved object is an .Rdata file. When loaded, it should be assigned
to an R object, e.g. `run2 <- PM_result$new("filename")`. An equivalent
statement would be `run2 <- PM_load(file = "filename")`.

------------------------------------------------------------------------

### Method `validate()`

Validate the result by internal simulation methods.

#### Usage

    PM_result$validate(...)

#### Arguments

- `...`:

  Arguments passed to
  [PM_valid](https://lapkb.github.io/Pmetrics_rust/reference/PM_valid.md).

------------------------------------------------------------------------

### Method [`step()`](https://rdrr.io/r/stats/step.html)

Conduct stepwise linear regression of Bayesian posterior parameter
values and covariates.

#### Usage

    PM_result$step(...)

#### Arguments

- `...`:

  Arguments passed to
  [PM_step](https://lapkb.github.io/Pmetrics_rust/reference/PM_step.md).

------------------------------------------------------------------------

### Method `opt()`

Calculate optimal sampling times.

Method to compute optimal sampling times.

#### Usage

    PM_result$opt(...)

#### Arguments

- `...`:

  Parameters to pass to
  [PM_opt](https://lapkb.github.io/Pmetrics_rust/reference/PM_opt.md).

#### Details

See [PM_opt](https://lapkb.github.io/Pmetrics_rust/reference/PM_opt.md)
for details.

------------------------------------------------------------------------

### Method [`load()`](https://rdrr.io/r/base/load.html)

**\[deprecated\]**

Deprecated method to load prior results saved with the `$save` method.
Replaced by
[PM_load](https://lapkb.github.io/Pmetrics_rust/reference/PM_load.md).

#### Usage

    PM_result$load(...)

#### Arguments

- `...`:

  Not used.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PM_result$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
