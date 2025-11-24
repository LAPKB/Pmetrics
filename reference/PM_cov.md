# Contains covariate data

**\[stable\]**

Contains a data frame with subject-specific covariate data output.

## Details

The PM_cov object is both a data field within a
[PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md),
and itself an R6 object comprising data fields and associated methods
suitable for analysis and plotting relationships between covariates and
posterior parameters, covariates over time, or parameter values over
time.

Because PM_cov objects are automatically added to the
[PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
at the end of a successful run, it is generally not necessary for users
to generate PM_cov objects themselves.

The results are contained in the `$data` field, and it is this field
which is passed to the `$plot` and `$summary` methods. You can use this
`$data` field for custom manipulations, although usually this is better
done on the `summary` object, e.g.,
`run1$cov$summary() %>% select(africa, gender) %>% table()`. If you are
unfamiliar with the `%>%` pipe function, please type
`help("%>%", "magrittr")` into the R console and look online for
instructions/tutorials in tidyverse, a powerful approach to data
manipulation upon which Pmetrics is built.

This output of this function is suitable for exploration of covariate-
parameter, covariate-time, or parameter-time relationships.

## See also

[plot.PM_cov](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_cov.md),
[PM_step](https://lapkb.github.io/Pmetrics_rust/reference/PM_step.md)

## Author

Michael Neely, Julian Otalvaro

## Public fields

- `data`:

  A data frame with the following columns

  - id Subject identification

  - time Times of covariate observations

  - covnames... Columns with each covariate observations in the dataset
    for each subject and `time`

  - parnames... Columns with each parameter in the model and the `icen`
    summary for each subject, replicated as necessary for covariate
    observation times and duplicated for Bayesian parameter means and
    medians

  - icen The type of summarized Bayesian posterior individual parameter
    values: mean or median

## Methods

### Public methods

- [`PM_cov$new()`](#method-PM_cov-new)

- [`PM_cov$step()`](#method-PM_cov-step)

- [`PM_cov$summary()`](#method-PM_cov-summary)

- [`PM_cov$plot()`](#method-PM_cov-plot)

- [`PM_cov$print()`](#method-PM_cov-print)

- [`PM_cov$clone()`](#method-PM_cov-clone)

------------------------------------------------------------------------

### Method `new()`

Create new object populated with covariate-parameter information

#### Usage

    PM_cov$new(PMdata = NULL, path = ".", ...)

#### Arguments

- `PMdata`:

  include Saved, parsed output of prior run, used when source files are
  not available. .

- `path`:

  include Path to the folder containing the raw results of the run.
  Default is the current working directory. .

- `...`:

  Not currently used.

#### Details

Creation of new `PM_cov` object is automatic and not generally necessary
for the user to do.

------------------------------------------------------------------------

### Method [`step()`](https://rdrr.io/r/stats/step.html)

Stepwise linear regression of covariates and Bayesian posterior
parameter values

#### Usage

    PM_cov$step(...)

#### Arguments

- `...`:

  Arguments passed to
  [PM_step](https://lapkb.github.io/Pmetrics_rust/reference/PM_step.md)

#### Details

See
[PM_step](https://lapkb.github.io/Pmetrics_rust/reference/PM_step.md).

------------------------------------------------------------------------

### Method [`summary()`](https://rdrr.io/r/base/summary.html)

Summary method

#### Usage

    PM_cov$summary(...)

#### Arguments

- `...`:

  Arguments passed to
  [summary.PM_cov](https://lapkb.github.io/Pmetrics_rust/reference/summary.PM_cov.md)

#### Details

See
[summary.PM_cov](https://lapkb.github.io/Pmetrics_rust/reference/summary.PM_cov.md).

------------------------------------------------------------------------

### Method [`plot()`](https://rdrr.io/r/graphics/plot.default.html)

Plot method

#### Usage

    PM_cov$plot(...)

#### Arguments

- `...`:

  Arguments passed to
  [plot.PM_cov](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_cov.md)

#### Details

See
[plot.PM_cov](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_cov.md).

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method

#### Usage

    PM_cov$print(...)

#### Arguments

- `...`:

  Arguments passed to [print](https://rdrr.io/r/base/print.html)

#### Details

Print method for PM_cov

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PM_cov$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
