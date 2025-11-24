# Observed vs. predicted data

**\[stable\]**

Contains observed vs. predicted data after a run, typically a field in a
[PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)

## Details

The PM_op object is both a data field within a
[PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md),
and itself an R6 object comprising data fields and associated methods
suitable for analysis and plotting of observed vs. population or
individual predicted outputs.

Because PM_op objects are automatically added to the
[PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
at the end of a successful run, it is generally not necessary for users
to generate PM_op objects themselves.

The main results are contained in the `$data` field, and it is this
field which is passed to the `$plot` and `$summary` methods. You can use
this `$data` field for custom manipulations, e.g.
`trough <- run1$op$data %>% filter(time == 24)`. If you are unfamiliar
with the `%>%` pipe function, please type `help("%>%", "magrittr")` into
the R console and look online for instructions/tutorials in tidyverse, a
powerful approach to data manipulation upon which Pmetrics is built.

To provide a more traditional experience in R, the `$data` field is also
separated by columns into the other data fields within the R6 object,
e.g. `id` or `time`. This allows you to access them in an S3 way, e.g.
`run1$op$time` if `run1` is a
[PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
object.

## Author

Michael Neely, Julian Otalvaro

## Public fields

- `id`:

  subject identification

- `time`:

  observation time in relative units, usually hours

- `obs`:

  observation

- `cens`:

  censoring information: "none" for observed, "bloq" for below limit of
  quantification, "aloq" for above limit of quantification

- `pred`:

  prediction

- `pred.type`:

  Population predictions based on Bayesian prior parameter value
  distribution, or individual predictions based on Bayesian posterior
  parameter value distributions

- `icen`:

  Predictions based on mean or median of Bayesian `pred.type`parameter
  values

- `outeq`:

  output equation number

- `block`:

  dosing block number for each subject, as defined by dose resets
  (evid=4).

- `obsSD`:

  standard deviation of the observation based on the assay error
  polynomial

- `d`:

  prediction error, `pred` - `obs`

- `ds`:

  squared prediction error

- `wd`:

  weighted prediction error, which is the prediction error divided by
  the `obsSD`

- `wds`:

  weighted squared prediction error

- `data`:

  A data frame of class **PM_op_data** combining all the above fields as
  its columns

## Methods

### Public methods

- [`PM_op$new()`](#method-PM_op-new)

- [`PM_op$plot()`](#method-PM_op-plot)

- [`PM_op$summary()`](#method-PM_op-summary)

- [`PM_op$auc()`](#method-PM_op-auc)

- [`PM_op$clone()`](#method-PM_op-clone)

------------------------------------------------------------------------

### Method `new()`

Create new object populated with observed vs. predicted data

#### Usage

    PM_op$new(PMdata = NULL, path = ".", ...)

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

Creation of new `PM_op` object is automatic at the end of a run and not
generally necessary for the user to do.

------------------------------------------------------------------------

### Method [`plot()`](https://rdrr.io/r/graphics/plot.default.html)

Plot method

#### Usage

    PM_op$plot(...)

#### Arguments

- `...`:

  Arguments passed to
  [plot.PM_op](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_op.md)

#### Details

See
[plot.PM_op](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_op.md).

------------------------------------------------------------------------

### Method [`summary()`](https://rdrr.io/r/base/summary.html)

Summary method

#### Usage

    PM_op$summary(...)

#### Arguments

- `...`:

  Arguments passed to
  [summary.PM_op](https://lapkb.github.io/Pmetrics_rust/reference/summary.PM_op.md)

#### Details

See
[summary.PM_op](https://lapkb.github.io/Pmetrics_rust/reference/summary.PM_op.md).

------------------------------------------------------------------------

### Method `auc()`

Calculate AUC

#### Usage

    PM_op$auc(...)

#### Arguments

- `...`:

  Arguments passed to
  [makeAUC](https://lapkb.github.io/Pmetrics_rust/reference/makeAUC.md)

- `data`:

  The object to use for AUC calculation

#### Details

See
[makeAUC](https://lapkb.github.io/Pmetrics_rust/reference/makeAUC.md)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PM_op$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
