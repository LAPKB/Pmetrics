# Individual Bayesian posterior predictions at short intervals

**\[stable\]**

Contains the Bayesian posterior predictions at short intervals specified
as an argument to the \$run method of
[PM_fit](https://lapkb.github.io/Pmetrics_rust/reference/PM_fit.md).
Default is every 12 minutes.

## Details

\#' The PM_post object is both a data field within a
[PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md),
and itself an R6 object comprising data fields and associated methods
suitable for analysis and plotting of posterior predictions generated
during the run.

Because PM_post objects are automatically added to the
[PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
at the end of a successful run, it is generally not necessary for users
to generate PM_post objects themselves.

The main results are contained in the `$data` field, and it is this
field which is passed to the `$plot` and `$summary` methods. data frame
with population predicted outputs for all subjects.

To provide a more traditional experience in R, the data frame is
separated by columns into fields, e.g. `id` or `time`. This allows you
to access them in an S3 way, e.g. `run1$post$time` if `run1` is a
`PM_result` object.

However, if you wish to manipulate the entire data frame, use the `data`
field, e.g. `trough <- run1$post$data %>% filter(time == 24)`. If you
are unfamiliar with the `%>%` pipe function, please type
`help("%>%", "magrittr")` into the R console and look online for
instructions/tutorials in tidyverse, a powerful approach to data
manipulation upon which Pmetrics is built.

## Author

Michael Neely, Julian Otalvaro

## Public fields

- `data`:

  A data frame with the following columns:

  - **id** Subject id

  - **time** Time of predictions in decimal hours

  - **icen** Prediction based on mean or median of Bayesian posterior
    parameter distribution

  - **outeq** Output equation number

  - **pred** Predicted output for each outeq

  - **block** Observation blocks within subjects as defined by *EVID=4*
    dosing events

## Methods

### Public methods

- [`PM_post$new()`](#method-PM_post-new)

- [`PM_post$plot()`](#method-PM_post-plot)

- [`PM_post$summary()`](#method-PM_post-summary)

- [`PM_post$auc()`](#method-PM_post-auc)

- [`PM_post$clone()`](#method-PM_post-clone)

------------------------------------------------------------------------

### Method `new()`

Create new object populated with Bayesian posterior predicted data at
regular, frequent intervals

#### Usage

    PM_post$new(PMdata = NULL, path = ".", ...)

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

Creation of new `PM_post` object is automatic and not generally
necessary for the user to do.

------------------------------------------------------------------------

### Method [`plot()`](https://rdrr.io/r/graphics/plot.default.html)

Plot method

#### Usage

    PM_post$plot(...)

#### Arguments

- `...`:

  Arguments passed to
  [plot.PM_pop](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_pop.md)

#### Details

See
[plot.PM_pop](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_pop.md).

------------------------------------------------------------------------

### Method [`summary()`](https://rdrr.io/r/base/summary.html)

Summary method

#### Usage

    PM_post$summary(...)

#### Arguments

- `...`:

  Arguments passed to
  [summary.PM_pop](https://lapkb.github.io/Pmetrics_rust/reference/summary.PM_pop.md)

#### Details

See
[summary.PM_post](https://lapkb.github.io/Pmetrics_rust/reference/summary.PM_post.md).

------------------------------------------------------------------------

### Method `auc()`

Calculate AUC

#### Usage

    PM_post$auc(...)

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

    PM_post$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
