# Pmetrics Run Cycle Information

**\[stable\]**

Contains the cycle information after a run.

## Details

The PM_cycle object is both a data field within a
[PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md),
and itself an R6 object comprising data fields and associated methods
suitable for analysis and plotting of cycle information generated during
the run.

Because PM_cycle objects are automatically added to the
[PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
at the end of a successful run, it is generally not necessary for users
to generate PM_cycle objects themselves.

The main results are contained in the `$data` field, and it is this
field which is passed to the `$plot` and `$summary` methods. You can use
this `$data` field for custom manipulations, e.g.
`last <- run1$cycle$data$aic %>% tail(1)`. This will report the last
cycle aic. If you are unfamiliar with the `%>%` pipe function, please
type `help("%>%", "magrittr")` into the R console and look online for
instructions/tutorials in tidyverse, a powerful approach to data
manipulation upon which Pmetrics is built.

To provide a more traditional experience in R, the `$data` field is also
separated by list items into the other data fields within the R6 object,
e.g. `mean` or `gamlam`. This allows you to access them in an S3 way,
e.g. `run1$cycle$mean` if `run1` is a
[PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
object.

## Author

Michael Neely, Julian Otalvaro

## Public fields

- `data`:

  A list with the following elements, which can also be extracted by
  name. e.e. `run1$cycle$objective`, which is equivalent to
  `run1$cycle$data$objective`. **names** Vector of names of the random
  parameters **objective** A tibble of -2\*Log-likelihood, AIC and BIC
  at each cycle **gamlam** A tibble of cycle number and gamma or lambda
  at each cycle for each output equation **mean** A tibble of cycle
  number and the mean of each random parameter at each cycle, normalized
  to initial mean **median** A tibble of cycle number and the median of
  each random parameter at each cycle, normalized to initial median
  **sd** A tibble of cycle number and the standard deviation of each
  random parameter at each cycle, normalized to initial standard
  deviation **status** Status of the last cycle: "Converged", "Maximum
  cycles reached", or "Posterior".

## Active bindings

- `names`:

  Vector of names of the random parameters

- `objective`:

  A tibble of -2\*Log-likelihood, AIC and BIC at each cycle

- `gamlam`:

  A tibble of cycle number and gamma or lambda at each cycle for each
  output equation

- `mean`:

  A tibble of cycle number and the mean of each random parameter at each
  cycle, normalized to initial mean

- `median`:

  A tibble of cycle number and the median of each random parameter at
  each cycle, normalized to initial median

- `sd`:

  A tibble of cycle number and the standard deviation of each random
  parameter at each cycle, normalized to initial standard deviation

- `status`:

  Status of the last cycle: "Converged", "Maximum cycles reached", or
  "Posterior"

## Methods

### Public methods

- [`PM_cycle$new()`](#method-PM_cycle-new)

- [`PM_cycle$plot()`](#method-PM_cycle-plot)

- [`PM_cycle$summary()`](#method-PM_cycle-summary)

- [`PM_cycle$print()`](#method-PM_cycle-print)

- [`PM_cycle$clone()`](#method-PM_cycle-clone)

------------------------------------------------------------------------

### Method `new()`

Create new object populated with cycle information

#### Usage

    PM_cycle$new(PMdata = NULL, path = ".", ...)

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

Creation of new `PM_cycle` object is automatic and not generally
necessary for the user to do.

------------------------------------------------------------------------

### Method [`plot()`](https://rdrr.io/r/graphics/plot.default.html)

Plot method

#### Usage

    PM_cycle$plot(...)

#### Arguments

- `...`:

  Arguments passed to
  [plot.PM_cycle](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_cycle.md)

#### Details

See
[plot.PM_cycle](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_cycle.md).

------------------------------------------------------------------------

### Method [`summary()`](https://rdrr.io/r/base/summary.html)

Summary method

#### Usage

    PM_cycle$summary(...)

#### Arguments

- `...`:

  Arguments passed to
  [summary.PM_cycle](https://lapkb.github.io/Pmetrics_rust/reference/summary.PM_cycle.md)

#### Details

See
[summary.PM_cycle](https://lapkb.github.io/Pmetrics_rust/reference/summary.PM_cycle.md).

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method

#### Usage

    PM_cycle$print()

#### Details

Prints the last cycle summary information.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PM_cycle$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
