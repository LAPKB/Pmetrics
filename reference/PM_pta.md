# Create Probability of Target Attainment (PTA) object

**\[stable\]**

This object class contains results of simulations and a probability of
target attainment analysis.

## Details

There are two ways of creating a *PM_pta* object.

- **PM_sim\$pta()** This way uses the simulation method directly from a
  [PM_sim](https://lapkb.github.io/Pmetrics_rust/reference/PM_sim.md)
  object.

- **PM_pta\$new()** This way takes an external
  [PM_sim](https://lapkb.github.io/Pmetrics_rust/reference/PM_sim.md)
  result as an argument and creates the PTA. It is described here.

Both methods require the prior creation of a simulation of appropriate
regimens.

## See also

[plot.PM_pta](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_pta.md),
[PM_sim](https://lapkb.github.io/Pmetrics_rust/reference/PM_sim.md)

## Author

Julian Otalvaro and Michael Neely

Michael Neely and Jan Strojil

## Public fields

- `data`:

  Contains the raw results.

## Methods

### Public methods

- [`PM_pta$new()`](#method-PM_pta-new)

- [`PM_pta$save()`](#method-PM_pta-save)

- [`PM_pta$summary()`](#method-PM_pta-summary)

- [`PM_pta$plot()`](#method-PM_pta-plot)

- [`PM_pta$load()`](#method-PM_pta-load)

- [`PM_pta$clone()`](#method-PM_pta-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `PM_pta` object.

#### Usage

    PM_pta$new(
      simdata,
      simlabels,
      target,
      target_type,
      success,
      outeq = 1,
      free_fraction = 1,
      start = 0,
      end = Inf,
      icen = "median",
      block = 1,
      ...
    )

#### Arguments

- `simdata`:

  Can be one of multiple inputs as shown in the examples below using.

  - `simEx` *PM_sim*

  - `simEx$data` *PM_simlist*

  - `simEx$data[[1]]` *PM_sim_data*

  - `NPex$post` *PM_post*

  - `NPex$post$data` *PM_post_data*

  - `NPex$data` *PM_data*

  - `NPex$data$standard_data` *PM_data_data*

  - `"simout.txt"` matches files with wildcard ability, see
    [PM_sim](https://lapkb.github.io/Pmetrics_rust/reference/PM_sim.md)

- `simlabels`:

  Optional character vector of labels for each simulation. Default is
  `c('Regimen 1', 'Regimen 2',...)`.

- `target`:

  One of several options.

  - A vector of pharmacodynamic targets, such as Minimum Inhibitory
    Concentrations (MICs), e.g. `c(0.25, 0.5, 1, 2, 4, 8, 16, 32)`.

  - A single numerical value such as a concentration, e.g. 10.

  - A sampled distribution using
    [makePTAtarget](https://lapkb.github.io/Pmetrics_rust/reference/makePTAtarget.md).

  - A list of multiple targets combining the above if multiple
    `target_type`s are used. If so, the first `target` can be a vector,
    but subsequent targets must be single values to avoid factorial
    expansion of combinations. For example, the first target could be a
    vector of MICs corresponding to a `target_type` of "time", the
    second target a value of 10 corresponding to a `target_type` of
    "min", and the third target a value of 50 corresponding to a
    `target_type` of "max" as in this example:
    `target = list(c(0.25, 0.5, 1, 2, 4, 8, 16, 32), 10, 50)`. The first
    value can also be a sampled distribution made with
    [makePTAtarget](https://lapkb.github.io/Pmetrics_rust/reference/makePTAtarget.md).

- `target_type`:

  A vector of the type for each `target`. For any, place a minus sign in
  front to make the success less than the target ratio, e.g.
  `target_type = c("min", "-min")`. Available types:

  - "time" is percent time above `target` within the time range
    specified by `start` and `end`. Use "-time" for percent time below
    `target`.

  - "auc" is ratio of area under the curve within the time range to
    `target`

  - "peak" or "max", ratio of peak or max (synonymous) concentration to
    `target` within the time range. Use "-max" or "-peak" to make the
    success less than the target ratio.

  - "min", is the ratio of minimum concentration to `target` within the
    time range. Use "-min" to make the success less than the target
    ratio.

  - A single numeric value, which must correspond to an observation time
    common to all PMsim objects in `simdata`, rounded to the nearest
    hour. In this case, the target statistic will be the ratio of
    observation at that time to `target`.

  This enables testing of a specific timed concentration (e.g. one hour
  after a dose or C1). Be sure that the time in the simulated data is
  used, e.g., 122 after a dose given at 120. As for the other target
  types, make the number negative to make the success less than the
  target ratio, eg -122.

- `success`:

  A vector specifying the success statistics, e.g. 0.4 for proportion
  time (end-start) above target, and/or 100 for max:target. For example
  `success = 0.4` or `success = c(0.4, 100)`. The length must be the
  same as for `target` and `target_type`.

- `outeq`:

  An integer specifying the number of the simulated output equation to
  use. Default is 1.

- `free_fraction`:

  Proportion of free, active drug, expressed as a numeric value \>=0 and
  \<=1. Default is 1, i.e., 100% free drug or 0% protein binding.

- `start`:

  Specify the time to begin PTA calculations. Default is a vector with
  the first observation time for subjects in each element of `simdata`,
  e.g. dose regimen. If specified as a vector, values will be recycled
  as necessary.

- `end`:

  Specify the time to end PTA calculations so that PTA is calculated
  from `start` to `end`. Default for end is the maximum observation time
  for subjects in each element of `simdata`, e.g. dose regimen. If
  specified as a vector, values will be recycled as necessary. Subjects
  with insufficient data (fewer than 5 simulated observations) for a
  specified interval will trigger a warning. Ideally then, the simulated
  datset should contain sufficient observations within the interval
  specified by `start` and `end`.

- `icen`:

  Can be either "median" for the predictions based on medians of
  `pred.type` parameter value distributions, or "mean". Default is
  "median".

- `block`:

  Which block to plot, where a new block is defined by dose resets (evid
  = 4); default is 1.

- `...`:

  Not currently used

#### Details

This function will calculate the probability of target attainment (PTA)
for any number of simulations, targets and definitions of success.
Simulations typically differ by dose, but may differ by other features
such as children vs. adults.

#### Returns

A list of class *PM_pta_data*, included in the `data` field of the
PM_pta object. The list contains each `target_type` as an element,
followed by a final `intersection` element showing the results for
profiles which meet ALL the conditions (intersection) or `NA` if only
one `target_type` was specified. The individual elements are tibbles
with all possible combinations of `target`s and simulated regimens for a
given `target_type`. The tibbles have the following columns:

- **reg_num** The simulation number in `simdata`.

- **label** Annotation of the simulation, supplied by the `simlabels`
  argument.

- **target** is the specified `target` for the results row. If a
  distribution created by
  [makePTAtarget](https://lapkb.github.io/Pmetrics_rust/reference/makePTAtarget.md),
  this will be a tibble with the simulated targets

- **type** is the specified `target_type` for the results row

- **success_ratio** The specified `success` metric for the results row

- **prop_success** The proportion of profiles meeting the
  `success_ratio` for the results row

- **success** A tibble of success (1) or not (0) for each profile for
  the results row

- **pdi** A tibble of the pharmacodynamic index, i.e. the ratio or time
  above for each profile for the results row

- **start** The start time used for the results row

- **end** The end time used for the results row. For the `$intersect`
  item in the return list, the columns are the same, but the `target`
  and `target_type` will reflect all requested values expressed in
  parenthetical multiplication format to emphasize intersection, e.g.,
  (auc)(min). Simulated (rather than discrete) targets made with
  [makePTAtarget](https://lapkb.github.io/Pmetrics_rust/reference/makePTAtarget.md)
  will be abbreviated as "(sim)", e.g. (sim)(5) for a combination of
  simulated targets and a single concentration target of 5.

#### Examples

    \dontrun{
    pta1 <- PM_pta$new(simEx,
                     simlabels = c("600 mg daily", "1200 mg daily", "300 mg bid", "600 mg bid"),
                     target = list(2^(-2:6), 1, 50),
                     target_type = c("time", 144, "-max"),
                     success = c(0.6, 1, 1),
                     start = 120, end = 144)

    pta2 <- PM_pta$new(simEx,
                     target = c(2^(-2:6)),
                     target_type = "time",
                     success = 0.6,
                     start = 120, end = 144)

    pta3 <- PM_pta$new(simEx,
                     simlabels = c("600 mg daily", "1200 mg daily", "300 mg bid", "600 mg bid"),
                     target = list(5,10),
                     target_type = c("min", "-min"),
                     success = c(1,1),
                     start = 120, end = 144)

    pta4 <- PM_pta$new(simEx,
                     simlabels = c("600 mg daily", "1200 mg daily", "300 mg bid", "600 mg bid"),
                     target = makePTAtarget(mic1),
                     target_type = "auc",
                     success = 200,
                     start = 120, end = 144)

    pta5 <- PM_pta$new(simdata = simEx,
                     simlabels = c("600 mg daily", "1200 mg daily", "300 mg bid", "600 mg bid"),
                     target = list(makePTAtarget(mic1),5),
                     target_type = c("auc","min"),
                     success = c(200,1),
                     start = 120, end = 144)
    }

------------------------------------------------------------------------

### Method [`save()`](https://rdrr.io/r/base/save.html)

Save the current PM_pta object into a .rds file.

#### Usage

    PM_pta$save(file_name = "PMpta.rds")

#### Arguments

- `file_name`:

  Name of the file to be created, the default is PMpta.rds

------------------------------------------------------------------------

### Method [`summary()`](https://rdrr.io/r/base/summary.html)

Summarize the `PM_pta` object. See
[summary.PM_pta](https://lapkb.github.io/Pmetrics_rust/reference/summary.PM_pta.md).

#### Usage

    PM_pta$summary(...)

#### Arguments

- `...`:

  Arguments passed to
  [summary.PM_pta](https://lapkb.github.io/Pmetrics_rust/reference/summary.PM_pta.md)

------------------------------------------------------------------------

### Method [`plot()`](https://rdrr.io/r/graphics/plot.default.html)

Plot the `PM_pta` object. See
[plot.PM_pta](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_pta.md).

#### Usage

    PM_pta$plot(...)

#### Arguments

- `...`:

  Arguments passed to
  [plot.PM_pta](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_pta.md)

------------------------------------------------------------------------

### Method [`load()`](https://rdrr.io/r/base/load.html)

**\[deprecated\]**

Deprecated method to load a prior PTA Replaced by `PM_pta$new()` to be
consistent with R6.

#### Usage

    PM_pta$load(...)

#### Arguments

- `...`:

  Not used.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PM_pta$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
## ------------------------------------------------
## Method `PM_pta$new`
## ------------------------------------------------

if (FALSE) { # \dontrun{
pta1 <- PM_pta$new(simEx,
                 simlabels = c("600 mg daily", "1200 mg daily", "300 mg bid", "600 mg bid"),
                 target = list(2^(-2:6), 1, 50),
                 target_type = c("time", 144, "-max"),
                 success = c(0.6, 1, 1),
                 start = 120, end = 144)

pta2 <- PM_pta$new(simEx,
                 target = c(2^(-2:6)),
                 target_type = "time",
                 success = 0.6,
                 start = 120, end = 144)

pta3 <- PM_pta$new(simEx,
                 simlabels = c("600 mg daily", "1200 mg daily", "300 mg bid", "600 mg bid"),
                 target = list(5,10),
                 target_type = c("min", "-min"),
                 success = c(1,1),
                 start = 120, end = 144)

pta4 <- PM_pta$new(simEx,
                 simlabels = c("600 mg daily", "1200 mg daily", "300 mg bid", "600 mg bid"),
                 target = makePTAtarget(mic1),
                 target_type = "auc",
                 success = 200,
                 start = 120, end = 144)

pta5 <- PM_pta$new(simdata = simEx,
                 simlabels = c("600 mg daily", "1200 mg daily", "300 mg bid", "600 mg bid"),
                 target = list(makePTAtarget(mic1),5),
                 target_type = c("auc","min"),
                 success = c(200,1),
                 start = 120, end = 144)
} # }
```
