# Calculation of AUCs

**\[stable\]**

Calculates AUC from a variety of inputs

## Usage

``` r
makeAUC(
  data,
  formula,
  include,
  exclude,
  start = 0,
  end = Inf,
  icen = "median",
  outeq = 1,
  block = 1,
  method = "linear",
  addZero = F
)
```

## Arguments

- data:

  A suitable data object, i.e.
  [PM_pop](https://lapkb.github.io/Pmetrics_rust/reference/PM_pop.md),
  [PM_post](https://lapkb.github.io/Pmetrics_rust/reference/PM_post.md),
  [PM_op](https://lapkb.github.io/Pmetrics_rust/reference/PM_op.md),
  [PM_sim](https://lapkb.github.io/Pmetrics_rust/reference/PM_sim.md),
  or some other suitable dataframe with at least time/observation
  columns referred to by `formula`.

- formula:

  A formula of the form `obs ~ time | group`. Default value for `group`
  is "id", so when if the data contain an "id" column, the formula can
  simply be `obs ~ time`. If the data contain a grouping variable, it
  can be specified as `obs ~ time | group`, where `group` is the name of
  the grouping variable. This is only required with data that is not of
  class PM_pop, PM_post, PM_op or PM_sim.

- include:

  A vector of subject IDs to include in the plot, e.g. c(1:3,5,15)

- exclude:

  A vector of subject IDs to exclude in the plot, e.g. c(4,6:14,16:20)

- start:

  Specify the time to begin AUC calculations. Default is 0.

- end:

  Specify the time to end AUC calculations so that AUC is calculated
  from `start` to `end`. Default for end is the maximum observation time
  for each subject. Subjects with insufficient data for a specified
  interval will have AUC calculated for the available data, not to
  exceed the specified interval.

- icen:

  Can be either "median" for the predictions based on medians
  of`pred.type` parameter value distributions, or "mean". Default is
  "median". Only relevant for PMpost or PMpop objects.

- outeq:

  Which output equation to plot. Default is 1.

- block:

  Which block to plot, where blocks are defined by dose reset events
  (EVID = 4) in the data.

- method:

  Default is "linear" for AUC trapezoidal calculation. Any other value
  will result in linear up, log down.

- addZero:

  Boolean to add a zero concentration at time 0. Default is `FALSE`.

## Value

A dataframe of class *PMauc*, which has 2 columns:

- `group` - Subject identification, usually "id"

- tau - AUC from `start` to `end`

## Details

Calculates the area under the time concentration curve using the
trapezoidal approximation from a variety of inputs. If a PM_pop,
PM_post, PM_op, or PMsim object is specified, `formula` is not required.

## See also

[PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md),
[PM_sim](https://lapkb.github.io/Pmetrics_rust/reference/PM_sim.md),
[PM_op](https://lapkb.github.io/Pmetrics_rust/reference/PM_op.md)

## Author

Michael Neely

## Examples

``` r
if (FALSE) { # \dontrun{
NPex$cov$plot(V ~ wt)
NPex$cov$plot(Ke ~ wt, line = list(lm = TRUE, ref = FALSE, loess = FALSE))
NPex$cov$plot(Ke ~ wt, line = list(loess = list(ci = 0.9, color = "green")))
NPex$cov$plot(V ~ time, marker = list(color = "blue"))
NPex$cov$plot(V ~ wt,
  line = list(lm = TRUE, loess = FALSE),
  stats = list(x = 0.5, y = 0.2, font = list(size = 7, color = "blue"))
)
} # }
```
