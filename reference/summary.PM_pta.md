# Summarize Percent Target Attainment

**\[stable\]**

Summarize a Pmetrics Percent Target Attainment Object

## Usage

``` r
# S3 method for class 'PM_pta'
summary(object, at = "intersect", ci = 0.95, ...)
```

## Arguments

- object:

  A [PM_pta](https://lapkb.github.io/Pmetrics_rust/reference/PM_pta.md)
  object

- at:

  Which object in the
  [PM_pta](https://lapkb.github.io/Pmetrics_rust/reference/PM_pta.md)
  result list to summarize. By default "intersect" if an intersection is
  present due to creation of the object with multiple target types, or 1
  if no intersection is present, which means only 1 target type was
  selected. If "intersect" is present in the object, the default can be
  overridden with a number to summarize one of the individual PTAs, e.g.
  `at = 2` to summarize the second PTA rather than the intersection of
  all the PTAs.

- ci:

  Width of the interval for pharmacodynamic index reporting. Default is
  0.95, i.e. 2.5th to 97.5th percentile.

- ...:

  Not used.

## Value

A tibble with the following columns (only the first five if
`at = "intersect"`):

- **reg_num** is the number of the simulation regimen

- **label** is the simulation label, for reference

- **target** is the target for the row, if targets are discrete, not
  used for simulated targets

- **type** is the target type for the row, e.g. "auc", "time", "-min",
  etc.

- **prop_success** is the proportion of simulated profiles that met the
  success definition

- **median** is the median parmacodynamic index (PDI), i.e. the
  proportion or ratio depending on the target type

- **lower** is the lower bound of the interval defined by `ci`

- **upper** is the upper bound of the interval defined by `ci`

- **mean** is the mean of the PDI

- **sd** is the standard deviation of the PDI

- **min** is the minimum PDI

- **max** is the maximum PDI

## Details

Summarize Pharmacodynamic Index (PDI) statistics and success proportions
in a [PM_pta](https://lapkb.github.io/Pmetrics_rust/reference/PM_pta.md)
object. The PDI is the metric calculated by the target type and target,
e.g. AUC/Target, or %time\>target. Since a PDI cannot be calculated for
intersections, summarizing the intersection object only provides the
success proportion per simulation/target.

## See also

[PM_pta](https://lapkb.github.io/Pmetrics_rust/reference/PM_pta.md)

## Author

Michael Neely

## Examples

``` r
if (FALSE) { # \dontrun{
ptaEx$summary()
} # }
```
