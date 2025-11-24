# Summarize PM_data objects

**\[stable\]**

Summarize the raw data used for a Pmetrics run.

## Usage

``` r
# S3 method for class 'PM_data'
summary(object, formula, FUN, include, exclude, ...)
```

## Arguments

- object:

  A
  [PM_data](https://lapkb.github.io/Pmetrics_rust/reference/PM_data.md)
  object.

- formula:

  Optional formula for specifying custom summaries. See
  [aggregate](https://rdrr.io/r/stats/aggregate.html) and
  [formula](https://rdrr.io/r/stats/formula.html) for details on how to
  specify formulae in R. If, for example, the data contain a covariate
  for weight named 'wt', then to summarize the mean dose in mg/kg per
  subject specify `formula = dose/wt ~ id` and `FUN = mean`.

- FUN:

  The summary function to apply to
  [formula](https://rdrr.io/r/stats/formula.html), if specified. This is
  not quoted, and usual choices will be
  [mean](https://rdrr.io/r/base/mean.html),
  [median](https://rdrr.io/r/stats/median.html),
  [max](https://rdrr.io/r/base/Extremes.html), or
  [min](https://rdrr.io/r/base/Extremes.html).

- include:

  A vector of subject IDs to include in the summary, e.g. `c(1:3,5,15)`

- exclude:

  A vector of subject IDs to exclude in the summary, e.g.
  `c(4,6:14,16:20)`

- ...:

  Additional arguments to `FUN`, e.g. `na.rm = TRUE`

## Value

A list of class *summary.PM_data* with the following items:

- **nsub** Number of subjects

- **ndrug** Number of drug inputs

- **numeqt** Number of outputs

- **nobsXouteq** Number of observations by outeq

- **missObsXouteq** Number of missing observations by outeq

- **loqObsXouteq** Number of observations coded as below the limit of
  quantification by outeq

- **ncov** Number of covariates

- **covnames** Covariate names

- **ndoseXid** Number of doses per input per subject

- **nobsXid** Number of observations per outeq per subject

- **doseXid** Doses per input per subject

- **obsXid** Observations per outeq per subject

- **formula** Results of including
  [formula](https://rdrr.io/r/stats/formula.html)

## See also

[aggregate](https://rdrr.io/r/stats/aggregate.html)

## Author

Michael Neely
