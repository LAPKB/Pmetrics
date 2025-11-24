# Stepwise covariate-parameter regressions

**\[superseded\]**

This function is largely superseded as it is accessed through the
`$step` methods for
[PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
and [PM_cov](https://lapkb.github.io/Pmetrics_rust/reference/PM_cov.md)
objects. There is rarely a need to call it directly any longer.

## Usage

``` r
PM_step(x, icen = "median", direction = "both")
```

## Arguments

- x:

  A PMcov object which is the `$data` field of a
  [PM_cov](https://lapkb.github.io/Pmetrics_rust/reference/PM_cov.md)
  object

- icen:

  A character vector to summarize covariate values. Default is "median",
  but can also be "mean".

- direction:

  The direction for covariate elmination can be "backward", "forward",
  or "both" (default).

## Value

A matrix with covariates in the rows and parameters in the columns.
Values for the matrix are the multi-variate P-values. A value of `NA`
indicates that the variable was not retained in the final model.

## Details

It will perform stepwise linear regressions on a
[PM_cov](https://lapkb.github.io/Pmetrics_rust/reference/PM_cov.md)
object. Every covariate in the model will be tested in a stepwise linear
regression for their relationships to each parameter in the model.
Bayesian posterior parameters and individual covariates are used.

## See also

[`stats::step()`](https://rdrr.io/r/stats/step.html)

## Author

Michael Neely
