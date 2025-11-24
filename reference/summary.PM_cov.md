# Summarize Covariates and Bayesian Posterior Parameter Values

**\[stable\]**

Summarize a Pmetrics Covariate object

## Usage

``` r
# S3 method for class 'PM_cov'
summary(object, icen = "median", ...)
```

## Arguments

- object:

  A PM_cov object

- icen:

  Summary function for covariates with time dependent values and
  posterior parameters. Default is "median", but can specify "mean".

- ...:

  Not used.

## Value

A data frame with the summary of the PM_cov object for each subject's
covariates and Bayesian posterior parameter values.

## Details

This is a function usually called by the `$summary()` method for
[PM_cov](https://lapkb.github.io/Pmetrics_rust/reference/PM_cov.md)
objects with a
[PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
to summarize covariates and Bayesian posterior parameter values for each
subject. The function can be called directly on a
[PM_cov](https://lapkb.github.io/Pmetrics_rust/reference/PM_cov.md)
object. See examples.Summarize .

## See also

[PM_cov](https://lapkb.github.io/Pmetrics_rust/reference/PM_cov.md)

## Author

Michael Neely

## Examples

``` r
if (FALSE) { # \dontrun{
NPex$cov$summary() # preferred
summary(NPex$cov) # alternative
NPex$cov$summary(icen = "mean") # use mean as summary
} # }
```
