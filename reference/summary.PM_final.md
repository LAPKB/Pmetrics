# Summary Statistics for Final Cycle

**\[stable\]**

Generates summary statistics of final population model parameters.

## Usage

``` r
# S3 method for class 'PM_final'
summary(object, lower = 0.025, upper = 0.975, file = NULL, ...)
```

## Arguments

- object:

  The PM_final object made after an NPAG or IT2B run

- lower:

  Desired lower confidence interval boundary. Default is 0.025. Ignored
  for IT2B objects.

- upper:

  Desired upper confidence interval boundary. Default is 0.975. Ignored
  for IT2B objects.

- file:

  Filename to save the summary. Include path if necessary.

- ...:

  Not used.

## Value

The output is a data frame. For NPAG this has 4 columns:

- **value** The value of the summary statistic

- **par** The name of the parameter

- **type** Either *WtMed* for weighted median, or *MAWD* for MAWD (see
  details)

- **percentile** Requested `lower`, 0.5 (median), and `upper` quantiles
  For IT2B this has 6 columns:

- **mean** Parameter mean value

- **se.mean** Standard error of the mean

- **cv.mean** Error of the mean divided by mean

- **var** Variance of the parameter values

- **se.var** Standard error of the variance

- **summary** Name of the summary statistic

## Details

\#' This is a function usually called by the `$summary()` method for
[PM_final](https://lapkb.github.io/Pmetrics_rust/reference/PM_final.md)
objects within a
[PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md).
The function can be called directly on a
[PM_final](https://lapkb.github.io/Pmetrics_rust/reference/PM_final.md)
object. For NPAG runs, this function will generate weighted medians as
central tendencies of the population points with a 95% confidence
interval (95% CI) around the median, and the median absolute weighted
deviation (MAWD) from the median as a measure of the variance, with its
95% CI. These estimates correspond to weighted mean, 95% CI of the mean,
variance, and 95% CI of the variance, respectively, for a sample from a
normal distribution.

To estimate these non-parametric summaries, the function uses a Monte
Carlo simulation approach, creating 1000 x npoint samples with
replacement from the weighted marginal distribution of each parameter,
where npoint is the number of support points in the model. As an
example, if there are 100 support points, npoint = 100, and for Ka,
there will be 1000 sets of 100 samples drawn from the weighted marginal
distribution of the values for Ka. For each of the 1,000 sets of npoint
values, the median and MAWD are calculated, with MAWD equal to the
median absolute difference between each point and the median of that
set. The output is npoint estimates of the weighted median and npoint
estimates of the MAWD for each parameter, from which the median, 2.5th,
and 97.5th percentiles can be found as point estimates and 95%
confidence interval limits, respectively, of both the weighted median
and MAWD.

For IT2B runs, the function will return the mean and variance of each
parameter, and the standard errors of these terms, using \$\$SE_mean =
SD/\sqrt(nsub)\$\$ \$\$SE_var = var \* \sqrt(2/(nsub-1))\$\$.

## See also

[PM_final](https://lapkb.github.io/Pmetrics_rust/reference/PM_final.md)

## Author

Michael Neely

## Examples

``` r
if (FALSE) { # \dontrun{
NPex$final$summary() # preferred
ITex$final$summary()
summary(NPex$final) # alternate
} # }
```
