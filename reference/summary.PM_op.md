# Summarize Observations and Predictions

**\[stable\]**

Summarize a Pmetrics Observed vs. Predicted object

## Usage

``` r
# S3 method for class 'PM_op'
summary(
  object,
  digits = max(3, getOption("digits") - 3),
  pred.type = "post",
  icen = "median",
  outeq = 1,
  ...
)
```

## Arguments

- object:

  A [PM_op](https://lapkb.github.io/Pmetrics_rust/reference/PM_op.md)
  object

- digits:

  Integer, used for number of digits to print.

- pred.type:

  Either 'post' for a posterior object or 'pop' for a population object.
  Default is 'post'.

- icen:

  Can be either "median" for the predictions based on medians of
  `pred.type` parameter value distributions, or "mean". Default is
  "median".

- outeq:

  Output equation number. Default is 1.

- ...:

  Not used.

## Value

A list with three elements of class *summary.PM_op*.

- sumstat A data frame with the minimum, first quartile, median, third
  quartile, maximum, mean and standard deviation for times, observations
  and predictions in `x`.

- pe A named vector with mean prediction error (mpe), the mean weighted
  prediction error (mwpe), the percent mean weighted prediction error
  (percent_mwpe), the mean squared prediction error (mspe), root mean
  sqaured error (rmse), percent root mean squared error (percent_rmse),
  the mean weighted squared prediction error (mwspe), the bias-adjusted
  mean squared prediction error (bamspe), the bias- adjusted mean
  weighted squared prediction error (bamwspe), the percent root mean
  bias- adjusted weighted squared prediction error (percent_rmbawspe).
  The percent_mwpe is bias and the percent_rmbawspe is imprecision on
  plots of PM_op objects.

- wtd.t A list of 6 elements based on a t test that the weighted mean
  prediction bias is different than zero

&nbsp;

- estimate: the weighted mean of the prediction bias for each
  observation

- se: the standard error of the estimate

- conf.int: the 95% confidence interval of the mean

- statistic: the t statistic of the standardized difference between mean
  and zero

- df: degrees of freedom equal to number of observations minus one

- p.value: the probability that the weighted mean is different than zero

## Details

This is a function usually called by the `$summary()` method for
[PM_op](https://lapkb.github.io/Pmetrics_rust/reference/PM_op.md)
objects within a
[PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
to summarize observations, predictions and errors. The function can be
called directly on a
[PM_op](https://lapkb.github.io/Pmetrics_rust/reference/PM_op.md)
object. See examples.

## See also

[PM_op](https://lapkb.github.io/Pmetrics_rust/reference/PM_op.md)

## Author

Michael Neely

## Examples

``` r
if (FALSE) { # \dontrun{
NPex$op$summary() # preferred
summary(NPex$op) # alternative
} # }
```
