# Summarize Observations and Predictions

**\[stable\]**

Summarize a Pmetrics Observed vs. Predicted object

## Usage

``` r
# S3 method for class 'PM_pop'
summary(
  object,
  digits = max(3, getOption("digits") - 3),
  icen = "median",
  outeq = 1,
  ...
)
```

## Arguments

- object:

  A [PM_pop](https://lapkb.github.io/Pmetrics_rust/reference/PM_pop.md)
  object

- digits:

  Integer, used for number of digits to print.

- icen:

  Can be either "median" for the predictions based on medians of the
  population parameter value distributions, or "mean". Default is
  "median".

- outeq:

  Output equation number. Default is 1.

- ...:

  Not used.

## Value

A data frame with the minimum, first quartile, median, third quartile,
maximum, mean and standard deviation for times and predictions in `x`.

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

[PM_pop](https://lapkb.github.io/Pmetrics_rust/reference/PM_pop.md)

## Author

Michael Neely

## Examples

``` r
if (FALSE) { # \dontrun{
NPex$pop$summary() # preferred
summary(NPex$pop) # alternative
} # }
```
