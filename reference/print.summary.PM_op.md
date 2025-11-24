# Print Summary of Observations and Predictions

**\[stable\]**

Print a Pmetrics Observed vs. Predicted Summary Object

## Usage

``` r
# S3 method for class 'summary.PM_op'
print(x, digits = getPMoptions("digits"), embed = FALSE, ...)
```

## Arguments

- x:

  An object made by
  [summary.PM_op](https://lapkb.github.io/Pmetrics_rust/reference/summary.PM_op.md).

- digits:

  Integer, used for number of digits to print.

- embed:

  If `TRUE`, will embed the summary in the R Markdown document. Default
  is `FALSE`.

- ...:

  Not used.

## Value

A printed object.

## Details

Print a summary of observations, predictions and errors in a
[summary.PM_op](https://lapkb.github.io/Pmetrics_rust/reference/summary.PM_op.md)
object made by
[summary.PM_op](https://lapkb.github.io/Pmetrics_rust/reference/summary.PM_op.md).
Users do not normally need to call this function directly, as it will be
the default method to display the object.

## See also

[summary.PM_op](https://lapkb.github.io/Pmetrics_rust/reference/summary.PM_op.md)

## Author

Michael Neely

## Examples

``` r
if (FALSE) { # \dontrun{
NPex$op$summary()
} # }
```
