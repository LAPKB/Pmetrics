# Plot PM_op_data objects

**\[stable\]** Plots the raw data (`class: PM_op_data`) from a
[PM_op](https://lapkb.github.io/Pmetrics_rust/reference/PM_op.md) object
in the same way as plotting a
[PM_op](https://lapkb.github.io/Pmetrics_rust/reference/PM_op.md)
object. Both use
[plot.PM_op](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_op.md).

## Usage

``` r
# S3 method for class 'PM_op_data'
plot(x, ...)
```

## Arguments

- x:

  A \`PM_op_data“ object

- ...:

  Additional arguments passed to
  [plot.PM_op](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_op.md)

## Examples

``` r
if (FALSE) { # \dontrun{
NPex$op$data %>%
dplyr::filter(pred > 5) %>%
dplyr::filter(pred < 10) %>%
plot()
} # }
```
