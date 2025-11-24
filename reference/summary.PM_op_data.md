# Summarize PM_op_data objects

**\[stable\]** Summarizes the raw data (`class: PM_op_data`) from a
[PM_op](https://lapkb.github.io/Pmetrics_rust/reference/PM_op.md) object
in the same way as summarizing a
[PM_op](https://lapkb.github.io/Pmetrics_rust/reference/PM_op.md)
object. Both use
[summary.PM_op](https://lapkb.github.io/Pmetrics_rust/reference/summary.PM_op.md).

## Usage

``` r
# S3 method for class 'PM_op_data'
summary(object, ...)
```

## Arguments

- object:

  A `PM_op_data` object

- ...:

  Additional arguments passed to
  [summary.PM_op](https://lapkb.github.io/Pmetrics_rust/reference/summary.PM_op.md)

## Examples

``` r
if (FALSE) { # \dontrun{
NPex$op$data %>%
dplyr::filter(pred > 5) %>%
dplyr::filter(pred < 10) %>%
summary()
} # }
```
