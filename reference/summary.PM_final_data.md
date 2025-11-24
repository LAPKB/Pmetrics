# Summarize PM_final_data objects

**\[stable\]** Summarizes the raw data (`class: PM_final_data`) from a
[PM_final](https://lapkb.github.io/Pmetrics_rust/reference/PM_final.md)
object in the same way as summarizing a
[PM_final](https://lapkb.github.io/Pmetrics_rust/reference/PM_final.md)
object. Both use
[summary.PM_final](https://lapkb.github.io/Pmetrics_rust/reference/summary.PM_final.md).

## Usage

``` r
# S3 method for class 'PM_final_data'
summary(object, ...)
```

## Arguments

- object:

  A `PM_final_data` object

- ...:

  Additional arguments passed to
  [summary.PM_final](https://lapkb.github.io/Pmetrics_rust/reference/summary.PM_final.md)

## Examples

``` r
NPex$final$data %>% summary()
```
