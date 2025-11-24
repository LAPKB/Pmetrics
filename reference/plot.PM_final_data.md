# Plot PM_final_data objects

**\[stable\]** Plots the raw data (`class: PM_final_data`) from a
[PM_final](https://lapkb.github.io/Pmetrics_rust/reference/PM_final.md)
object in the same way as plotting a
[PM_final](https://lapkb.github.io/Pmetrics_rust/reference/PM_final.md)
object. Both use
[plot.PM_final](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_final.md).

## Usage

``` r
# S3 method for class 'PM_final_data'
plot(x, ...)
```

## Arguments

- x:

  A \`PM_final_data“ object

- ...:

  Additional arguments passed to
  [plot.PM_final](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_final.md)

## Examples

``` r
NPex$final$data %>% plot()
```
