# Plot PM_cycle_data objects

**\[stable\]** Plots the raw data (`class: PM_cycle_data`) from a
[PM_cycle](https://lapkb.github.io/Pmetrics_rust/reference/PM_cycle.md)
object in the same way as plotting a
[PM_cycle](https://lapkb.github.io/Pmetrics_rust/reference/PM_cycle.md)
object. Both use
[plot.PM_cycle](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_cycle.md).

## Usage

``` r
# S3 method for class 'PM_cycle_data'
plot(x, ...)
```

## Arguments

- x:

  A `PM_cycle_data` object

- ...:

  Additional arguments passed to
  [plot.PM_cycle](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_cycle.md)

## Examples

``` r
# There is no example we can think of to filter or otherwise process a PM_cycle object,
# but we provide this function for completeness.
NPex$cycle$data %>% plot()
```
