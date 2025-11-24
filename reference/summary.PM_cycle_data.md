# Summarize PM_cycle_data objects

**\[stable\]** Summarizes the raw data (`class: PM_cycle_data`) from a
[PM_cycle](https://lapkb.github.io/Pmetrics_rust/reference/PM_cycle.md)
object in the same way as summarizing a
[PM_cycle](https://lapkb.github.io/Pmetrics_rust/reference/PM_cycle.md)
object. Both use
[summary.PM_cycle](https://lapkb.github.io/Pmetrics_rust/reference/summary.PM_cycle.md).

## Usage

``` r
# S3 method for class 'PM_cycle_data'
summary(object, ...)
```

## Arguments

- object:

  A `PM_cycle_data` object

- ...:

  Additional arguments passed to
  [summary.PM_cycle](https://lapkb.github.io/Pmetrics_rust/reference/summary.PM_cycle.md)

## Examples

``` r
# There is no example we can think of to filter or otherwise process a PM_cycle object,
# but we provide this function for completeness.
NPex$cycle$data %>% summary()
#> 
#> ── Cycle Summary ──
#> 
#> Cycle number: 100 of 100
#> Status: Stop: MaxCycles
#> Log-likelihood: 440.985
#> AIC:: 448.985
#> BIC: 452.968
#> Outeq 1: Gamma = 2.39
#> 
#> ── Normalized parameter values: 
#> # A tibble: 3 × 6
#>   cycle    ka    ke     v tlag1 stat  
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <chr> 
#> 1   100  1.14 0.978 0.975 0.931 mean  
#> 2   100  1.10 0.622 1.83  1.41  sd    
#> 3   100  1.13 1     0.92  0.865 median
# all the below are the same
# summary(NPex$cycle$data) 
# summary(NPex$cycle)
# NPex$cycle$summary()
```
