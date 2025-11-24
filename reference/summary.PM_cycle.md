# Summarize Final Cycle Statistics

**\[stable\]**

Summarize a Pmetrics Cycle object

## Usage

``` r
# S3 method for class 'PM_cycle'
summary(object, cycle = NULL, digits = 3, ...)
```

## Arguments

- object:

  A
  [PM_cycle](https://lapkb.github.io/Pmetrics_rust/reference/PM_cycle.md)
  object

- cycle:

  Cycle number to summarize. Default is last cycle.

- digits:

  Number of digits to round to. Default is 3.

- ...:

  Not used.

## Value

A list of class *summary.PM_cycle* whose elements are the last cycle
values for the following fields in a
[PM_cycle](https://lapkb.github.io/Pmetrics_rust/reference/PM_cycle.md)
object.

- **cycle** Maximum cycle number

- **ll** Log likelihood

- **aic** Akaike Information Criterion

- **bic** Bayesian Information Criterion

- **gamlam** Value of gamma or lambda for each output equation

- **mean** Normalized mean parameter values compared to initial value

- **sd** Normalized standard deviation of parameter values compared to
  initial value

- **median** Normalized median parameter values compared to initial
  value

## Details

This is a function usually called by the `$summary()` method for
[PM_cycle](https://lapkb.github.io/Pmetrics_rust/reference/PM_cycle.md)
objects within a
[PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
to summarize final cycle statistics. The function can be called directly
on a
[PM_cycle](https://lapkb.github.io/Pmetrics_rust/reference/PM_cycle.md)
object. See examples.

## See also

[PM_cycle](https://lapkb.github.io/Pmetrics_rust/reference/PM_cycle.md)

## Author

Michael Neely

## Examples

``` r
#'
if (FALSE) { # \dontrun{
NPex$cycle$summary() # preferred
summary(NPex$cycle) # alternative
} # }
```
