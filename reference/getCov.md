# Extract covariate information

**\[stable\]**

Extracts covariate information from a Pmetrics data object.

## Usage

``` r
getCov(mdata)
```

## Arguments

- mdata:

  A
  [PM_data](https://lapkb.github.io/Pmetrics_rust/reference/PM_data.md)
  object. It can be other data frames but the results will likely not be
  meaningful.

## Value

A list with named items: *ncov, covnames, covstart, covend*.

## Details

The function subtracts the number of fixed columns in a standard data
format, currently 14, from the total number of columns in `mdata` and
queries the remaining columns. When given a
[PM_data](https://lapkb.github.io/Pmetrics_rust/reference/PM_data.md)
object, will return a list with the number of covariates, their names,
and the starting and ending column numbers

## Author

Michael Neely

## Examples

``` r
if (FALSE) { # \dontrun{
getCov(dataEx)
} # }
```
