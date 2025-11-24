# Print Summary of Parameter Values and Credibility Intervals

**\[stable\]**

Print a Pmetrics Final Summary Object

## Usage

``` r
# S3 method for class 'summary.PM_final'
print(x, digits = 3, ...)
```

## Arguments

- x:

  A summary.PM_final object made by
  [summary.PM_final](https://lapkb.github.io/Pmetrics_rust/reference/summary.PM_final.md).

- digits:

  Integer, used for number of digits to print.

- ...:

  Not used.

## Value

A printed object.

## Details

Print a summary of parameter medians and MAWD, with point estimates and
credibilty intervals from an object made by
[summary.PM_final](https://lapkb.github.io/Pmetrics_rust/reference/summary.PM_final.md).
Users do not normally need to call this function directly, as it will be
the default method to display the object.

## See also

[summary.PM_final](https://lapkb.github.io/Pmetrics_rust/reference/summary.PM_final.md)

## Author

Michael Neely

## Examples

``` r
if (FALSE) { # \dontrun{
NPex$final$summary
} # }
```
