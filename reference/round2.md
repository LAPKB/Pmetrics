# Round to x digits

**\[stable\]** Rounds a numeric value to a specified number of digits
for display in flextables and plots.

## Usage

``` r
round2(x, digits = getPMoptions("digits"))
```

## Arguments

- x:

  A numeric value to be rounded.

- digits:

  The number of digits to round to. Default is set using
  [setPMoptions](https://lapkb.github.io/Pmetrics_rust/reference/setPMoptions.md).

## Value

A character string representing the rounded value with the specified
number of digits.

## Details

Uses [base::format](https://rdrr.io/r/base/format.html) and base::round
to round a numeric value to a specified number of digits.
