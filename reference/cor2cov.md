# Convert correlation matrix to covariance matrix

**\[stable\]** Converts a correlation matrix to a covariance matrix
using standard deviations.

## Usage

``` r
cor2cov(cor, sd)
```

## Arguments

- cor:

  A correlation matrix.

- sd:

  A vector of standard deviations corresponding to the variables in the
  correlation matrix.

## Value

A covariance matrix.

## Details

Uses matrix multiplication to convert a correlation matrix to a
covariance matrix.

## Author

Michael Neely
