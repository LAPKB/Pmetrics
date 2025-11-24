# Check if a matrix is positive definite

**\[stable\]** Checks if a matrix is positive definite and attempts to
fix it if necessary.

## Usage

``` r
pos_def(mat, id, source)
```

## Arguments

- mat:

  A covariance matrix to check.

## Value

A positive definite covariance matrix, 1 if aborting, or -1 if unable to
fix

## Author

Michael Neely
