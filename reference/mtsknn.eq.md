# Compare discrete distributions

**\[stable\]**

Multivariate two-sample test based on k-nearest neighbors

## Usage

``` r
mtsknn.eq(x, y, k, clevel = 0.05, getpval = TRUE, print = TRUE)
```

## Arguments

- x:

  A matrix or data frame.

- y:

  A matrix or data frame.

- k:

  k An integer.

- clevel:

  The confidence level. Default value is 0.05.

- getpval:

  Logic value. If it is set to be TRUE the p value of test will be
  calcuated and reported; if it is set to be false the p value will not
  be calculated.

- print:

  Boolean value. If it is set to be TRUE the test result will be
  reported; if it is set to be FALSE the test result will not be
  reported.

## Value

A list consists of the test statistics, normalized Z score and
corresponding P value.

## Details

This function tests whether two samples share the same underlying
distribution based on k-nearest-neighbors approach. Matrices or data
frames x and y are the two samples to be tested. Each row consists of
the coordinates of a data point. The integer k is the number of nearest
neighbors to choose in the testing procedure. This approach is robust in
the unbalanced case.

## References

Schilling, M. F. (1986). Multivariate two-sample tests based on nearest
neighbors. *J. Amer. Statist. Assoc.*, 81 799-806. Henze, N. (1988). A
multivariate two-sample test based on the number of nearest neighbor
type coincidences.*Ann. Statist.*, 16 772-783. Chen, L. and Dou W.
(2009). Robust multivariate two-sample tests based on k nearest
neighbors for unbalanced designs. *manuscripts*.

## Author

Lisha Chen (Yale), Peng Dai (Stonybrook) and Wei Dou (Yale)
