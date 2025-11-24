# Sample size calculations for Phase 1 PK study design

**\[stable\]**

This function calculates sample size based on a desired standard error
of the mean, to a specified confidence, for a given mean and standard
deviation.

## Usage

``` r
ss.PK(n, mean, sd, precision, ci = 0.95)
```

## Arguments

- n:

  Sample size. This value can be missing if sample size is desired, or
  specified to calculate the maximum sd for given `mean`, `precision`,
  and \`ci.

- mean:

  Mean prameter value. User value is mandatory.

- sd:

  Standard deviation of parameter values. If present, the function will
  return `n. If missing and `n\` is specified, will return the maximum
  sd as detailed above.

- precision:

  Desired width of the standard error of the mean (SEM). Default is 0.2,
  i.e. 20% or 10% below and 10% above the mean. If missing, and `mean`,
  `sd` and `n` are specified, `precision` will be calculated.

- ci:

  Confidence for the desired width of the SEM. Default is 0.95.

## Value

The missing argument: `n`, `sd` or `precision.`

## Details

The formula is `n = qnorm((1+ci)/2)**2 * sd**2 / (precision*mean)**2`

## Author

Michael Neely
