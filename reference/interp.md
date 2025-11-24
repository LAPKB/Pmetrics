# Model covariate declaration

**\[stable\]**

Declare whether covariates in the data are to have interpolation between
values or not.

## Usage

``` r
interp(type = "lm")
```

## Arguments

- type:

  If `type = "lm"` (the default) or `type = "linear"`, the covariate
  value will be linearly interpolated between values when fitting the
  model to the data. in a model list `cov` item. To fix covariate values
  to the value at the last time point, set `type = "none"`.

## Value

A value of 1 for "lm" and 0 for "none", which will be passed to Rust.

## Examples

``` r
if (FALSE) { # \dontrun{
cov <- c(
  wt = interp(), # same as interp("lm") or interp("linear")
  visit = interp("none")
)
} # }
```
