# Initial mean/SD for primary parameter values

**\[stable\]**

Define primary model parameter initial values as mean and standard
deviation, which translate to a range. The mean serves as the midpoint
of the range, with 3 standard deviations above and below the mean to
define the min and max of the range. For nonparametric, this range will
be absolutely respected. For parametric, values can be estimated beyond
the range.

## Usage

``` r
msd(mean, sd)
```

## Arguments

- mean:

  Initial mean.

- sd:

  Initial standard deviation.
