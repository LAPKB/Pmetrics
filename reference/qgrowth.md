# Extract CDC pediatric growth charts

**\[stable\]**

Will extract height, weight, and BMI for boys, girls or both for a given
range of ages in months and percentiles. This can be useful for
simulations in Pmetrics.

## Usage

``` r
qgrowth(sex = "B", agemos = (seq(0, 18) * 12), percentile = 50)
```

## Arguments

- sex:

  A single quoted character: "M" for males, "F" for females, or "B" for
  both. Default is "B".

- agemos:

  A vector of ages in months to return. The default is `seq(0,18)*12`,
  i.e. 0 to 216 months in increments of 12, which is 1 to 18 years.
  Values do not have to be multiples of 12.

- percentile:

  An integer vector of the percentile for each age/sex to return.
  Default is 50, but could be, for example
  `c(5, 10, 25, 50, 75, 90, 95)`.

## Value

A dataframe with columns

- agemos Age in months

- ageyrs Age in years

- wt Weight in kilograms

- ht Height or length in centimeters

- bmi Body mass index \$kg/m^2\$

- sex The selected sex(es)

- percentile The selected percentile(s)

## Author

Michael Neely
