# Extract CDC pediatric BMI z-scores

**\[stable\]**

Will extract BMI z-scores based on a single age in months and sex.
Overweight is a z-score of \>1.04, and obese is a z-score \> 1.64.
Calculations are based on [CDC
formulae](https://www.cdc.gov/nccdphp/dnpa/growthcharts/resources/biv-cutoffs.pdf)

For a z-score \> 3, which indicates an extreme BMI, consider using the
modified z-score and percentile.

## Usage

``` r
zBMI(agemos, sex, bmi, wt, ht, data = "CDC")
```

## Arguments

- agemos:

  The age in months. Should be between 24 and 240.5.

- sex:

  A single quoted character: "M" for males, "F" for females. Default is
  "M".

- bmi:

  The individual's BMI. If specified, `wt` and `ht` are not necessary
  and will be ignored.

- wt:

  The individual's weight in kg as an alternative to specifying `BMI`.
  Will be ignored if `BMI` is specified.

- ht:

  The individual's height in centimeters. Required if `wt` is specified
  and `BMI` is not. Ignored if `BMI` is specified.

- data:

  Source data for calculations. Default is "CDC" which uses the
  [cdc_bmi](https://lapkb.github.io/Pmetrics_rust/reference/cdc_bmi.md)
  dataset. The alternative is "NHANES", which uses the
  [ger_bmi](https://lapkb.github.io/Pmetrics_rust/reference/ger_bmi.md)
  dataset.

## Value

A list with objects calculated for `agemos` and `sex`.

- z Z-score

- mod_z Modified Z-score for extreme BMI

- per BMI percentile

- mod_per Modified BMI percentile

## Author

Michael Neely

## Examples

``` r
if (FALSE) { # \dontrun{
zBMI(agemos = 36, bmi = 15)
} # }
```
