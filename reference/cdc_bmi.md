# CDC Pediatric and Adolescent BMI Table

Centers for Disease Control Pediatric and Adolescent BMI Table

## Usage

``` r
cdc_bmi
```

## Format

A data frame with the following 9 columns: Sex (1 = male), Agemos; L, M,
S (coefficients for calculating z-scores), P3, P5, P10, P25, P50, P75,
P85, P90, P95, P97: age and sex specific BMI percentiles

## Details

Coefficients to calculate sex-specific BMI z-scores and percentiles.
Downloaded from
[](https://www.cdc.gov/nccdphp/dnpa/growthcharts/resources/biv-cutoffs.pdf).
Tables were last updated in 2000, based on data through 1994.
Definitions of overweight and obese come from these data, based on BMI
percentile \>=85 for overweight and \>=95 for obese. See
[ger_bmi](https://lapkb.github.io/Pmetrics_rust/reference/ger_bmi.md)
for percentiles based on more modern NHANES data.

## Author

Michael Neely
