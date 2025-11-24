# CDC Pediatric and Adolescent Growth Data Table

Centers for Disease Control Pediatric and Adolescent Growth Data Table

## Usage

``` r
growth
```

## Format

A data frame with the following 9 columns: KNOT (integer age in months);
A, B1, B2, B3 (coefficients for calculating percentiles), SEX, AGE,
PERCENTILE, and CHART (length x age, wt x age, wt x length, hc x age, or
ht x age).

## Details

Coefficients to calculate sex-specific percentiles of length, weight and
head cicumference data in children from 0 to 18 years. Downloaded and
combined from [](https://www.cdc.gov/growthcharts/data_tables.htm). Used
with the
[`qgrowth()`](https://lapkb.github.io/Pmetrics_rust/reference/qgrowth.md)
function to generate height and weight percentiles for the purposes of
simulation.

## Author

Michael Neely
