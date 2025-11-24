# Example MIC data

Example MIC data

## Usage

``` r
mic1
```

## Format

An R data frame containing example MIC distribution data in two columns:

- mic Minimum inhibitory concentration

- n Number of organisms with the given MIC

## Details

This data frame contains MIC data for vancomycin against S. aureus. It
was obtained from the EUCAST website at [](https://mic.eucast.org).
Select the organism or drug, and then select the desired row of the
resulting table to see a histogram (top) and table (bottom) of MIC
distributions.

Copy the table into excel, save as a .csv file, and read into R using a
function like [`read.csv()`](https://rdrr.io/r/utils/read.table.html) or
[`readr::read_csv()`](https://readr.tidyverse.org/reference/read_delim.html).
Then use
[`makePTAtarget()`](https://lapkb.github.io/Pmetrics_rust/reference/makePTAtarget.md).

## Author

Michael Neely
