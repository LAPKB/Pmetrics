# Print data frame in CLI format

**\[stable\]** Prints a data frame in a format suitable for the command
line interface (CLI).

## Usage

``` r
cli_df(df)
```

## Arguments

- df:

  A data frame to be printed.

## Value

A formatted text output of the data frame.

## Details

Uses [dplyr::mutate](https://dplyr.tidyverse.org/reference/mutate.html)
to convert all columns to character, rounds numeric values using
[round2](https://lapkb.github.io/Pmetrics_rust/reference/round2.md), and
formats the output using
[knitr::kable](https://rdrr.io/pkg/knitr/man/kable.html) for a simple
table format. The function replaces spaces with non-breaking spaces for
better alignment in the CLI.
