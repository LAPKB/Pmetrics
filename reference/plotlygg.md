# Convert a plotly object to ggplot

**\[stable\]** Converts a plotly object to a ggplot object. It is the
inverse of
[`plotly::ggplotly()`](https://rdrr.io/pkg/plotly/man/ggplotly.html).

## Usage

``` r
plotlygg(p, print = TRUE)
```

## Arguments

- p:

  A plotly object to convert.

- print:

  If `TRUE` (the default), will print the ggplot object and invisibly
  return it.

## Value

A ggplot object.

## Details

This function extracts the data and layout from a plotly object and
constructs a ggplot object with the same data. It supports various trace
types including scatter, bar, and line traces.
