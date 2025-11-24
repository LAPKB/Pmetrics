# Display multiple plotly plots

**\[stable\]**

Wrapper around
[`plotly::subplot()`](https://rdrr.io/pkg/plotly/man/subplot.html).

## Usage

``` r
sub_plot(
  ...,
  nrows = 1,
  widths = NULL,
  heights = NULL,
  margin = 0.02,
  titles = NULL,
  shareX = FALSE,
  shareY = FALSE,
  titleX = shareX,
  titleY = shareY,
  which_layout = "merge",
  print = TRUE
)
```

## Arguments

- ...:

  One of the following

  - any number of plotly objects

  - a list of plotly objects. Note that unlike
    [`plotly::subplot()`](https://rdrr.io/pkg/plotly/man/subplot.html),
    ggplots and tibbles cannot be passed to this function.

- nrows:

  number of rows for laying out plots in a grid-like structure. Default
  is 1.

- widths, heights:

  Vector of relative column widths or heights on a 0-1 scale. By default
  all columns have an equal relative width/height, i.e. `c(0.5, 0.5)`
  for two columns, `rep(0.25, 4)` for 4 columns.

- margin:

  either a single value or a vector of four values (all between 0 and
  1), e.g. `c(0.05, 0.05, 0.05, 0.1)` If four values are provided, the
  first is used as the left margin, the second is used as the right
  margin, the third is used as the top margin, and the fourth is used as
  the bottom margin. If a single value is provided, it will be used as
  all four margins.

- titles:

  Include titles on individual subplots? Default is `NULL`. If specified
  as vector of length 2, will be rendered as x and y values relative to
  each plot. For example, `title = c(0,1)` plots the titles in the upper
  left corner of each subplot and `title = c(1,0)` renders the titles in
  the lower right corner. Title text and formatting will be grabbed from
  each individual plot. To modify these characteristics, modify the code
  that generated the individual plot.

- shareX, shareY:

  Should the x- or y- axis be shared amongst the subplots?

- titleX, titleY:

  Should x- or y- axis titles be retained?

- which_layout:

  Adopt the layout of which plot? If the default value of "merge" is
  used, layout options found later in the sequence of plots will
  override options found earlier in the sequence. This argument also
  accepts a numeric vector specifying which plots to consider when
  merging.

- print:

  If `TRUE`, will print the plotly object and return it. If `FALSE`,
  will only return the plotly object.

## Value

A plot and plotly object combining all the plots in `...`, which can be
further modified.

## Details

This function addresses the deficiency with the native plotly method of
combining multiple plots that prevents individualized titling of
subplots. The function has identical arguments to
[`plotly::subplot()`](https://rdrr.io/pkg/plotly/man/subplot.html) with
the addition of a `titles` argument. In addition to `subplot`, the
behavior of this function is two-fold:

- Fetch the titles (text and formatting) from each included plot

- Include the titles with placement per the `titles` argument

## See also

[`plotly::subplot()`](https://rdrr.io/pkg/plotly/man/subplot.html)

## Author

Michael Neely

## Examples

``` r
if (FALSE) { # \dontrun{
plot1 <- NPex$op$plot(title = "Posterior")
plot2 <- NPex$op$plot(pred.type = "pop", title = "Population")
sub_plot(plot1, plot2, titles = c(0, 0.95), nrows = 2)
} # }
```
