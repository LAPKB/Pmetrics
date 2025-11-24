# Add regression to plotly plot

**\[stable\]**

Modifies an existing plot to include a regression line with confidence
interval.

## Usage

``` r
add_smooth(
  p = plotly::last_plot(),
  x = NULL,
  y = NULL,
  data = NULL,
  method = c("lm", "loess"),
  span = 0.75,
  line = TRUE,
  ci = 0.95,
  stats = TRUE
)
```

## Arguments

- p:

  The plot to which the shape should be added. Default is the
  `plotly::last_plot()`.

- x:

  X value for regression if not the original x value of the plot. Be
  sure to specify as a formula, e.g. `x = ~pred`.

- y:

  Y value for regression if not the original y value of the plot. Be
  sure to specify as a formula, e.g. `y = ~obs`.

- data:

  A secondary data object to use for regression other than the original
  plot data.

- method:

  The tegression method, currently either "lm" (the default) for linear
  regression, or "loess" for loess regression.

- span:

  Only used for `method = "loess"`. The span parameter to control the
  degree of smoothing. Default is 0.75.

- line:

  Controls characteristics of lines. This argument maps to plotly line
  attributes. `TRUE` will plot default lines. `FALSE` will suppress
  lines. If a list, can control many line characteristics, including
  overriding defaults. Use the plotly
  [`plotly::schema()`](https://rdrr.io/pkg/plotly/man/schema.html)
  command in the console and navigate to traces \> scatter \> attributes
  \> line to see all the ways the line can be formatted. Most common
  will be:

  - `color` Line color.

  - `dash` Plotting character. See
    [`plotly::schema()`](https://rdrr.io/pkg/plotly/man/schema.html),
    traces \> scatter \> attributes \> line \> dash \> values.

  - `width` Thickness in points.

  Example: `line = list(color = "red", dash = "longdash", width = 2)`
  Default is `list(color = "blue", width = 2)`. The confidence interval
  will have the same color but at opacity 0.2.

- ci:

  Confidence interval for regressions. Default is 0.95. It can be
  suppressed by setting to 0.

- stats:

  Add the statistics from linear regression to the plot. Ignored if
  `method = "loess"`. If missing or `FALSE`, will be suppressed. Can be
  set to `TRUE` which results in default format of
  `list(x= 0.8, y = 0.1, bold = F, font = list(color = "black", family = "Arial", size = 14))`.
  The coordinates are relative to the plot with lower left = (0,0),
  upper right = (1,1). This argument maps to
  [`plotly::add_text()`](https://rdrr.io/pkg/plotly/man/add_trace.html).
  It is also an option that can be set in
  [setPMoptions](https://lapkb.github.io/Pmetrics_rust/reference/setPMoptions.md)
  to avoid specifying this argument for every plot. The default option
  is `TRUE`. If specified as a Pmetrics option, it can be overridden for
  specific plots by supplying a value.

## Details

This function adds a regression line to an existing plotly plot. The
default is to use the x and y values in the plot, but this can be
overridden by specifying a data object. If another `data` object is
used, values for `x` and `y` must also be specified. Alternatively, the
original data can be used and new columns selected for regression by
omitting the `data` argument and specifying new `x` and `y` values. The
intent of this function is to replicate the behavior of
`ggplot::geom_smooth()`.

## See also

[add_shapes](https://lapkb.github.io/Pmetrics_rust/reference/add_shapes.md)

## Examples

``` r
if (FALSE) { # \dontrun{
plotly::plot_ly(mtcars,
  x = ~hp, y = ~mpg,
  type = "scatter", mode = "markers", showlegend = FALSE
) %>%
  add_smooth()
plotly::plot_ly(iris,
  x = ~Sepal.Length, y = ~Petal.Length,
  type = "scatter", mode = "markers", showlegend = FALSE
) %>%
  add_smooth(method = "loess", ci = 0.9, line = list(color = "red", dash = "dash"))
} # }
```
