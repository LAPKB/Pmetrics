# Plot Pmetrics Covariate objects

**\[stable\]**

Plot PMcov objects

## Usage

``` r
# S3 method for class 'PM_cov'
plot(
  x,
  formula,
  line = list(lm = NULL, loess = NULL, ref = NULL),
  marker = TRUE,
  colors,
  icen = "median",
  include = NULL,
  exclude = NULL,
  legend,
  log = FALSE,
  grid = TRUE,
  xlab,
  ylab,
  title,
  stats = TRUE,
  xlim,
  ylim,
  print = TRUE,
  ...
)
```

## Arguments

- x:

  The name of an
  [PM_cov](https://lapkb.github.io/Pmetrics_rust/reference/PM_cov.md)
  data object and loaded with
  [PM_load](https://lapkb.github.io/Pmetrics_rust/reference/PM_load.md)
  as a
  [PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md),
  e.g. `PM_result$cov`.

- formula:

  This is a mandatory formula of the form `y ~ x`, where `y` and `x` are
  the two `data` parameters to plot.

- line:

  Controls characteristics of lines. Unlike some other Pmetrics plots,
  but like
  [plot.PM_op](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_op.md),
  `line` is a list of three elements:

  - `lm` If set to `TRUE` or a list of plotly line attributes, will
    generate a linear regression of the form y ~ x Line attributes will
    control the appearance of the regression line and the confidence
    interval around the line. If set to `FALSE`, no linear regression
    will be generated. The default values for the elements of the `lm`
    list, all of which can be overriden are:

    - `ci` Confidence interval around the regression, default 0.95.

    - `color` Color of the regression line and the confidence area
      around the line, but at opacity = 0.2. Default is "dodgerblue".

    - `width `Width of the regression line, default 1.

    - `dash` See
      [`plotly::schema()`](https://rdrr.io/pkg/plotly/man/schema.html),
      traces \> scatter \> attributes \> line \> dash \> values. Default
      is "solid". Example:
      `line = list(lm = list(color = "red", dash = "longdash", width = 2))`

  - `loess` If set to `TRUE` or a list of plotly line attributes, will
    generate a loess regression of the form y ~ x The list elements and
    default values in the `loess` list are the same as for `lm` except
    the default style is "dash". Example:
    `line = list(lm = FALSE, loess = TRUE)`

  - `ref` If set to `TRUE` or a list of plotly line attributes, will
    generate a reference line with slope = 1 and intercept = 0. The
    default values for the elements of the `ref` list are:

    - `color` "grey".

    - `width` 1.

    - `dash` "dot". Note that there is no *ci* argument for the *ref*
      list. Example:
      `line = list(lm = FALSE, loess = TRUE, ref = list(color = "lightgrey"))`
      If the `line` argument is missing, it will be set to
      `line = list(lm = FALSE, loess = TRUE, ref = FALSE)`, i.e. there
      will be a linear regression with reference line, but no loess
      regression. If *time* is chosen as the x variable in the formula,
      linear, loess and reference lines will be suppressed, although
      formatting specified in the loess line (except color, see below)
      will be applied to the lines joining the subject values.

- marker:

  Controls the plotting symbol for observations. This argument maps to
  the plotly marker object. It can be boolean or a list. `TRUE` will
  plot the marker with default characteristics. `FALSE` will suppress
  marker plotting. If a list, can control many marker characteristics,
  including overriding defaults. Use the plotly
  [`plotly::schema()`](https://rdrr.io/pkg/plotly/man/schema.html)
  command in the console and navigate to traces \> scatter \> attributes
  \> marker to see all the ways the marker can be formatted. Most common
  will be:

  - `color` Marker color.

  - `symbol` Plotting character. See
    [`plotly::schema()`](https://rdrr.io/pkg/plotly/man/schema.html),
    traces \> scatter \> attributes \> marker \> symbol \> values.

  - `size` Character size in points.

  - `opacity` Ranging between 0 (fully transparent) to 1 (fully opaque).

  - `line` A list of additional attributes governing the outline for
    filled shapes, most commonly color and width.

  Example:
  `marker = list(color = "red", symbol = "triangle", opacity = 0.8, line = list(color = "black", width = 2))`
  Default is
  `marker = list(color = orange, shape = "circle", size = 10, opacity = 0.5, line = list(color = black, width = 1))`.

- colors:

  to use for subjects when *time* is set as the x parameter. This can be
  a palette or a vector of colors. For accepted palette names see
  [`RColorBrewer::brewer.pal.info`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html).
  Examples include "BrBG", or "Set2". An example vector could be
  `c("red", "green", "blue")`. It is not necessary to specify the same
  number of colors as groups within `color`, as colors will be
  interpolated to generate the correct number. The default is the
  "Spectral" palette. This will override any color in the `marker` or
  `line`.

- icen:

  Can be either "median" for the predictions based on medians
  of`pred.type` parameter value distributions, or "mean". Default is
  "median".

- include:

  A vector of subject IDs to include in the plot, e.g. c(1:3,5,15)

- exclude:

  A vector of subject IDs to exclude in the plot, e.g. c(4,6:14,16:20)

- legend:

  Not used for this function.

- log:

  Boolean operator to plot the y axis in log base 10. This argument maps
  to the yaxis type value in the layout object in plotly. Use the plotly
  [`plotly::schema()`](https://rdrr.io/pkg/plotly/man/schema.html)
  command in the console and navigate to layout \> layoutAttributes \>
  yaxis \> type.

  Example: `log = T`

- grid:

  Controls grid display. This argument maps to the xaxis and yaxis
  layout objects in plotly. Use the plotly
  [`plotly::schema()`](https://rdrr.io/pkg/plotly/man/schema.html)
  command in the console and navigate to layout \> layoutAttributes \>
  xaxis or yaxis \> gridcolor or gridwidth. It is a Boolean operator. If
  `FALSE`, no grid is plotted. If `TRUE`, the default color *grey* and
  width 1 will be plotted at major tick marks. If a list, color and
  width can be customized.

  Examples:

  - `grid = F`

  - `grid = list(gridcolor = "black", gridwidth = 2)`

- xlab:

  Value for x axis label. This argument maps to the the xaxis title
  element of the layout object in plotly. It can simply be a character
  vector of length 1 that specifies the name of the axis, or it can be a
  list for greater control. Use the plotly
  [`plotly::schema()`](https://rdrr.io/pkg/plotly/man/schema.html)
  command in the console and navigate to layout \> layoutAttributes \>
  xaxis \> title to see the ways to customize this axis label. In
  addition to the plotly attributes, a custom Pmetrics attribute `bold`
  may be included as a list element, either on its own or within the
  font list. The default for `bold` is `TRUE`.

  Examples:

  - `xlab = "Time (h)"`

  - `xlab = list(text = "Time", bold = F, font = list(color = "red", family = "Arial", size = 10))`

  - `xlab = list(text = "Time", font = list(bold = T))`

  If missing, will default to the name of the x variable in the formula.

- ylab:

  Value for y axis label. This argument maps to the the yaxis title
  element of the layout object in plotly. See `xlab` for details. If
  `xlab` is specified as a list with formatting, and `ylab` is simply a
  character label, then the formatting for the `xlab` will be applied to
  the `ylab`. To format `ylab` independently, specify a formatting list
  as for `xlab`.

  If missing, will default to the name of the y variable in the formula.

- title:

  Plot title. This argument maps to the the title layout object in
  plotly. It can simply be a character vector of length 1 that specifies
  the name of the plot title, or it can be a list for greater control.
  Use the plotly
  [`plotly::schema()`](https://rdrr.io/pkg/plotly/man/schema.html)
  command in the console and navigate to layout \> layoutAttributes \>
  title to see other ways to customize the title using lists as
  additional arguments. In addition to the plotly attributes, a custom
  Pmetrics attribute `bold` may be included as a list element. The
  default for `bold` is `TRUE`.

  Examples:

  - `title = "Observed vs. Predicted"`

  - `title = list(text = "Raw Data", font = list(color = "red", family = "Arial", size = 10, bold = T))`

  Default is to have no title.

- stats:

  Add the statistics from linear regression to the plot. If `FALSE`,
  will be suppressed. Default is `TRUE` which results in default format
  of
  `list(x= 0.8, y = 0.1, bold = F, font = list(color = "black", family = "Arial", size = 14))`.
  The coordinates are relative to the plot with lower left = (0,0),
  upper right = (1,1). This argument maps to
  [`plotly::add_text()`](https://rdrr.io/pkg/plotly/man/add_trace.html).

- xlim:

  Limits of the x axis as a vector. This argument maps to the the xaxis
  range in the layout object in plotly. Use the plotly
  [`plotly::schema()`](https://rdrr.io/pkg/plotly/man/schema.html)
  command in the console and navigate to layout \> layoutAttributes \>
  xaxis \> range.

  Example: `xlim = c(0,1)`

- ylim:

  Limits of the y axis as a vector. This argument maps to the the yaxis
  range in the layout object in plotly. Use the plotly
  [`plotly::schema()`](https://rdrr.io/pkg/plotly/man/schema.html)
  command in the console and navigate to layout \> layoutAttributes \>
  yaxis \> range.

  Example: `ylim = c(0,100)`

- print:

  If `TRUE`, will print the plotly object and return it. If `FALSE`,
  will only return the plotly object.

- ...:

  Other attributes which can be passed to the layout \> xaxis/yaxis in a
  plotly plot to further control formatting. Note that `log`, `xlab`,
  `ylab`, `xlim`, and `ylim` are all controlled by the layout object,
  but are provided throughout Pmetrics plotly function arguments as
  shortcuts that map to layout elements. Therefore, the dots argument
  should be used to specify other aspects of the x axis, y axis, or
  both. See
  [`plotly::schema()`](https://rdrr.io/pkg/plotly/man/schema.html)
  layout \> layoutAttributes \> xaxis/yaxis for options. To add to
  single axis, name it as a list. If attributes are specified without an
  enclosing xaxis or yaxis list, they will be applied to both axes.

  Examples:

  - `NPex$data$plot(xaxis = list(tickcolor = "black", tickfont = list(family = "Arial", size = 14, color = "black"))) #applies to x axis only`

  - `NPex$data$plot(linecolor = "red", ticks = "inside") #applies to both axes`

  .

## Value

Plots the object.

## Details

This method will plot any two columns, specified using a formula, of a
PMcov object, which contains covariate and Bayesian posterior parameter
information for each subject. Specifiying any two variables that do not
include time will result in a scatter plot with optional regression and
reference lines. If time is included as the x variable, the y variable
will be plotted vs. time, aggregated by subject. This can be useful to
see time varying parameters, although a formula within formula approach
may be required, e.g. `$plot(I(cl_0*wt**0.75) ~ time)` in order to see
the change in cl over time according to the change in wt over time, even
though cl_0 is constant for a given subject.

## See also

[PM_cov](https://lapkb.github.io/Pmetrics_rust/reference/PM_cov.md),
[PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md),
[plotly::schema](https://rdrr.io/pkg/plotly/man/schema.html)

Other PMplots:
[`plot.PM_cycle()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_cycle.md),
[`plot.PM_data()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_data.md),
[`plot.PM_final()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_final.md),
[`plot.PM_model()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_model.md),
[`plot.PM_op()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_op.md),
[`plot.PM_opt()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_opt.md),
[`plot.PM_pop()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_pop.md),
[`plot.PM_post()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_post.md),
[`plot.PM_pta()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_pta.md),
[`plot.PM_sim()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_sim.md),
[`plot.PM_valid()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_valid.md)

## Author

Michael Neely

## Examples

``` r
#'
if (FALSE) { # \dontrun{
NPex$cov$plot(V ~ wt)
NPex$cov$plot(Ke ~ wt, line = list(lm = TRUE, ref = FALSE, loess = FALSE))
NPex$cov$plot(Ke ~ wt, line = list(loess = list(ci = 0.9, color = "green")))
NPex$cov$plot(V ~ time, marker = list(color = "blue"))
NPex$cov$plot(V ~ wt,
  line = list(lm = TRUE, loess = FALSE),
  stats = list(x = 0.5, y = 0.2, font = list(size = 7, color = "blue"))
)
} # }
```
