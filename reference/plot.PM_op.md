# Plot Pmetrics Observed vs. Predicted Objects

**\[stable\]**

Plot PM_op objects

## Usage

``` r
# S3 method for class 'PM_op'
plot(
  x,
  line = list(lm = NULL, loess = NULL, ref = NULL),
  marker = TRUE,
  resid = FALSE,
  icen = "median",
  pred.type = "post",
  outeq = 1,
  block,
  include,
  exclude,
  mult = 1,
  legend,
  log = FALSE,
  grid = TRUE,
  xlab,
  ylab,
  title,
  stats = TRUE,
  print = TRUE,
  xlim,
  ylim,
  ...
)
```

## Arguments

- x:

  The name of a
  [PM_op](https://lapkb.github.io/Pmetrics_rust/reference/PM_op.md) data
  object and loaded with
  [PM_load](https://lapkb.github.io/Pmetrics_rust/reference/PM_load.md)
  as a field in a
  [PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md),
  e.g. `PM_result$op`.

- line:

  Controls characteristics of lines. Unlike some other Pmetrics plots,
  for plot.PM_op, `line` is a list of three elements:

  - `lm` If set to `TRUE` (default) or a list of plotly line attributes,
    will generate a linear regression of the form obs ~ pred. Line
    attributes will control the appearance of the regression line and
    the confidence interval around the line. If set to `FALSE`, no
    linear regression will be generated. The default values for the
    elements of the `lm` list, all of which can be overriden are:

    - `ci` Confidence interval around the regression, default 0.95.

    - `color` Color of the regression line and the confidence area
      around the line, but at opacity = 0.2. Default is "dodgerblue".

    - `width `Width of the regression line, default 1.

    - `dash` See
      [`plotly::schema()`](https://rdrr.io/pkg/plotly/man/schema.html),
      traces \> scatter \> attributes \> line \> dash \> values. Default
      is "solid". Example:
      `line = list(lm = list(color = "red", dash = "longdash", width = 2))`

  - `loess` If set to `TRUE` (default is `FALSE`) or a list of plotly
    line attributes, will generate a loess regression of the form obs ~
    pred. The list elements and default values in the `loess` list are
    the same as for `lm` except the default style is "dash". Example:
    `line = list(lm = FALSE, loess = TRUE)`

  - `ref` If set to `TRUE` (default) or a list of plotly line
    attributes, will generate a reference line with slope = 1 and
    intercept = 0. The default values for the elements of the `ref` list
    are:

    - `color` "grey".

    - `width` 1.

    - `dash` "dot". Note that there is no *ci* argument for the *ref*
      list. Example:
      `line = list(lm = FALSE, loess = TRUE, ref = list(color = "lightgrey"))`
      If the `line` argument is missing, it will be set to
      `line = list(lm = TRUE, loess = FALSE, ref = TRUE)`, i.e. there
      will be a linear regression with reference line, but no loess
      regression. However, if `resid = T`, the default will become
      `line = list(lm = FALSE, loess = TRUE, ref = TRUE)`, i.e., loess
      regression with reference line, but no linear regression.

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
  The color of any BLQ points is set to a color 90 degrees different on
  the color wheel from the color of the other points using
  [opposite_color](https://lapkb.github.io/Pmetrics_rust/reference/opposite_color.md)
  to ensure good contrast. The symbol for BLQ points is fixed to
  "triangle-down", and the opacity is fixed to 1. Size is the same as
  for other points.

- resid:

  Boolean operator to generate a plot of weighted prediction error vs.
  time, a plot of weighted prediction error vs. prediction. Prediction
  error is pred - obs. By default a loess regression will indicate
  deviation from zero prediction error.

- icen:

  Can be either "median" for the predictions based on medians
  of`pred.type` parameter value distributions, or "mean". Default is
  "median".

- pred.type:

  Either 'post' for a posterior object or 'pop' for a population object.
  Default is 'post'.

- outeq:

  Which output equation to plot. Default is 1.

- block:

  Which block to plot, where blocks are defined by dose reset events
  (EVID = 4) in the data. Default is missing, which results in all
  blocks included.

- include:

  A vector of subject IDs to include in the plot, e.g. c(1:3,5,15)

- exclude:

  A vector of subject IDs to exclude in the plot, e.g. c(4,6:14,16:20)

- mult:

  Multiplication factor for y axis, e.g. to convert mg/L to ng/mL

- legend:

  Ignored for this plot.

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

  If missing, will default to "Predicted" for plots when `resid = F` and
  either "Time" or "Predicted" for residual plots.

- ylab:

  Value for y axis label. This argument maps to the the yaxis title
  element of the layout object in plotly. See `xlab` for details. If
  `xlab` is specified as a list with formatting, and `ylab` is simply a
  character label, then the formatting for the `xlab` will be applied to
  the `ylab`. To format `ylab` independently, specify a formatting list
  as for `xlab`.

  If missing, will default to "Observed" for plots when `resid = F` and
  either "Individual weighted residuals" or "Population weighted
  residuals" for residual plots, depending on the value of `pred.type`.

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
  `list(x= 0.8, y = 0.1, font = list(color = "black", family = "Arial", size = 14, bold = FALSE))`.
  The coordinates are relative to the plot with lower left = (0,0),
  upper right = (1,1). This argument maps to
  [`plotly::add_text()`](https://rdrr.io/pkg/plotly/man/add_trace.html).

- print:

  If `TRUE`, will print the plotly object and return it. If `FALSE`,
  will only return the plotly object.

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

This is a function usually called by the `$plot()` method for
[PM_op](https://lapkb.github.io/Pmetrics_rust/reference/PM_op.md)
objects within a
[PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
to generate a plot of Observed vs. Predicted observations. The function
can be called directly on a
[PM_op](https://lapkb.github.io/Pmetrics_rust/reference/PM_op.md)
object. The default is to generate an observed vs. predicted plot of
predictions based on the median of the Bayesian posterior distributions
for each subject. Missing observations are excluded. Observations
reported as BLQ (below the limit of quantification) are indicated as
downward triangles, and colored differently from other observations.
They are plotted at the reported LOQ on the observed axis and the
predicted value on the predicted axis. They are not included in any
regression lines or statistics.

Clicking on a point in the plot will highlight all points from that
subject. The color of the highlight is 180 degrees different on the
color wheel from the color of the other points
([opposite_color](https://lapkb.github.io/Pmetrics_rust/reference/opposite_color.md)),
ensuring good contrast. Clicking on the plot background will remove the
highlighting.

## See also

[PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md),
[PM_op](https://lapkb.github.io/Pmetrics_rust/reference/PM_op.md),
[plotly::schema](https://rdrr.io/pkg/plotly/man/schema.html)

Other PMplots:
[`plot.PM_cov()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_cov.md),
[`plot.PM_cycle()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_cycle.md),
[`plot.PM_data()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_data.md),
[`plot.PM_final()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_final.md),
[`plot.PM_model()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_model.md),
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
if (FALSE) { # \dontrun{
NPex$op$plot()
NPex$op$plot(pred.type = "pop")
NPex$op$plot(line = list(lm = TRUE, ref = TRUE, loess = FALSE))
NPex$op$plot(line = list(loess = list(ci = 0.9, color = "green")))
NPex$op$plot(marker = list(color = "blue"))
NPex$op$plot(resid = TRUE)
NPex$op$plot(stats = list(x = 0.5, y = 0.2, font = list(size = 7, color = "blue")))
} # }
```
