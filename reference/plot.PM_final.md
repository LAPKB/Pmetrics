# Plot Pmetrics Final Cycle Parameter Value Distributions

**\[stable\]**

Plot R6
[PM_final](https://lapkb.github.io/Pmetrics_rust/reference/PM_final.md)
objects loaded as a field in the
[PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
object, e.g. `PM_result$final`.

## Usage

``` r
# S3 method for class 'PM_final'
plot(
  x,
  formula = NULL,
  line,
  marker = TRUE,
  standardize,
  legend,
  log,
  grid = TRUE,
  xlab,
  ylab,
  zlab,
  title,
  xlim,
  ylim,
  print = TRUE,
  ...
)
```

## Arguments

- x:

  The name of an
  [PM_final](https://lapkb.github.io/Pmetrics_rust/reference/PM_final.md)
  data object as a field in a
  [PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
  R6 object, e.g `PM_result$final`.

- formula:

  An optional formula of the form `y ~ x`, where `y` and `x` are two
  model parameters to plot in a 3-dimensional bivariate plot. See
  details.

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
  The `line` argument is used to format:

  - the density line drawn over an NPAG
    [PM_final](https://lapkb.github.io/Pmetrics_rust/reference/PM_final.md)
    object. Default is `FALSE`, which means no density line will be
    drawn. Use `TRUE` to draw the default line, which is black, solid,
    and of width 1, or specify as a list to control these elements, e.g.
    `line = list(color = "red", width = 2, dash = "dash")`

  - the drop lines from point to lower surface when a `formula` is
    specified to generate a bivariate plot from an NPAG
    [PM_final](https://lapkb.github.io/Pmetrics_rust/reference/PM_final.md)
    object. In this case, default is `line = TRUE`. The default format
    is black, dashed, and of width 1.

  - the lines drawing the normal distribution of parameter values from
    an IT2B
    [PM_Final](https://lapkb.github.io/Pmetrics_rust/reference/PM_final.md)
    object. Again, here the default is `line = TRUE`, and the format is
    black, solid, of width 1. See
    [density](https://rdrr.io/r/stats/density.html). Ignored for IT2B
    output.

- marker:

  See details for which objects the `marker` argument controls. This
  argument maps to the plotly marker object. It can be boolean or a
  list. `TRUE` will plot the marker with default characteristics.
  `FALSE` will suppress marker plotting. If a list, can control many
  marker characteristics, including overriding defaults. Use the plotly
  [`plotly::schema()`](https://rdrr.io/pkg/plotly/man/schema.html)
  command in the console and navigate to traces \> scatter \> attributes
  \> marker to see all the ways the marker can be formatted. Most common
  will be:

  - `color` Fill color for NPAG bars, marker color for bivariate NPAG
    plots. Ignored for IT2B plots.

  - `symbol` Plotting character. See
    [`plotly::schema()`](https://rdrr.io/pkg/plotly/man/schema.html),
    traces \> scatter \> attributes \> marker \> symbol \> values. Only
    relevant for bivariate NPAG plots.

  - `size` Character size in points. Only relevant for bivariate NPAG
    plots.

  - `opacity` Bar fill color for univariate NPAG plots or marker opacity
    for bivariate NPAG plots. Ignored for IT2B plots. Ranges between 0
    (fully transparent) to 1 (fully opaque).

  - `line` A list of additional attributes governing the outline for
    bars in univariate NPAG plots or markers in bivariate NPAG plots.
    Ignored for IT2B plots.

    - `color` Outline color. Default is "black".

    - `width` Outline width. Default is 1. Example:
      `marker = list(color = "red", symbol = "triangle", opacity = 0.8, line = list(color = "black", width = 2))`

- standardize:

  Standardize the normal parameter distribution plots from IT2B to the
  same scale x-axis. Ignored for NPAG output.

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

  Default is the name of the plotted x-variable. Formatting can be
  controlled, but the text is recommended to not be changed.

- ylab:

  Value for y axis label. This argument maps to the the yaxis title
  element of the layout object in plotly. See `xlab` for details. If
  `xlab` is specified as a list with formatting, and `ylab` is simply a
  character label, then the formatting for the `xlab` will be applied to
  the `ylab`. To format `ylab` independently, specify a formatting list
  as for `xlab`.

  Default is "Probability" for univariate plots, and the name of the
  plotted y-variable for bivariate plots. Formatting can be controlled,
  but the text is recommended to not be changed.

- zlab:

  Only for bivariate plots. Default is "Probability". Can be a list to
  control formatting or default text, as for `xlab` and `zlab`.

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

  Default is to have no title on plots.

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

This is a function usually called by the `$plot()` method for
[PM_final](https://lapkb.github.io/Pmetrics_rust/reference/PM_final.md)
objects within a
[PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
to generate a plot the parameter value probability densities after
completion of a model fitting. The function can be called directly on a
[PM_final](https://lapkb.github.io/Pmetrics_rust/reference/PM_final.md)
object.

If `formula` is omitted, this will generate a marginal plot for each
parameter. For NPAG data, this will be a histogram of marginal values
for each parameter and the associated probability of that value. For
IT2B, this will be a series of normal distributions with mean and
standard deviation equal to the mean and standard deviation of each
parameter marginal distribution.

On the other hand, if `formula` is specified as two parameters, e.g.
CL~V, this will generate a bivariate plot. For NPAG data, it will be
support point with size proportional to the probability of each point.
For IT2B, it will be an elliptical distribution of a bivariate normal
distribution centered at the mean of each plotted variable and
surrounding quantiles of the bivariate distribution plotted in
decreasing shades of grey. Mulitvariate normal density code is borrowed
from the mvtnorm package.

## See also

[PM_final](https://lapkb.github.io/Pmetrics_rust/reference/PM_final.md),
[plotly::schema](https://rdrr.io/pkg/plotly/man/schema.html)

Other PMplots:
[`plot.PM_cov()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_cov.md),
[`plot.PM_cycle()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_cycle.md),
[`plot.PM_data()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_data.md),
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
if (FALSE) { # \dontrun{
# NPAG
NPex$final$plot()
NPex$final$plot(line = TRUE)
NPex$final$plot(Ke ~ V)
# IT2B
ITex$final$plot()
ITex$final$plot(Ke ~ V)
} # }
```
