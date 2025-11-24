# Plot Cycle Information

**\[stable\]**

Plot PM_cycle objects. These objects are created by as part of a
[PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
object when
[PM_load](https://lapkb.github.io/Pmetrics_rust/reference/PM_load.md) is
run.

## Usage

``` r
# S3 method for class 'PM_cycle'
plot(
  x,
  line = TRUE,
  marker = TRUE,
  colors,
  linetypes,
  omit,
  grid = TRUE,
  xlab,
  ylab,
  print = TRUE,
  ...
)
```

## Arguments

- x:

  The name of a
  [PM_cycle](https://lapkb.github.io/Pmetrics_rust/reference/PM_cycle.md)
  object, e.g. `NPex$cycle`.

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
  Default = `list(color = "dodgerblue", width = 2, dash = "solid")`.
  **Note** The width will apply to all plots, but `color` and `dash`
  will only apply to the first three plots (log-likelihood, AIC,
  gamma/lambda). Use `colors` and `linetypes` below to control the
  appearance of the line traces for the normalized plots, because each
  of those traces is mapped to a parameter.

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
  Here, the observation controlled is the value of a given trace at a
  specific cycle number. Default =
  `list(symbol = "circle", color = "dodgerblue", size = 4)`. **Note**
  the marker color for the normalized parameter value plots will be
  controlled by the `colors` parameter below, but size and symbol will
  apply to all plots.

- colors:

  to use for normalized parameter value line traces. This can be a
  palette or a vector of colors. For accepted palette names see
  [`RColorBrewer::brewer.pal.info`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html).
  Examples include "BrBG", or "Set2". An example vector could be
  `c("red", "green", "blue")`. It is not necessary to specify the same
  number of colors parameters to be plotted, as colors will be
  interpolated to generate the correct number. The default when `color`
  is not specified is the "Set2" palette.

- linetypes:

  to use for normalized parameter value line traces. See
  [`plotly::schema()`](https://rdrr.io/pkg/plotly/man/schema.html),
  traces \> scatter \> attributes \> line \> dash \> values. An example
  vector could be `c("solid", "dash", "longdash")`. It is not necessary
  to specify the same number of linetype parameters to be plotted, as
  they will be recycled to generate the correct number. The default when
  `linetypes` is not specified is "solid".

- omit:

  Decimal between 0 and 1 specifying the proportion of "burn-in" cycles
  to omit from the plots. If missing, the first 20% will be omitted.

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

  Controls the formatting of the x-axis label. The text is fixed by the
  function and cannot be altered. Use the plotly
  [`plotly::schema()`](https://rdrr.io/pkg/plotly/man/schema.html)
  command in the console and navigate to layout \> layoutAttributes \>
  xaxis \> title to see the ways to customize this axis label. In
  addition to the plotly attributes, a custom Pmetrics attribute `bold`
  may be included as a list element, either on its own or within the
  font list. The default for `bold` is `TRUE`.  
    
  Examples:

  - `xlab = list(bold = F, font = list(color = "red", family = "Arial", size = 10))`

  - `xlab = list(font = list(bold = T))`

- ylab:

  Format for y-axis label. This argument maps to the the yaxis title
  element of the layout object in plotly. See `xlab` for details. If
  `xlab` is specified as a list with formatting, then the formatting for
  the `xlab` will be applied to the `ylab`. To format `ylab`
  independently, specify a formatting list as for `xlab`.

- print:

  If `TRUE`, will print the plotly object and return it. If `FALSE`,
  will only return the plotly object.

- ...:

  Additional R plotting parameters.

## Value

Plots a panel with the following windows: -2 times the log-likelihood at
each cycle, gamma/lambda at each cycle; Akaike Information Criterion at
each cyle and Bayesian (Schwartz) Information Criterion at each cycle,
the mean parameter values at each cycle (normalized to starting values);
the normalized standard deviation of the population distribution for
each parameter at each cycle; and the normalized median parameter values
at each cycle.

## See also

[PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md),
[plotly::schema](https://rdrr.io/pkg/plotly/man/schema.html)

Other PMplots:
[`plot.PM_cov()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_cov.md),
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
if (FALSE) { # \dontrun{
NPex$cycle$plot()
NPex$cycle$plot(omit = 0, marker = list(symbol = "cross"), line = list(width = 1))
NPex$cycle$plot(
  linetypes = "dash", colors = "Blues", marker = list(size = 1),
  line = list(width = 3)
)
NPex$cycle$plot(
  grid = FALSE,
  xlab = list(bold = FALSE, font = list(color = "red", family = "Arial", size = 10))
)
} # }
```
