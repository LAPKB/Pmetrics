# Plot Pmetrics Validation Objects

**\[stable\]** Usually called by the `$plot` method for
[PM_valid](https://lapkb.github.io/Pmetrics_rust/reference/PM_valid.md)
objects, which are in turn typically added to a
[PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
object by the `$validate` method. For example:

       NPex$validate(limits = c(0, 3)) # creates a PM_valid object and adds it to the $valid field of NPex
       NPex$valid$plot(type = "vpc", tad = TRUE, log = TRUE) # now we can plot it

Plot
[PM_valid](https://lapkb.github.io/Pmetrics_rust/reference/PM_valid.md)
objects.

## Usage

``` r
# S3 method for class 'PM_valid'
plot(
  x,
  type = "vpc",
  tad = FALSE,
  outeq = 1,
  line = TRUE,
  marker = TRUE,
  legend = FALSE,
  log = FALSE,
  grid = TRUE,
  xlab,
  ylab,
  title,
  xlim,
  ylim,
  print = TRUE,
  ...
)
```

## Arguments

- x:

  The name of an *PM_valid* data object, which is usually called by the
  `$validate` method for
  [PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
  objects.

- type:

  Default is "vpc" for a visual predictive check, but could be "pcvpc"
  for a prediction-corrected visual predictive check, or "npde" for a
  normalized prediction distribution error analysis/plot. Choosing npde
  will call
  [npde::plot.NpdeObject](https://rdrr.io/pkg/npde/man/plot.NpdeObject.html).
  To modify this plot, supply argmuents as a named list: `npde = (...)`.
  Available arguments are in the user manual for the [npde
  package](https://github.com/ecomets/npde30).

- tad:

  Boolean operator to use time after dose rather than time after start.
  Default is `FALSE`.

- outeq:

  Which output equation to plot. Default is 1.

- line:

  A list of three elements `$upper`, `$mid`, and `$lower`, each of which
  controls characteristics of corresponding quantiles. The arguments to
  each of these list elements map to several plotly attributes. Each can
  be a boolean value or a list. `TRUE` will plot default
  characteristics. `FALSE` will suppress quantile plots. The elements of
  the list for each argument are as follows:

  - `value` The quantile value. Default for lower is 0.025, mid is 0.5,
    and upper is 0.975.

  - `color` The color for both the 95%CI region around simulated
    quantile vs. time, and the color of the line for the observation
    quantile vs. time. Default for lower and upper is "dodgerblue" and
    for mid it is "red".

  - `dash` The style of the obervation quantile line. See
    [`plotly::schema()`](https://rdrr.io/pkg/plotly/man/schema.html),
    traces \> scatter \> attributes \> line \> dash \> values. Default
    for lower and upper is "dash" and for mid it is "solid".

  - `width` Default is 1 for lower, mid, and upper.

  - `opacity` The opacity of the 95%CI region around simulated quantile
    vs. time. Default is 0.4 for lower, mid and upper, but can range
    between 0 (fully transparent) to 1 (fully opaque). Example:
    `line = list(upper = list(value = 0.9, color = "red", dash = "longdash", opacity = 0.5, width = 2))`

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

- legend:

  Controls display of legend. This argument maps to the plotly layout
  showlegend and legend arguments. It is either a boolean operator (most
  common) or a list of parameters to be supplied to plotly. See
  [`plotly::schema()`](https://rdrr.io/pkg/plotly/man/schema.html) \>
  layout \> layoutAttributes \> legend and showlegend for more details
  on the available options for formatting. If legend is supplied as a
  list, the plotly layout \> layoutAttributes \> showlegend value will
  be set to `TRUE` automatically.

  Examples:

  - `legend = T`

  - `legend = list(orientation = "h", font = list(color = "blue"))`

  Default is `FALSE`

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

  Default is "Time" or "Time after dose" if `tad = TRUE`.

- ylab:

  Value for y axis label. This argument maps to the the yaxis title
  element of the layout object in plotly. See `xlab` for details. If
  `xlab` is specified as a list with formatting, and `ylab` is simply a
  character label, then the formatting for the `xlab` will be applied to
  the `ylab`. To format `ylab` independently, specify a formatting list
  as for `xlab`.

  Default is "Output".

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

  If `type` is not "npde", the following apply. Other attributes which
  can be passed to the layout \> xaxis/yaxis in a plotly plot to further
  control formatting. Note that `log`, `xlab`, `ylab`, `xlim`, and
  `ylim` are all controlled by the layout object, but are provided
  throughout Pmetrics plotly function arguments as shortcuts that map to
  layout elements. Therefore, the dots argument should be used to
  specify other aspects of the x axis, y axis, or both. See
  [`plotly::schema()`](https://rdrr.io/pkg/plotly/man/schema.html)
  layout \> layoutAttributes \> xaxis/yaxis for options. To add to
  single axis, name it as a list. If attributes are specified without an
  enclosing xaxis or yaxis list, they will be applied to both axes.

  Examples:

  - `NPex$data$plot(xaxis = list(tickcolor = "black", tickfont = list(family = "Arial", size = 14, color = "black"))) #applies to x axis only`

  - `NPex$data$plot(linecolor = "red", ticks = "inside") #applies to both axes`

  . . However, if `type` is "npde", to modify the appearance of the
  plot, supply a list of options, `npde = list(...)`. See the
  documentation for the `type` argument above.

## Value

Plots and returns the plotly object

## Details

Generates a plot of outputs (typically concentrations) on the y axis and
time on the x axis. If `tad` was set to `TRUE` when
[make_valid](https://lapkb.github.io/Pmetrics_rust/reference/make_valid.md)
was called, then time may be either absolute (default) or time after
dose, controlled by the `tad` argument to this plot function. The
following items are included in the plot:

- Observed outputs. These may be either as measured for `type = "vpc"`
  or prediction corrected for `type = "pcvpc"`. Format of the
  observations is controlled by the `marker` argument. The default is
  `list(color = "black", symbol = "circle-open", size = 8)`.

- Quantiles vs. time for observations. These are plotted by default as
  dashed blue lines for the 2.5th and 97.5th percentiles and a solid red
  line for the median. Formatting and the value for each quantile can be
  controlled with the `upper`, `mid`, and `lower` arguments.

- 95% CI around the same quantiles of combined simulations from each
  subject. The values and formatting for these quantile CIs are the same
  as for the observations, and also controlled with the `upper`, `mid`,
  and `lower` arguments.

Good vpc/pcvpc plots are considered to be those where the quantile lines
for the oberservations lie within the 95%CI quantile regions for
simulations, indicated that the model is "centered" on the data and
faithfully captures the variability in the data. For an npde plot, one
expects to see approximately normally distributed normalized prediction
errors.

## See also

[make_valid](https://lapkb.github.io/Pmetrics_rust/reference/make_valid.md)

Other PMplots:
[`plot.PM_cov()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_cov.md),
[`plot.PM_cycle()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_cycle.md),
[`plot.PM_data()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_data.md),
[`plot.PM_final()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_final.md),
[`plot.PM_model()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_model.md),
[`plot.PM_op()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_op.md),
[`plot.PM_opt()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_opt.md),
[`plot.PM_pop()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_pop.md),
[`plot.PM_post()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_post.md),
[`plot.PM_pta()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_pta.md),
[`plot.PM_sim()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_sim.md)

## Author

Michael Neely

## Examples

``` r
if (FALSE) { # \dontrun{
# VPC
NPex$valid$plot()

# pcVPC
NPex$valid$plot(type = "pcvpc")

# modify median line and marker
NPex$valid$plot(
  line = list(mid = list(color = "orange", dash = "dashdot")),
  marker = list(
    color = "blue", size = 12, symbol = "diamond",
    line = list(color = "navy")
  )
)
} # }
```
