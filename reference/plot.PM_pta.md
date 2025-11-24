# Plot PM_pta Percent Target Attainment objects

**\[stable\]**

This function will plot the percent target attainment for associated
with simulations.

## Usage

``` r
# S3 method for class 'PM_pta'
plot(
  x,
  at = "intersect",
  include,
  exclude,
  type = "pta",
  mult = 1,
  outeq = 1,
  line = TRUE,
  marker = TRUE,
  ci = 0.9,
  legend = TRUE,
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

  The name of an *PM_pta* data object

- at:

  Which object in the *PM_pta* result list to plot. By default
  "intersect" if an intersection is present due to creation of the
  object with multiple target types, or 1 if no intersection is present,
  which means only 1 target type was selected. If "intersect" is present
  in the object, the default can be overridden with a number to plot one
  of the individual PTAs, e.g. `at = 2` to plot the second PTA rather
  than the intersection of all the PTAs.

- include:

  A vector of subject IDs to include in the plot, e.g. c(1:3,5,15)

- exclude:

  A vector of subject IDs to exclude in the plot, e.g. c(4,6:14,16:20)

- type:

  Character vector controlling type of plot. Default is "pta", which
  plots proportion with success on the y-axis and target on the x-axis.
  The other choice is "pdi", which plots the median pdi (pharmacodynamic
  index), e.g. AUC/MIC, on the y-axis, and target on the x-axis.

- mult:

  Multiplication factor for y axis, e.g. to convert mg/L to ng/mL

- outeq:

  Which output equation to plot. Default is 1.

- line:

  Controls characteristics of lines. This argument maps to the plotly
  line object. It can be boolean or a list. `TRUE` will plot the line
  with default characteristics for each simulated regimen. `FALSE` will
  suppress line plotting. If a list, it functions a little differently
  than other Pmetrics plotly functions. Rather than controlling
  individual line characteristics, for this plot, the `line` argument
  should be a list of the options for group based plotting, where each
  group corresponds to a simulated regimen. The possible elements of the
  `line` list should be exactly named:

  - color Maps to the
    [plotly::plot_ly](https://rdrr.io/pkg/plotly/man/plot_ly.html)
    `colors` argument to override default colors applied to the lines
    for each regimen. This can be a named palette, which can be obtained
    with
    [`RColorBrewer::display.brewer.all()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html)
    or a vector of hexadecimal color names. One way to ensure reliable
    color palettes is to use the
    [ColorBrewer](https://colorbrewer2.org/#type=qualitative&scheme=Accent&n=6)
    site. Choosing the number of data classes to correspond to regimens,
    and qualitative data results in a distinct palette. Easiest
    importing into R is to copy/paste the Export of JavaScript on the
    ColorBrewer website. The default is "Set1". Palettes with fewer
    colors than regimens will be recycled. A color can also be a
    character vector of color names, recycled as needed. For example, a
    print-friendly choice is `line = list(color = "black")`.

  - width Maps to the
    [plotly::plot_ly](https://rdrr.io/pkg/plotly/man/plot_ly.html)
    `width` argument to override default widths applied to the lines for
    each regimen. All lines will have the same width. The default value
    is 2.

  - dash Maps to the
    [plotly::plot_ly](https://rdrr.io/pkg/plotly/man/plot_ly.html)
    `linetypes` argument to override default styles applied to the lines
    for each regimen. If numeric, will map to `lty`
    [par](https://rdrr.io/r/graphics/par.html) values. It can also be a
    character vector of dash names as listed in
    [plotly::plot_ly](https://rdrr.io/pkg/plotly/man/plot_ly.html).
    Example: `line = list(color = "Blues", width = 1, dash = 2)`, whicb
    will result in dotted lines (dash = 2) all with width 1 but in
    different shades of blue.

- marker:

  Controls the plotting symbol. This argument maps to the plotly marker
  object. It can be boolean or a list. `TRUE` will plot the profiles
  with default characteristics for each simulated regimen. `FALSE` will
  suppress line plotting. If a list, it functions a little differently
  than other Pmetrics plotly functions. Rather than controlling
  individual marker characteristics, for this plot, the `marker`
  argument should be a list of the options for group based plotting,
  where each group corresponds to a simulated regimen. The possible
  elements of the `marker` list should be exactly named:

  - color Default marker color is the same as the line color. If line
    color is specified, marker color does not need to also be specified.
    Even if line plotting is suppressed with `line = F`, the default
    color value of "Set1" will be applied to markers, unless specified,
    e.g. `marker = list(color = "Blues")`.

  - symbol Maps to the
    [plotly::plot_ly](https://rdrr.io/pkg/plotly/man/plot_ly.html)
    `symbols` argument to override default symbols applied to the
    markers for each regimen. If only one value is supplied for this, it
    will be recycled for each regimen, i.e. all will have the same
    symbol. See
    [`plotly::schema()`](https://rdrr.io/pkg/plotly/man/schema.html),
    traces \> scatter \> attributes \> marker \> symbol \> values for
    options.

  - size Maps to the
    [plotly::plot_ly](https://rdrr.io/pkg/plotly/man/plot_ly.html)
    `size` argument to override default size applied to the markers for
    each regimen. All markers will have the same size. The default value
    is 12.

- ci:

  Confidence interval around curves on `type = "pdi"` plot, on scale of
  0 to 1. Default is 0.9.

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

  Default will be the labeled regimen names as an argument when creating
  a [PM_pta](https://lapkb.github.io/Pmetrics_rust/reference/PM_pta.md)
  object, or if missing, "Regimen 1, Regimen 2,...Regimen n", where *n*
  is the number of regimens in the PM_pta object.

- log:

  Boolean operator to plot the x axis in log base 10. This argument maps
  to the xaxis type value in the layout object in plotly. Many other
  Pmetrics plots map this argument to the y axis, but for PTA plots, the
  x axis is the more common axis to log transform. Use the plotly
  [`plotly::schema()`](https://rdrr.io/pkg/plotly/man/schema.html)
  command in the console and navigate to layout \> layoutAttributes \>
  xaxis \> type. Example: `log = T`

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

  Default is "Target" when targets are discrete, and "Regimen" when
  targets are sampled.

- ylab:

  Value for y axis label. This argument maps to the the yaxis title
  element of the layout object in plotly. See `xlab` for details. If
  `xlab` is specified as a list with formatting, and `ylab` is simply a
  character label, then the formatting for the `xlab` will be applied to
  the `ylab`. To format `ylab` independently, specify a formatting list
  as for `xlab`.

  Default is "Proportion with success" for plot `type = "pta"` and
  "Pharmacodynamic Index" for plot `type = "pdi"`.

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

[PM_pta](https://lapkb.github.io/Pmetrics_rust/reference/PM_pta.md)
objects are made with the `$pta` method for
[PM_sim](https://lapkb.github.io/Pmetrics_rust/reference/PM_sim.md) or
with `PM_pta$new()`.

## See also

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
[`plot.PM_sim()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_sim.md),
[`plot.PM_valid()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_valid.md)

## Author

Michael Neely

## Examples

``` r
if (FALSE) { # \dontrun{
pta1 <- simEx$pta(
  simlabels <- c("600 mg daily", "1200 mg daily", "300 mg bid", "600 mg bid"),
  targets = c(0.25, 0.5, 1, 2, 4, 8, 16, 32), target.type = "time",
  success = 0.6, start = 120, end = 144
)
pta1$summary()
pta1$plot()
} # }
```
