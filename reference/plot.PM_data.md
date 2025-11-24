# Plot PM_data Time-Output Data

**\[stable\]**

Plots *PM_data* objects

## Usage

``` r
# S3 method for class 'PM_data'
plot(
  x,
  include = NULL,
  exclude = NULL,
  line = list(join = TRUE, pred = FALSE),
  marker = TRUE,
  group = NULL,
  group_names = NULL,
  mult = 1,
  outeq = 1,
  out_names = NULL,
  block = 1,
  tad = FALSE,
  overlay = TRUE,
  legend,
  log = FALSE,
  grid = FALSE,
  xlab = "Time",
  ylab = "Output",
  title = "",
  xlim,
  ylim,
  print = TRUE,
  ...
)
```

## Arguments

- x:

  The name of an `PM_data` data object or loaded as a field in a
  [PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
  object

- include:

  A vector of subject IDs to include in the plot, e.g. c(1:3,5,15)

- exclude:

  A vector of subject IDs to exclude in the plot, e.g. c(4,6:14,16:20)

- line:

  Controls characteristics of lines as for all plotly plots. Here `line`
  is a list of two elements:

  - `join` Can either be a boolean or a list. If set to `TRUE` or a list
    of plotly line attributes, it will generate line segments joining
    observations. If set to `FALSE`, no segments will be generated. The
    color of the joining line is the same as the marker color for that
    line. To avoid confusion, the line color cannot be changed. The
    default values for the other elements of the `join` list, both of
    which can be overriden are:

    - `width `Width of the segments, default 1.

    - `dash` See
      [`plotly::schema()`](https://rdrr.io/pkg/plotly/man/schema.html),
      traces \> scatter \> attributes \> line \> dash \> values. Default
      is "solid". Example:
      `line = list(join = list(dash = "longdash", width = 2))`

  - `pred` Default is `FALSE`, which means that predictions will not be
    included in the plot. To include predictions, supply one of the
    following:

  - If plotting data contained in a
    [PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md),
    use "pop" or "post" to include population or posterior predictions.
    \*\* Example 1:
    `run1 <- PM_load(1); run1$data$plot(line = list(pred = "post"))`

  - If plotting data not contained in a
    [PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md),
    you may add the name of a population
    [PM_pop](https://lapkb.github.io/Pmetrics_rust/reference/PM_pop.md)
    or posterior
    [PM_post](https://lapkb.github.io/Pmetrics_rust/reference/PM_post.md)
    prediction object in a
    [PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
    object. This might be useful if you want to see how the predictions
    from one population match the raw data from another. \*\* Example 2:
    `dat <- PM_data$new("new.csv"); dat$plot(line = list(pred = run1$post))`.

  To format the predictions, supply `pred` as a list, with the
  prediction object first, followed by named options to control the
  prediction plot:

  - icen Chooses the median or mean of each subject's Bayesian posterior
    parameter distribution. Default is "median", but could be "mean".

  - As for `join`, the color of the `pred` line is fixed to the same
    color as the marker. Other parameters to pass to plotly to control
    line characteristics that join the predictions are `width`, and
    `dash`. Continuing Example 1 above:
    `pred = list("post", icen = "mean", width = 2)`. Default formats are
    the same as for the `join` argument, since normally one would not
    plot both lines joining observations and prediction lines, i.e.,
    typical use would be `line = list(join = F, pred = "post")`.

- marker:

  Formats the symbols plotting observations. Controls the plotting
  symbol for observations. This argument maps to the plotly marker
  object. It can be boolean or a list. `TRUE` will plot the marker with
  default characteristics. `FALSE` will suppress marker plotting. If a
  list, can control many marker characteristics, including overriding
  defaults. Use the plotly
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
  . When a `group` and/or multiple `outeq` are specified, the `$color`
  element should be a palette or a vector of colors. For accepted
  palette names see
  [`RColorBrewer::brewer.pal.info`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html).
  Examples include "BrBG", or "Set2". An example vector could be
  `c("red", "green", "blue")`. It is not necessary to specify the same
  number of colors as groups within `group`, as colors will be
  interpolated to generate the correct number. The default when `group`
  is specified is the "Set1" palette. When there are groups, the
  `$color` element for join or pred lines will be set to the same as
  `marker$color`.

- group:

  Character vector naming one column in `x` to **group** by, e.g. "id"
  or a covariate like "gender"

- group_names:

  A character vector of names to label the **groups** if
  `legend = TRUE`. This vector must be the same length as the number of
  groups within `group`. If missing, the vector will be generated from
  the unique values in `group`. Example: `c("Male", "Female")` if
  `color = "gender"` and "gender" is a covariate in the data.

- mult:

  Multiplication factor for y axis, e.g. to convert mg/L to ng/mL

- outeq:

  Which output equation to plot. Default is 1. Default is 1, but can be
  multiple if present in the data, e.g. `1:2` or `c(1, 3)`. In the case
  of multiple outputs, `group_colors` will be used to color the lines
  and markers.

- out_names:

  Character vector of names to label the outputs if `legend = TRUE`.
  These can be combined with `group_names`. The number must match the
  number of outputs in `outeq`. If missing, the default is "Output 1",
  "Output 2", etc.

- block:

  Which block to plot, where blocks are defined by dose reset events
  (EVID = 4) in the data. Default is 1, but can be multiple if present
  in the data, as for `outeq`.

- tad:

  Boolean operator to use time after dose rather than time after start.
  Default is `FALSE`.

- overlay:

  Operator to overlay all time concentration profiles in a single plot.
  The default is `TRUE`. If `FALSE`, will trellisplot subjects one at a
  time. Can also be specified as a vector with number of rows and
  columns, e.g. `c(3, 2)` for 3 rows and 2 columns of subject splots to
  include in each trellis.

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

  Default is `FALSE` unless groups are specified with `color`above.

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

  Default is "Time".

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

This function will plot raw and fitted time and concentration data with
a variety of options. By default markers are included and have the
following plotly properties:
`list(symbol = "circle", color = "red", size = 10, opacity = 0.5, line = list(color = "black", width = 1))`.
Markers can be joined by lines, default is `FALSE`. If chosen to be
`TRUE`, the joining lines will have the following properties:
`list(color = "dodgerblue", width = 1, dash = "solid"`. The grid and
legend are omitted by default.

## See also

[PM_data](https://lapkb.github.io/Pmetrics_rust/reference/PM_data.md),
[PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)

Other PMplots:
[`plot.PM_cov()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_cov.md),
[`plot.PM_cycle()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_cycle.md),
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
# basic spaghetti plot
dataEx$plot()
# format line and marker
dataEx$plot(
  marker = list(color = "blue", symbol = "square", size = 12, opacity = 0.4),
  line = list(join = list(color = "orange"))
)
# include predictions with default format and suppress joining lines
dataEx$plot(
  line = list(join = FALSE, pred = NPex$post),
  xlim = c(119, 146)
)
# customize prediction lines
dataEx$plot(
  line = list(
    pred = list(NPex$post, color = "slategrey", dash = "dash"),
    join = FALSE
  )
)
} # }
```
