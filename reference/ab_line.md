# Add lines to plotly plot

**\[stable\]**

Analogous to [abline](https://rdrr.io/r/graphics/abline.html), draws
horizontal, vertical or sloped reference lines.

## Usage

``` r
ab_line(a = NULL, b = NULL, h = NULL, v = NULL, line = TRUE)
```

## Arguments

- a:

  Intercept y value in relative coordinates, i.e. 0 (bottom) to 1 (top).
  The x value is 0.

- b:

  Slope

- h:

  Y-intercept of horizontal line, in absolute coordinates

- v:

  X-intercept of vertical line, in absolute coordinates

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

  Example: `line = list(color = "red", dash = "longdash", width = 2)` .
  Default is `line = list(color = "black", width = 1, dash = "dash")`.

## Details

This function creates a line shape that can be added a plotly plot. See
schema() \> layout \> layoutAttributes \> shapes for details. Use only
one of the following:

- a and b to specify a line with intercept and slope

- h to specify a horizontal line with y-intercept at `h`

- v to specify a vertical line with x-intercept at `v`

If using this function to add a line to an existing plot, it must be
used with
[add_shapes](https://lapkb.github.io/Pmetrics_rust/reference/add_shapes.md).
If used for a new plot, it can be included as an element in the layout
list.

## See also

[add_shapes](https://lapkb.github.io/Pmetrics_rust/reference/add_shapes.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# add to an existing plot
NPex$op$plot() %>%
  add_shapes(shapes = ab_line(v = 12))
# add to a new plot
plotly::plot_ly(x = 1:10, y = 1:10, type = "scatter", mode = "lines+markers") %>%
  plotly::layout(shapes = ab_line(h = 5, line = list(color = "red", dash = "solid")))
} # }
```
