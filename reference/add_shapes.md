# Add shapes to plotly plot

**\[stable\]**

Modifies the layout object of an existing plot to include a new shape.

## Usage

``` r
add_shapes(p = plotly::last_plot(), shapes)
```

## Arguments

- p:

  The plot to which the shape should be added. Default is the
  `last_plot()`.

- shapes:

  A list of attributes that specify a shape. Note that only one shape
  can be added for each call, but to be consistent with the `shapes`
  argument to `plotly::layout()`, we use the same plural.

## Details

This function adds a shape to the layout element of a plotly plot. Type
`schema()` in the console and expand the list for layout \>
layoutAttributes \> shapes for details on how to specify a shape. A
convenient Pmetrics helper function to add line shapes is
[ab_line](https://lapkb.github.io/Pmetrics_rust/reference/ab_line.md).
Other shapes such as circles, rectangles and paths can be added, but
must be done manually as outlined in the plotly schema documentation. An
example of a circle shape is below in examples.

## See also

[ab_line](https://lapkb.github.io/Pmetrics_rust/reference/ab_line.md)

## Examples

``` r
#'
if (FALSE) { # \dontrun{
NPex$op$plot() %>%
  add_shapes(shapes = ab_line(v = 12))

NPex$data$plot() %>%
  add_shapes(shapes = list(type = "circle", x0 = 125, y0 = 10, x1 = 135, y1 = 15))
} # }
```
