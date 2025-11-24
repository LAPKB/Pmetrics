# Plot PM_model objects

**\[stable\]**

Plots a
[PM_model](https://lapkb.github.io/Pmetrics_rust/reference/PM_model.md)
based on differential equations using network plots from tidygraph and
ggraph packages.

## Usage

``` r
# S3 method for class 'PM_model'
plot(x, marker = TRUE, line = TRUE, explicit, implicit, print = TRUE, ...)
```

## Arguments

- x:

  The name of an
  [PM_model](https://lapkb.github.io/Pmetrics_rust/reference/PM_model.md)
  object.

- marker:

  Controls the characteristics of the compartments (nodes). It can be
  boolean or a list. `TRUE` will plot the compartments with default
  characteristics. `FALSE` will suppress compartment plotting. If a
  list, can control some marker characteristics, including overriding
  defaults. These include:

  - `color` Marker color (default: dodgerblue).

  - `opacity` Ranging between 0 (fully transparent) to 1 (fully opaque).
    Default is 0.5.

  - `size` Relative size of boxes, ranging from 0 to 1. Default is 0.25.

  - `line` A list of additional attributes governing the outline for
    filled shapes, most commonly color (default: black) and width
    (default: 0.5).

    
    
  Example:
  `marker = list(color = "red", opacity = 0.8, line = list(color = "black", width = 1))`

- line:

  Controls characteristics of arrows (edges). `TRUE` will plot default
  lines. `FALSE` will suppress lines. If a list, can control some line
  characteristics, including overriding defaults. These include:

  - `color` Line color (default: black)

  - `width` Thickness in points (default: 1).

    
    
  Example: `line = list(color = "red", width = 2)`

- explicit:

  A data frame or tibble containing two columns named `from` and `to` to
  add additional connecting arrows to the plot indicating transfer
  between compartments. For each row, the `from` column contains the
  compartment number of the arrow origin, and the `to` column contains
  the compartment number of the arrow destination. Use 0 to indicate a
  destination to the external sink. e.g.,
  `explicit = data.frame(from = 3, to = 0)`

- implicit:

  Similar to `explicit`, used to add dashed connecting arrows to the
  plot indicating implicit transfer between compartments. For each row,
  the `from` column contains the compartment number of the arrow origin,
  and the `to` column contains the compartment number of the arrow
  destination. Use 0 to indicate a destination to the external sink.
  e.g., `implicit = data.frame(from = 2, to = 4)`

- print:

  If `TRUE`, will print the object and return it. If `FALSE`, will only
  return the object.

- ...:

  Not used.

## Value

A plot object of the model.

## Details

This accepts a
[PM_model](https://lapkb.github.io/Pmetrics_rust/reference/PM_model.md)
object and creates a network plot where nodes are compartments and edges
are arrows connecting compartments.

## See also

[PM_model](https://lapkb.github.io/Pmetrics_rust/reference/PM_model.md),
[`ggraph::ggraph()`](https://ggraph.data-imaginist.com/reference/ggraph.html),
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)

Other PMplots:
[`plot.PM_cov()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_cov.md),
[`plot.PM_cycle()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_cycle.md),
[`plot.PM_data()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_data.md),
[`plot.PM_final()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_final.md),
[`plot.PM_op()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_op.md),
[`plot.PM_opt()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_opt.md),
[`plot.PM_pop()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_pop.md),
[`plot.PM_post()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_post.md),
[`plot.PM_pta()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_pta.md),
[`plot.PM_sim()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_sim.md),
[`plot.PM_valid()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_valid.md)

## Author

Markus Hovd, Julian Otalvaro, Michael Neely

## Examples

``` r
if (FALSE) { # \dontrun{
NPex$model$plot()
} # }
```
