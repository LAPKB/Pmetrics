# Click on plotly plot to highlight traces

**\[stable\]** Adds click functionality to a plotly plot to highlight
traces when clicked.

## Usage

``` r
click_plot(p, highlight_color = "#1f77b4")
```

## Arguments

- p:

  The plotly plot to which the click functionality should be added.
  Default is the `plotly::last_plot()`.

- highlight_color:

  The color to use for traces that are highlighted. Colors for
  non-highlighted traces will be preserved but dimmed to 20% opacity.
  Default highlight color is `orange()`.

## Details

This function modifies a plotly plot to allow clicking on traces to
highlight them. Clicking on a trace will highlight it in orange
(default) and dim all other traces. Clicking on the same trace again
will deselect it and restore the original colors. Clicking on the
background will also restore all traces to their original colors. The
function uses the
[`htmlwidgets::onRender`](https://rdrr.io/pkg/htmlwidgets/man/onRender.html)
function from the `htmlwidgets` package to add JavaScript code that
handles the click events on the plotly plot.

## Author

Michael Neely
