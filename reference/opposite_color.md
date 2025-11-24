# Find opposite color with max contrast

**\[stable\]** Finds an opposite color with maximum contrast to the
input color.

## Usage

``` r
opposite_color(
  col,
  method = c("complement", "complement_maxcontrast", "bw_maxcontrast"),
  degrees = 180
)
```

## Arguments

- col:

  A color name or hex string (e.g. "red", "#FF0000", "#FF0000FF").

- method:

  The method to use for finding the opposite color. One of "complement",
  "complement_maxcontrast", or "bw_maxcontrast". Default is
  "complement".

- degrees:

  The degree offset for the hue complement. Default is 180.

## Value

A hex color string in the format "#RRGGBBAA".

## Details

This function takes a color input (name or hex) and returns an opposite
color using one of three methods:

- "complement": strict 180 degrees hue complement

- "complement_maxcontrast": 180 degrees hue complement adjusted for
  maximum contrast

- "bw_maxcontrast": black or white, whichever has higher contrast The
  function uses the WCAG relative luminance and contrast ratio formulas
  to determine contrast.
