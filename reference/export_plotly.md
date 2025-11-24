# Export plotly plot

**\[stable\]**

Wrapper around
[`plotly::save_image()`](https://rdrr.io/pkg/plotly/man/save_image.html).

## Usage

``` r
export_plotly(
  p,
  file,
  width = NULL,
  height = NULL,
  scale = NULL,
  units = "px",
  show = TRUE
)
```

## Arguments

- p:

  The plot to which the shape should be added. Default is the
  `plotly::last_plot()`.

- file:

  A file path with a suitable file extension (png, jpg, jpeg, webp, svg,
  or pdf). Unlike `save_image`, the `file` argument may include the full
  path and filename other than in the current working directory.

- width, height:

  The width/height of the exported image in pixels, mutliplied by
  `scale`.

- scale:

  The scale factor to use when exporting the figure. Default is 1.0. A
  scale factor larger than 1.0 will increase the image size, and less
  than 1.0 will decrease the image size. Note that the documentation for
  `save_image` says that this argument changes the resolution, but that
  is not true. The resolution will remain at 72 pixels/inch (28.3
  px/cm), which is the default for R. To increase the resolution, export
  to .pdf or .svg and use an external program, such as Adobe Acrobat,
  Acrobat Reader, or Mac Preview.

- units:

  Units for `width` and `height`. Default is pixels ("px"). Alternatives
  are inches ("in") or centimeters ("cm")

- show:

  Show the exported image in R via
  [`base::file.show()`](https://rdrr.io/r/base/file.show.html). If
  export format is pdf, it will open the system default pdf viewer.
  Default is `TRUE`.

## Value

Plot `p` will be exported to `file` with format determined by the
extension for `file`.

## Details

This function improves the experience with the native plotly method of
exporting plots to static images. Much of the online documentation
points towards using the orca package, but the R help indicates that
this method has been superseded by the kaleido python package, made
accesible in R via the reticulate package and installation of the
miniconda package manager.

These steps are all outlined in the help for
[`plotly::save_image()`](https://rdrr.io/pkg/plotly/man/save_image.html),
but one step is neglected. It is necessary to execute the following line
of code at the end: `reticulate::py_run_string("import sys")`.

This function will check to see that all installations are in place and
offer to install if not.

Many of the arguments are the same as for `save_image` and are passed
directly to that function.

## See also

[`plotly::save_image()`](https://rdrr.io/pkg/plotly/man/save_image.html)

## Author

Michael Neely

## Examples

``` r
if (FALSE) { # \dontrun{
NPex$op$plot(stats = list(x = 0.9)) %>%
  export_plotly(file = "op.png", width = 12, height = 6, units = "in")
} # }
```
