# Read a Pmetrics data file

**\[superseded\]**

Reads a Pmetrics .csv matrix input file into R. This function is largely
superseded as the function is called automatically when data are
intialized as a
[PM_data](https://lapkb.github.io/Pmetrics_rust/reference/PM_data.md)
object with `PM_data$new()`. There is rarely need to call
`PMreadMatrix()` directly any longer.

## Usage

``` r
PMreadMatrix(
  file,
  sep = getPMoptions("sep"),
  dec = getPMoptions("dec"),
  quiet = FALSE,
  ...
)
```

## Arguments

- file:

  The name of the file to be loaded, including the full path if not in
  the current working directory (check with
  [getwd](https://rdrr.io/r/base/getwd.html)).

- sep:

  Delimiter between columns, which is a comma by default, but can be
  changed with
  [setPMoptions](https://lapkb.github.io/Pmetrics_rust/reference/setPMoptions.md).

- dec:

  Decimal separator, which is a period by default, but can be changed
  with
  [setPMoptions](https://lapkb.github.io/Pmetrics_rust/reference/setPMoptions.md).

- quiet:

  Default is `FALSE`. If `TRUE`, there will be no report to the console
  on the contents of file.

- ...:

  Other parameters to be passed to
  [`readr::read_delim()`](https://readr.tidyverse.org/reference/read_delim.html)

## Value

`PMreadMatrix` returns a data frame of class "PMmatrix". If the file is
successfully read and `quiet=F`, the column headers will be reported to
the console as a validation check. Note that this function converts the
column headers in the `file` from upper to lowercase for convenient
referencing in R.

## Details

As of Pmetrics version 2, the structure of a valid .csv file relaxed.
Minimal required columns are id, time, dose, and out. This function is
now included as part of the
[PM_data](https://lapkb.github.io/Pmetrics_rust/reference/PM_data.md) R6
object to create new `PM_data` objects. Users should rarely have a need
to call `PMreadMatrix` as a standalone function unless they continue to
use Pmetrics in its legacy mode (versions \< 2.0). Note that support for
legacy Pmetrics will eventually wither as the package evolves.

There are a number of other options for columns in the data input.
Details can be found in the
[documentation](https://lapkb.github.io/Pmetrics/articles/data.html).

## See also

[PMwriteMatrix](https://lapkb.github.io/Pmetrics_rust/reference/PMwriteMatrix.md),
[PMcheck](https://lapkb.github.io/Pmetrics_rust/reference/PMcheck.md),
and
[plot.PM_data](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_data.md)

## Author

Michael Neely
