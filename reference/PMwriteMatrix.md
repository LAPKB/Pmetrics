# Write a Pmetrics .csv Matrix File

**\[superseded\]**

This function is largely superseded as the function is accessed with the
`$save()` method for
[PM_data](https://lapkb.github.io/Pmetrics_rust/reference/PM_data.md)
objects. There is rarely a need to call it directly. It is the companion
function to
[PMreadMatrix](https://lapkb.github.io/Pmetrics_rust/reference/PMreadMatrix.md).
It will write an appropriate R data object to a formatted .csv file.

## Usage

``` r
PMwriteMatrix(
  data,
  filename,
  override = FALSE,
  version = "DEC_11",
  header = FALSE
)
```

## Arguments

- data:

  Must be a data.frame with appropriate structure (see
  [PMcheck](https://lapkb.github.io/Pmetrics_rust/reference/PMcheck.md)).

- filename:

  Name of file to create.

- override:

  Boolean operator to write even if errors are detected. Default is
  `FALSE`.

- version:

  Which matrix data format version to write. Default is the current
  version.

- header:

  Is there a header row? Default is `FALSE` as this was the legacy
  format.

## Value

Returns the error report (see
[PMcheck](https://lapkb.github.io/Pmetrics_rust/reference/PMcheck.md)
for details).

## Details

*PMwriteMatrix* will first run
[PMcheck](https://lapkb.github.io/Pmetrics_rust/reference/PMcheck.md) to
determine if there are any errors in the structure of `data`. If the
error check fails, the file will not be written and a message will be
printed on the console.

## See also

[PM_data](https://lapkb.github.io/Pmetrics_rust/reference/PM_data.md),
[PMcheck](https://lapkb.github.io/Pmetrics_rust/reference/PMcheck.md),
[PMreadMatrix](https://lapkb.github.io/Pmetrics_rust/reference/PMreadMatrix.md)

## Author

Michael Neely

## Examples

``` r
if (FALSE) { # \dontrun{
# write to the current directory
NPex$data$save("data.csv")
} # }
```
