# Check if all values are numeric

**\[stable\]** Checks if all values in a vector are numeric.

## Usage

``` r
all_is_numeric(x, what = c("test", "vector", "nonnum"), extras = c(".", "NA"))
```

## Arguments

- x:

  A vector to check.

- what:

  A character string indicating what to return. Can be "test", "vector",
  or "nonnum". The default is "test".

- extras:

  A character vector of extra values to exclude from the check. The
  default is c(".", "NA").

## Value

A logical value indicating if all values are numeric. If `what` is
"vector", a numeric vector is returned. If `what` is "nonnum", a
character vector of non-numeric values is returned. If `what` is "test",
a logical value is returned.

## Details

The function checks if all values in a vector are numeric. It can be
used to check if a vector contains only numeric values. It can also be
used to check if a vector contains any non-numeric values.

## Examples

``` r
if (FALSE) { # \dontrun{
all_is_numeric(c("1", "2", "3"))
all_is_numeric(c("1", "2", "a"))
all_is_numeric(c("1", "2", "3"), what = "vector")
all_is_numeric(c("1", "2", "a"), what = "nonnum")
} # }
```
