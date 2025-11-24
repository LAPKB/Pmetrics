# Get a list of default colors

**\[stable\]**

Generate list of default color names.

## Usage

``` r
getDefaultColors(n)
```

## Arguments

- n:

  The number of colors to return from the list.

## Value

A character vector of color names, which is recycled as needed.

## Details

Used for Pmetrics plots. The following list is recycled as necessary to
generate the requested number of colors:
`c(red(), green(), blue(), brown(), black(), purple(), pink(), gold(), orange(), gray())`.
Each value is a function that returns a color name.

## Author

Michael Neely

## Examples

``` r
if (FALSE) { # \dontrun{
getDefaultColors(6)
} # }
```
