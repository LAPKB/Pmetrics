# Get color palette

**\[stable\]**

Generate list of palettes for plots.

## Usage

``` r
getPalettes()
```

## Value

A character vector of palette names.

## Details

If RColorBrewer package is installed, will return the list of palette
names from RColorBrewer::brewer.pal.info. If not, will return the
current list as of April 2024.

## Author

Michael Neely

## Examples

``` r
if (FALSE) { # \dontrun{
getPalettes()
} # }
```
