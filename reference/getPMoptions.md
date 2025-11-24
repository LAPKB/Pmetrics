# Get Pmetrics User Options

**\[stable\]**

Get user options for Pmetrics

## Usage

``` r
getPMoptions(opt, warn = TRUE, quiet = FALSE)
```

## Arguments

- opt:

  The option to retrieve. If omitted, all option values will be
  returned.

- warn:

  Warn if options file doesn't exist. Default `TRUE`.

- quiet:

  Suppress warning messages. Default `FALSE`.

## Value

A list with the current options.

## Details

This function will get user options for Pmetrics. It will look for a
*PMoptions.json* file in a hidden folder outside of the Pmetrics
package. If that does not exist, it will look for a default options file
in the package options folder. See
[setPMoptions](https://lapkb.github.io/Pmetrics_rust/reference/setPMoptions.md)
for details on where the options file is stored and how to set options.

## Author

Michael Neely
