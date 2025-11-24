# Set Pmetrics User Options

**\[stable\]**

Set user options for Pmetrics

## Usage

``` r
setPMoptions(launch.app = TRUE)
```

## Arguments

- launch.app:

  Launch the app to set options. Default `TRUE`.

## Value

The user preferences file will be updated. This will persist from
session to session and if stored in the external location, through
Pmetrics versions.

## Details

When you call this function with the default `launch.app = TRUE`, it
will start a Shiny app to set options for the Pmetrics package. Also,
when the Pmetrics package is first loaded with
[`library(Pmetrics)`](https://lapkb.github.io/Pmetrics_rust/), this
function will be called with `launch.app = TRUE` to read saved options
from a *PMoptions.json* file stored in a folder outside of the Pmetrics
package, so that your options will persist when Pmetrics is updated.

## Author

Michael Neely
