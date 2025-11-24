# Make a Percent Target Attainment (PTA) Target

**\[stable\]**

Generates an object of class *PMpta.targ* which can be used in the
`$new()` method for
[PM_pta](https://lapkb.github.io/Pmetrics_rust/reference/PM_pta.md) or
`$pta()` method for
[PM_sim](https://lapkb.github.io/Pmetrics_rust/reference/PM_sim.md) for
targets sampled from a distribution.

## Usage

``` r
makePTAtarget(x)
```

## Arguments

- x:

  A data.frame or name of .csv file in working directory whose first two
  columns are targets and the number of samples for each target. An
  example can be seen for Staphylococcus aureus susceptibility to
  vancomycin at
  [EUCAST](http://mic.eucast.org/Eucast2/regShow.jsp?Id=1214).

## Value

A data frame with two columns named targets and n, of class
*PMpta.targ*.

## See also

[PM_pta](https://lapkb.github.io/Pmetrics_rust/reference/PM_pta.md)

## Examples

``` r
if (FALSE) { # \dontrun{
makePTAtarget(mic1)
} # }
```
