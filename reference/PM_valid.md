# Pmetrics validation object

**\[stable\]**

Contains results of internal validation by simulation to permit
generation of visual predictive checks (VPCs), prediction corrected
visual predictive checks, (pcVPCs), normalized prediction distribution
errors (NPDE), and numerical predictive checks. This is typically a
field in a
[PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)

## Details

The PM_valid object is both a data field within a
[PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md),
and itself an R6 object comprising data fields and associated methods
suitable for analysis and plotting of observed vs. population or
individual predicted outputs.

Because PM_valid objects are automatically added to the
[PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
by calling the `$validate()` method of a
[PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
after a successful run, it is generally not necessary for users to
generate PM_valid objects themselves.

## See also

[PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)

## Author

Michael Neely

## Public fields

- `simdata`:

  Simulated data created in the validation process

- `timeBinMedian`:

  Median times for cluster bins

- `tadBinMedian`:

  Median times after previous doses for cluster bins

- `opDF`:

  Observed-predicted data frame

- `npde`:

  Data for Normalized Prediction Distribution Error

- `npde_tad`:

  Data for Normalized Prediction Distribution Error using Time After
  Dose if available

## Methods

### Public methods

- [`PM_valid$new()`](#method-PM_valid-new)

- [`PM_valid$plot()`](#method-PM_valid-plot)

- [`PM_valid$clone()`](#method-PM_valid-clone)

------------------------------------------------------------------------

### Method `new()`

**\[stable\]**

This function will create an object suitable for plotting visual
predictive checks (VPCs) and prediction-corrected visual predictive
checks (pcVPCs).

#### Usage

    PM_valid$new(result, tad = FALSE, binCov, doseC, timeC, tadC, limits, ...)

#### Arguments

- `result`:

  The result of a prior run, usually supplied by calling the
  `$validate()` method of a
  [PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
  at the end of a run, or later loaded with
  [PM_load](https://lapkb.github.io/Pmetrics_rust/reference/PM_load.md).

- `tad`:

  Boolean operator to use time after dose rather than time after start.
  Default is `FALSE`.

- `binCov`:

  A character vector of the names of covariates which are included in
  the model, i.e. in the model equations and which need to be binned.
  For example `binCov='wt'` if "wt" is included in a model equation like
  V=V0\*wt, or `binCov=c( 'wt', 'crcl')` if both "wt" and "crcl" are
  included in model equations.

- `doseC`:

  An integer with the number of dose/covariate bins to cluster, if known
  from a previous run of this function. Including this value will skip
  the clustering portion for doses/covariates.

- `timeC`:

  An integer with the number of observation time bins to cluster, if
  known from a previous run of this function. Including this value will
  skip the clustering portion for observation times.

- `tadC`:

  An integer with the number of time after dose bins to cluster, if
  known from a previous run of this function. Including this value will
  skip the clustering portion for time after dose. This argument will be
  ignored if `tad=FALSE`.

- `limits`:

  Limits on simulated parameters. See
  [PM_sim](https://lapkb.github.io/Pmetrics_rust/reference/PM_sim.md).

- `...`:

  Other parameters to be passed to
  [PM_sim](https://lapkb.github.io/Pmetrics_rust/reference/PM_sim.md),
  especially `limits`.

#### Details

The function will guide the user through appropriate clustering of
doses, covariates and sample times for prediction correction using the
methods of Bergstrand et al (2011). *NOTE:* Including `tad` is only
valid if steady state conditions exist for each patient. This means that
dosing is stable and regular for each patient, without changes in amount
or timing, and that sampling occurs after the average concentrations are
the same from dose to dose. Otherwise observations are *NOT*
superimposable and `tad` should *NOT* be used, i.e. should be set to
`FALSE`.

#### Returns

An R6 object of class `PM_valid`, which is a list with the following.

- simdata The combined, simulated files for all subjects using the
  population mean values and each subject as a template. This object
  will be automatically saved to the run, to be loaded with
  [PM_load](https://lapkb.github.io/Pmetrics_rust/reference/PM_load.md)
  next time.

- timeBinMedian A data frame with the median times for each cluster bin.

- tadBinMedian A data frame with the median time after dose (tad) for
  each cluster bin. This will be `NA` if `tad = FALSE`.

- opDF A data frame with observations, predicitons, and bin-corrected
  predictions for each subject.

- ndpe An object with results of normalized distrubition of prediction
  errors analysis.

- npde_tad NPDE with time after dose rather than absolute time, if
  `tad = TRUE`

#### Examples

    \dontrun{
    valid <- NPex$validate(limits = c(0, 3))
    }

------------------------------------------------------------------------

### Method [`plot()`](https://rdrr.io/r/graphics/plot.default.html)

Plot method. Calls
[plot.PM_valid](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_valid.md).

#### Usage

    PM_valid$plot(...)

#### Arguments

- `...`:

  Arguments to pass to \[plot.PM_valid\].

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PM_valid$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
## ------------------------------------------------
## Method `PM_valid$new`
## ------------------------------------------------

if (FALSE) { # \dontrun{
valid <- NPex$validate(limits = c(0, 3))
} # }
```
