# Data objects

## 1 Introduction

[Jump to examples](#examples)

**Pmetrics always needs data and a model to run.** Pmetrics data objects
are typically read into memory from files. Although the file format is
usually comma-separated (.csv), it is possible to use other separators,
like the semicolon, by setting the appropriate argument with
[`setPMoptions`](https://lapkb.github.io/Pmetrics/reference/setPMoptions.html).

``` r
setPMoptions()
# Choose "Data File Reading"
# Changes field separator to ";" from default ","
# and decimal mark from "." to ","
```

Examples of programs that can save .csv files are any text editor
(e.g. TextEdit on Mac, Notepad on Windows) or spreadsheet program
(e.g. Excel).

It is possible to create a data object in R directly, without reading a
file. This is useful for simulation purposes, where you may want to
create a small dataset on the fly. We’ll cover this below.

## 2 R6 objects

Most Pmetrics objects, including data, follow the
[R6](https://r6.r-lib.org/reference/R6Class.html) framework. The idea of
this object is to represent a dataset that is going to be
modeled/simulated. All its behaviour is represented by the class
[`PM_data`](https://lapkb.github.io/Pmetrics/reference/PM_data.html).
This class allows datasets to be checked, plotted, written to disk and
more. Use `PM_data$new("filename")` to create a `PM_data` object by
reading the file.

``` r
# assume that data.csv is in the working directory
data1 <- PM_data$new("data.csv")
```

You can also build an appropriate data frame in R and provide that as an
argument to `PM_data$new()`.

``` r
# assume df is data frame with at least these columns:
# id, time, dose, out
data1 <- PM_data$new(df)
```

Lastly, you can take advantage of the `addEvent` method in `PM_data`
objects to build a data object on the fly. This can be particularly
useful for making quick simulation templates. Start with an empty call
to `PM_data$new()` and add successive rows. See
[`PM_data`](https://lapkb.github.io/Pmetrics/reference/PM_data.html) for
details under the `addEvent` method.

``` r
dat <- PM_data$new()$
    addEvent(id = 1, time = 0, dose = 100, addl = 5, ii = 24)$
    addEvent(id = 1, time = 144, out = -1)$
    addEvent(id = 1, wt = 75, validate = TRUE)
```

*Notes on the above statements:*

1.  Lack of time element in the last `addEvent` will add *wt=75* to all
    rows for id = 1
2.  Use `validate = TRUE` as an argument in the last `addEvent` to
    finalize creation
3.  You can chain `addEvent` as shown above by including the `$` between
    events.

*Advanced concept*: For those familiar with
[tidyverse](https://tidyverse.org) or the native R pipe to join
functions (“%\>%” or “\|\>”, respectively), chaining in R6 is similar
but restricted to methods defined for the object. In this case we chain
the `addEvent` methods. We could even chain an additional `PM_data`
method like `$plot()` at the end of the above code. However, that would
create `dat` as a plotly plot object, not a `PM_data` one.

Below you see the data standardization and validation reports that are
generated when you create a new `PM_data` object, and the output of
typing `dat$data` and `dat$standard_data` look like in the viewer. The
former is your original data, and the latter is what it looks like after
standardization to the full Pmetrics format.

    #>
    #> ── DATA STANDARDIZATION ────────────────────────────────────────────────────────
    #> EVID inferred as 0 for observations, 1 for doses.
    #>  All doses assumed to be oral (DUR = 0).
    #>  ADDL set to missing for all records.
    #>  II set to missing for all records.
    #>  All doses assumed to be INPUT = 1.
    #>  All observations assumed to be OUTEQ = 1.
    #>  All observations assumed to be uncensored.
    #>  One or more error coefficients not specified. Error in model object will be used.
    #>
    #> ── DATA VALIDATION ─────────────────────────────────────────────────────────────
    #> No data errors found.

|  id | time | dose | out |  wt |
|----:|-----:|-----:|----:|----:|
|   1 |    0 |  100 |  NA |  75 |
|   1 |   24 |  100 |  NA |  75 |
|   1 |   48 |  100 |  NA |  75 |
|   1 |   72 |  100 |  NA |  75 |
|   1 |   96 |  100 |  NA |  75 |
|   1 |  120 |  100 |  NA |  75 |
|   1 |  144 |   NA |  -1 |  75 |

|  id | evid | time | dur | dose | addl | ii  | input | out | outeq | cens | c0  | c1  | c2  | c3  |  wt |
|----:|-----:|-----:|----:|-----:|:-----|:----|------:|----:|------:|:-----|:----|:----|:----|:----|----:|
|   1 |    1 |    0 |   0 |  100 | NA   | NA  |     1 |  NA |    NA | NA   | NA  | NA  | NA  | NA  |  75 |
|   1 |    1 |   24 |   0 |  100 | NA   | NA  |     1 |  NA |    NA | NA   | NA  | NA  | NA  | NA  |  75 |
|   1 |    1 |   48 |   0 |  100 | NA   | NA  |     1 |  NA |    NA | NA   | NA  | NA  | NA  | NA  |  75 |
|   1 |    1 |   72 |   0 |  100 | NA   | NA  |     1 |  NA |    NA | NA   | NA  | NA  | NA  | NA  |  75 |
|   1 |    1 |   96 |   0 |  100 | NA   | NA  |     1 |  NA |    NA | NA   | NA  | NA  | NA  | NA  |  75 |
|   1 |    1 |  120 |   0 |  100 | NA   | NA  |     1 |  NA |    NA | NA   | NA  | NA  | NA  | NA  |  75 |
|   1 |    0 |  144 |  NA |   NA | NA   | NA  |    NA |  -1 |     1 | none | NA  | NA  | NA  | NA  |  75 |

Once you have created the `PM_data` object, you never need to create it
again during your R session. You also don’t have to bother copying the
data file to the Runs folder each time you run the model, like you used
to do with older **(“Legacy”)** versions of Pmetrics. The data are
stored in memory and can be used in any Pmetrics function that needs it.

## 3 Data format

R6 Pmetrics can use file or data frame input. The format is very
flexible. The only required columns are those below. Unlike Legacy
Pmetrics, there are no requirements for a header or to prefix the ID
column with “#”. However, any subsequent row that begins with “#” will
be ignored, which is helpful if you want to exclude data from the
analysis, but preserve the integrity of the original dataset, or to add
comment lines. The column order can be anything you wish, but the names
should be the same as below. Ultimately, `PM_data$new()` converts all
valid data into a standardized format, which is the same as used in
Legacy Pmetrics.

- ***ID*** This field can be numeric or character and identifies each
  individual. All rows must contain an ID, and all records from one
  individual must be contiguous. IDs may be any alphanumeric
  combination. The number of subjects is unlimited.

- ***TIME*** This is the elapsed time in decimal hours since the first
  event, which is always `TIME = 0`, unless you specify `TIME` as clock
  time. In that case, you must nclude a `DATE` column, described below.
  For clock time, the default format is HH:MM. Other formats can be
  specified. See
  [`PM_data`](https://lapkb.github.io/Pmetrics/reference/PM_data.html)
  for more details. Every row must have an entry, and within a given ID,
  rows must be sorted chronologically, earliest to latest.

- ***DATE*** This column is only required if `TIME` is clock time,
  detected by the presence of “:”. The default format of the date column
  is YYYY-MM-DD. As for `TIME`, other formats can be specified. See
  [`PM_data`](https://lapkb.github.io/Pmetrics/reference/PM_data.html)
  for more details.

- ***DOSE*** This is the dose amount. It should be “.” for observation
  rows. All sujects must have a dose event at time 0, which is the first
  row for that subject. The dose amount can be any numeric value,
  including 0. If the dose is an infusion, the `DUR` column must also be
  included. In other software packages, `DOSE` is equivalent to `AMT`.

- ***OUT*** This is the observation, or output value, and it is always
  required. If `EVID = 0`, there must be an entry. For such events, if
  the observation is missing, e.g. a sample was lost or not obtained,
  this must be coded as -99. It will be ignored for any other `EVID` and
  therefore should be “.”. `OUT` can be coded as `DV` in other software
  packages. When `OUT = -99`, this is equivalent to `MDV = 1`, or
  missing dependent variable in other packages, but Pmetrics does not
  use `MDV`.

- ***COVARIATES…*** Any column named other than above is assumed to be a
  covariate, one column per covariate. The first row for any subject
  must have a value for all covariates, since the first row is always a
  dose. **Covariates are handled differently than in Legacy Pmetrics.**
  In Legacy, they were only considered at the times of dose events
  (`EVID = 1` or `EVID = 4`). In *Pmetrics 3.0* and later, they are
  considered at all times, including observation events (`EVID = 0`).
  Therefore, to enter a new covariate value at a time other than a dose
  or an observation, create a row at the appropriate time (and possibly
  date if using clock/calendar), making the row either a dose row with
  `DOSE = 0` or an observation row with `OUT = -99` (missing). By
  default, covariate values are linearly interpolated between entries.
  This is useful for covariates like weight, which may vary from
  measurement to measurement. You can change this behavior in the model
  definition to make them piece-wise constant, i.e. carried forward from
  the previous value until a new value causes an instant change. This
  could be used, for example, to indicate periods of off and on
  dialysis. See the chapter on Models for more details.

When `PM_data` reads a file, it will standardize it to the format below.
This means some inferences are made. For example, in the absence of
`EVID`, all doses are interpreted as oral. If they are infusions, `DUR`
must be included to indicate the duration of the infusion. `EVID` only
needs to be included if `EVID=4` (reset event) is required, described
below. Similarly, `INPUT` and `OUTEQ` are only required if multiple
inputs or outputs are being modeled. Lastly, `ADDL` and `II` are
optional.

Lastly, the standardized data are checked for errors and if found,
Pmetrics generates a report with the errors and will attempt to fix
those that it can.

### 3.1 Standardized Data

The order, capitalization and names of the header and the first 13
columns are fixed. All entries must be numeric, with the exception of
`ID` and “.” for non-required placeholder entries. Any subsequent row
that begins with “#” will be ignored, as above.

A full example data file is below, with details following.

| ID   | EVID |  TIME | DUR | DOSE | ADDL |  II | INPUT | OUT  | OUTEQ | CENS |   C0 |  C1 |    C2 |    C3 |  COV |
|:-----|-----:|------:|----:|-----:|-----:|----:|------:|:-----|------:|:-----|-----:|----:|------:|------:|-----:|
| GH   |    1 |  0.00 | 0.0 |  400 |   NA |  NA |     1 | NA   |    NA | NA   |   NA |  NA |    NA |    NA | 10.0 |
| GH   |    0 |  0.50 |  NA |   NA |   NA |  NA |    NA | 0.42 |     1 | none |   NA |  NA |    NA |    NA |   NA |
| GH   |    0 |  1.00 |  NA |   NA |   NA |  NA |    NA | 0.46 |     1 | none |   NA |  NA |    NA |    NA |   NA |
| GH   |    0 |  2.00 |  NA |   NA |   NA |  NA |    NA | 2.47 |     1 | none |   NA |  NA |    NA |    NA |   NA |
| GH   |    4 |  0.00 | 0.0 |  150 |   NA |  NA |     1 | NA   |    NA | NA   |   NA |  NA |    NA |    NA |   NA |
| GH   |    1 |  3.50 | 0.5 |  150 |   NA |  NA |     1 | NA   |    NA | NA   |   NA |  NA |    NA |    NA |   NA |
| GH   |    0 |  5.12 |  NA |   NA |   NA |  NA |    NA | 0.55 |     1 | none |   NA |  NA |    NA |    NA |   NA |
| GH   |    0 | 24.00 |  NA |   NA |   NA |  NA |    NA | 0.52 |     1 | none |   NA |  NA |    NA |    NA |   NA |
| 1423 |    1 |  0.00 | 1.0 |  400 |   -1 |  12 |     1 | NA   |    NA | NA   |   NA |  NA |    NA |    NA | 34.5 |
| 1423 |    1 |  0.10 | 0.0 |  100 |   NA |  NA |     2 | NA   |    NA | NA   |   NA |  NA |    NA |    NA |   NA |
| 1423 |    0 |  1.00 |  NA |   NA |   NA |  NA |    NA | -99  |     1 | none | 0.01 | 0.1 |  0.00 | 0.000 |   NA |
| 1423 |    0 |  2.00 |  NA |   NA |   NA |  NA |    NA | 0.38 |     1 | none | 0.01 | 0.1 |  0.00 | 0.000 |   NA |
| 1423 |    0 |  2.00 |  NA |   NA |   NA |  NA |    NA | 1.6  |     2 | none | 0.05 | 0.2 | -0.11 | 0.002 |   NA |

- ***ID*** See [above](#data-id).

- ***EVID*** This is the event ID field. It can be 0, 1, or 4. It is
  only required if `EVID = 4` is included in the data, in which case
  every row must have an entry. If there are no `EVID = 4` events, the
  entire `EVID` column can be omitted from the data.

  - 0 = observation

  - 1 = input (e.g. dose)

  - 2, 3 are currently unused

  - 4 = reset, where all compartment values are set to 0 and the time
    counter is reset to 0. This is useful when an individual has
    multiple sampling episodes that are widely spaced in time with no
    new information gathered. This is a dose event, so dose information
    needs to be complete. The `TIME` value for `EVID = 4` should be 0,
    and subsequent rows should increase monotonically from 0 until the
    last record or until another `EVID = 4` event, which will restart
    time at 0.

- ***TIME*** See [above](#data-time).

- ***DATE*** See [above](#data-date).

- ***DUR*** This is the duration of an infusion in hours. If `EVID = 0`
  (observation event), `DUR` is ignored and should have a “.”
  placeholder. For a bolus (e.g. an oral dose), set the value equal
  to 0. As mentioned above, if all doses are oral, `DUR` can be omitted
  from the data altogether. Some other packages use `RATE` instead of
  `DUR`, but of course, one can convert rate to duration with
  `DUR = DOSE / RATE`.

- ***DOSE*** See [above](#data-dose).

- ***ADDL*** This specifies the number of additional doses to give at
  interval `II`. `ADDL` can be positive or negative. If positive, it is
  the number of doses to give after the dose at time 0. If negative, it
  is the number of doses to give before the dose at time 0. It may be
  missing (“.”) for dose events (`EVID = 1` or `EVID = 4`), in which
  case it is assumed to be 0. It is ignored for observation (`EVID = 0`)
  events. Be sure to adjust the time entry for the subsequent row, if
  necessary, to account for the extra doses. All compartments in the
  model will contain the predicted amounts of drug at the end of the
  `II` interval after the last `ADDL` dose.

- ***II*** This is the interdose interval and is only relevant if `ADDL`
  is not equal to 0, in which case `II` cannot be missing. If `ADDL = 0`
  or is missing, `II` is ignored.

- ***INPUT*** This defines which input (i.e. drug) the `DOSE`
  corresponds to. The model defines which compartments receive the
  input(s). If only modeling one drug, `INPUT` is unnecessary, as all
  values will be assumed to be 1. Other packages may use `CMT` for
  compartment for both inputs and outputs. It is necessary to separate
  these in Pmetrics and for outputs, designate the corresponding model
  input number with `INPUT` (e.g. RATEIV\[x\] or B\[x\] for infusions
  and boluses in the model object), not the compartment.

- ***OUT*** See [above](#data-out).

- ***OUTEQ*** This is the output equation number that corresponds to the
  `OUT` value. Output equations are defined in the model file. If only
  modeling one output, this column is unnecessary, as all values are
  assumed to be 1. As discussed in `INPUT`, other packages may use `CMT`
  for compartment for both inputs and outputs. It is necessary to
  separate these in Pmetrics and for outputs, designate the
  corresponding model output equation number with `OUTEQ`, not the
  compartment.

- ***CENS*** This is a new column as of Pmetrics 3.0.0. It indicates
  whether the observation is censored, i.e. below a lower limit of
  quantification or above an upper limit . It can take on four values:

  - Missing for dose events which are not observations. Use a “.” as a
    placeholder in your data file.

  - 0 = not censored

  - 1 = left censored (below lower limit of quantification)

  - 2 = right censored (above upper limit of quantification)

  If there are no censored observations, the entire `CENS` column can be
  omitted from the data. In data fitting, left censored observations are
  handled using the M3 method described by Beal ([Beal
  2001](#ref-bealWaysFitPK2001a)). Right censored observations are
  handled similarly, but using the complementary probability. The value
  in the `OUT` column is the censoring lower limit of quantification
  (LLOQ) for left censored observations. It is the upper limit of
  quantification (ULOQ) for right censored observations. For uncensored
  observations, `OUT` is the observed value as usual.

- ***C0, C1, C2, C3*** These are the coefficients for the assay error
  polynomial for that observation. Each subject may have up to one set
  of coefficients per output equation. If more than one set is detected
  for a given subject and output equation, the last set will be used. If
  there are no available coefficients, these cells may be omitted. If
  they are included, for events which are not observations, they can be
  filled with “.” as a placeholder. In data fitting, if the coefficients
  are present in the data file, Pmetrics will use them. If missing,
  Pmetrics will look for coefficients defined in the model.

- ***COVARIATES***… See [above](#data-cov).

## 4 Manipulation of CSV files

#### 4.0.1 Read

As we have seen, `PM_data$new("filename")` will read an appropriate data
file in the current working directory to create a new `PM_data` object.

#### 4.0.2 Save

`PM_data$save("filename")` will save the `PM_data` object to a file
called “filename”. This can be useful if you have loaded or created a
data file and then changed it in R.

#### 4.0.3 Standardize

`PM_data$new()` automatically standardizes the data into the full
format. This includes conversion of calendar date / clock time into
decimal elapsed time.

#### 4.0.4 Validate

`PM_data$new()` automatically calls
[`PMcheck`](https://lapkb.github.io/Pmetrics/reference/PMcheck.html) so
the data are validated as the data object is created.

#### 4.0.5 Data conversion

- [`PMwrk2csv()`](https://lapkb.github.io/Pmetrics_rust/reference/PMwrk2csv.md)
  This function will convert old-style, single-drug USC\*PACK .wrk
  formatted files into Pmetrics data .csv files.

- [`NM2PM()`](https://lapkb.github.io/Pmetrics_rust/reference/NM2PM.md)
  Although the structure of Pmetrics data files is similar to NONMEM,
  there are some differences. This function attempts to automatically
  convert to Pmetrics format. It has been tested on several examples,
  but there are probably NONMEM files which will cause it to crash.

## 5 Examples

Let’s start by setting the working directory to where the data files are
located. This is usually the “src” folder, but it can be anywhere you
like. The working directory we want to move to can be specified as an
absolute path or as a relative path.

``` r
# absolute
setwd("full path to 'src' folder here") 
# relative, if `src` is a subfolder of the current working directory
setwd("src")

# List the files inside the current working directory 
list.files()
```

**Pmetrics** comes with an example dataset called `dataEx` already
loaded. You can practice with it.

``` r
# Save it to your current working directory
dataEx$save("ex.csv")

# Save it to elsewhere
dataEx$save("/mypath/ex.csv")

# Load it again from current working directory or from elsewhere
exData <- PM_data$new("ex.csv")
exData <- PM_data$new("/mypath/ex.csv")
```

You can look at the `ex.csv` file directly by opening from your hard
drive it in a spreadsheet program like Excel, or a text editor.

`exData` is an R6 object, which means that contains both data and
methods to process that data.

``` r

# See the contents of the object
names(exData)
#>  [1] "nca"             ".__enclos_env__" "summary"         "auc"            
#>  [5] "addEvent"        "post"            "clone"           "initialize"     
#>  [9] "standard_data"   "save"            "print"           "plot"           
#> [13] "data"            "pop"
```

The first element is an artifact of the R6 class. The remaining elements
are documented in the help for
[`PM_data`](https://lapkb.github.io/Pmetrics/reference/PM_data.html).
You can of course inspect the data directly.

``` r

# Your original data (first few rows)
head(exData$data)
#>   id time dose   out   wt africa age gender height
#> 1  1    0  600    NA 46.7      1  21      1    160
#> 2  1   24  600    NA 46.7      1  21      1    160
#> 3  1   48  600    NA 46.7      1  21      1    160
#> 4  1   72  600    NA 46.7      1  21      1    160
#> 5  1   96  600    NA 46.7      1  21      1    160
#> 6  1  120   NA 10.44 46.7      1  21      1    160
```

``` r
# See the standardized data nicely formatted in the viewer (truncated for brevity)
exData
```

|  id | evid | time | dur | dose | addl | ii  | input |   out | outeq | cens | c0  | c1  | c2  | c3  |   wt | africa | age | gender | height |
|----:|-----:|-----:|----:|-----:|:-----|:----|------:|------:|------:|:-----|:----|:----|:----|:----|-----:|-------:|----:|-------:|-------:|
|   1 |    1 |    0 |   0 |  600 | NA   | NA  |     1 |    NA |    NA | NA   | NA  | NA  | NA  | NA  | 46.7 |      1 |  21 |      1 |    160 |
|   1 |    1 |   24 |   0 |  600 | NA   | NA  |     1 |    NA |    NA | NA   | NA  | NA  | NA  | NA  | 46.7 |      1 |  21 |      1 |    160 |
|   1 |    1 |   48 |   0 |  600 | NA   | NA  |     1 |    NA |    NA | NA   | NA  | NA  | NA  | NA  | 46.7 |      1 |  21 |      1 |    160 |
|   1 |    1 |   72 |   0 |  600 | NA   | NA  |     1 |    NA |    NA | NA   | NA  | NA  | NA  | NA  | 46.7 |      1 |  21 |      1 |    160 |
|   1 |    1 |   96 |   0 |  600 | NA   | NA  |     1 |    NA |    NA | NA   | NA  | NA  | NA  | NA  | 46.7 |      1 |  21 |      1 |    160 |
|   1 |    0 |  120 |  NA |   NA | NA   | NA  |    NA | 10.44 |     1 | none | NA  | NA  | NA  | NA  | 46.7 |      1 |  21 |      1 |    160 |

A method exists for all Pmetrics objects to summarize them.

``` r
exData$summary() 
#> 
#> ── Data Summary ────────────────────────────────────────────────────────────────
#> Number of subjects: 20
#> Number of inputs: 1
#> Number of outputs: 1
#> Covariates: wt, africa, age, gender, and height
#> 
#> ── Inputs: Mean (SD), Min to Max ──
#> 
#> Number of doses per subject: 6.000 (0.000), 6.000 to 6.000
#> Dose amount per subject: 585.000 (45.189), 450.000 to 600.000
#> 
#> ── Outputs: Mean (SD), Min to Max ──
#> 
#> Total across all subjects: 139, with 0 (0.000%) missing.
#> Number per subject: 6.950 (0.224), 6 to 7
#> Value per subject: 7.241 (3.799), 1.860 to 20.150
#> 
#> ── Population level covariates: Mean (SD), Min to Max ──
#> 
#> wt: 54.538 (7.173), 43.000 to 66.500
#> africa: 1.000 (0.000), 1.000 to 1.000
#> age: 27.035 (7.717), 19.000 to 54.000
#> gender: 0.749 (0.434), 0.000 to 1.000
#> height: 167.792 (7.562), 150.000 to 181.000
#> 
#> Note: See `?summary.PM_data()` for more summary options using `formula`.
```

The older S3 method to summarize will also work, but is less preferred:
`summary(exData)`.

## 6 Citations

Beal, S. L. 2001. “[Ways to Fit a PK Model with Some Data Below the
Quantification Limit.](https://www.ncbi.nlm.nih.gov/pubmed/11768292)”
*Journal of Pharmacokinetics and Pharmacodynamics* 28 (5): 481–504.
