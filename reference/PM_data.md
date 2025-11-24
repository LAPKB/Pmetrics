# Defines the PM_data object

**\[stable\]**

PM_data R6 objects containing raw, standardized and valid data, and
methods to process the data

## Details

*PM_data* objects are passed to
[PM_fit](https://lapkb.github.io/Pmetrics_rust/reference/PM_fit.md)
objects to initiate a population analysis. The object is created by
reading a delimited file in the current working directory. The data will
be transformed into the standard format which is the same for all
engines, with a report of any assumptions that were necessary to
standardize the data.
[PMcheck](https://lapkb.github.io/Pmetrics_rust/reference/PMcheck.md) is
called on the standard data to evaluate for errors. If dates and times
are converted to relative decimal times in the standard data, automatic
detection of the correct format will be attempted using
[`lubridate::parse_date_time()`](https://lubridate.tidyverse.org/reference/parse_date_time.html).
In the case of failure due to an unusual format, use the 'dt' argument
to specify the correct format in your data. In the case of successful
automatic detection, the format used will be included in the
standardization report generated upon creation of a new *PM_data*
object. Check carefully to make sure the correct format was chosen. Note
that if your clock times did not include seconds, they were appended as
":00" to the end of each time and will appear that way in the copy of
the original data.

There are a number of methods defined for a PM_data object, including to
write the standard data back to a file for future use, to summarize and
to plot the object, to conduct a non-compartmental analysis on the raw
data using
[makeNCA](https://lapkb.github.io/Pmetrics_rust/reference/makeNCA.md),
to calculate an AUC using
[makeAUC](https://lapkb.github.io/Pmetrics_rust/reference/makeAUC.md),
and to add event rows, which is particularly useful for making
simulation templates on the fly.

## Public fields

- `data`:

  Data frame containing the data to be modeled

- `standard_data`:

  Data frame containing standardized version of the data

- `pop`:

  The `$data` field from a
  [PM_pop](https://lapkb.github.io/Pmetrics_rust/reference/PM_pop.md)
  object. This makes it easy to add population predictions to a raw data
  plot. This field will be `NULL` until the PM_data object is added to
  the
  [PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
  after a run. As examples:

  - `dat <- PM_data$new("data.csv")`. Here, `dat$pop` will be `NULL`.

  - `run1 <- PM_load(1)`. Here, `run1$data$pop` will be the same as
    `run1$pop$data`.

- `post`:

  The `$data` field from a
  [PM_post](https://lapkb.github.io/Pmetrics_rust/reference/PM_post.md)
  object. See details in the `pop` argument above.

## Methods

### Public methods

- [`PM_data$new()`](#method-PM_data-new)

- [`PM_data$save()`](#method-PM_data-save)

- [`PM_data$auc()`](#method-PM_data-auc)

- [`PM_data$nca()`](#method-PM_data-nca)

- [`PM_data$plot()`](#method-PM_data-plot)

- [`PM_data$print()`](#method-PM_data-print)

- [`PM_data$summary()`](#method-PM_data-summary)

- [`PM_data$addEvent()`](#method-PM_data-addEvent)

- [`PM_data$clone()`](#method-PM_data-clone)

------------------------------------------------------------------------

### Method `new()`

Create new data object

#### Usage

    PM_data$new(data = NULL, dt = NULL, quiet = FALSE, validate = TRUE, ...)

#### Arguments

- `data`:

  A quoted name of a file with full path if not in the working
  directory, an unquoted name of a data frame in the current R
  environment, or a PM_data object, which will rebuild it.

- `dt`:

  Pmetrics will try a variety of date/time formats. If all 16 of them
  fail, use this parameter to specify the correct format as a character
  vector whose first element is date format and second is time. Use the
  following abbreviations:

  - Y = 4 digit year

  - y = 2 digit year

  - m = decimal month (1, 2, ..., 12)

  - d = decimal day (1, 2, ..., 31)

  - H = hours (0-23)

  - M = minutes (0-59) Example: `format = c("myd", "mh")`. Not one of
    the tried combinations! Always check to make sure that dates/times
    were parsed correctly and the relative times in the
    `PM_data$standard_data` field look correct. Other date/time formats
    are possible. See
    [`lubridate::parse_date_time()`](https://lubridate.tidyverse.org/reference/parse_date_time.html)
    for these.

- `quiet`:

  Quietly validate. Default is `FALSE`.

- `validate`:

  Check for errors. Default is `TRUE`. Strongly recommended.

- `...`:

  Other arguments (not currently used).

#### Details

Creation of a new PM_data objects from a file or a data frame. Data will
be standardized and checked automatically to a fully specified, valid
data object.

------------------------------------------------------------------------

### Method [`save()`](https://rdrr.io/r/base/save.html)

Save data to file

#### Usage

    PM_data$save(file_name, ...)

#### Arguments

- `file_name`:

  A quoted name of the file to create with full path if not in the
  working directory.

- `...`:

  Arguments passed to
  [PMwriteMatrix](https://lapkb.github.io/Pmetrics_rust/reference/PMwriteMatrix.md)

#### Details

Saves a delimited file (e.g. comma-separated) from the `standard_data`
field

------------------------------------------------------------------------

### Method `auc()`

Calculate AUC

#### Usage

    PM_data$auc(...)

#### Arguments

- `...`:

  Arguments passed to
  [makeAUC](https://lapkb.github.io/Pmetrics_rust/reference/makeAUC.md).

#### Details

See
[makeAUC](https://lapkb.github.io/Pmetrics_rust/reference/makeAUC.md).

------------------------------------------------------------------------

### Method `nca()`

Perform non-compartmental analysis

#### Usage

    PM_data$nca(...)

#### Arguments

- `...`:

  Arguments passed to
  [makeNCA](https://lapkb.github.io/Pmetrics_rust/reference/makeNCA.md).

#### Details

See
[makeNCA](https://lapkb.github.io/Pmetrics_rust/reference/makeNCA.md).

------------------------------------------------------------------------

### Method [`plot()`](https://rdrr.io/r/graphics/plot.default.html)

Plot method

#### Usage

    PM_data$plot(...)

#### Arguments

- `...`:

  Arguments passed to
  [plot.PM_data](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_data.md)

#### Details

See
[plot.PM_data](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_data.md).

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print method

#### Usage

    PM_data$print(standard = F, viewer = T, ...)

#### Arguments

- `standard`:

  Display the standardized data if `TRUE`. Default is `FALSE`.

- `viewer`:

  Display the Viewer if `TRUE`. Default is `TRUE`.

- `...`:

  Other arguments to
  [print.data.frame](https://rdrr.io/r/base/print.dataframe.html). Only
  passed if `viewer = FALSE`.

#### Details

Displays the PM_data object in a variety of ways.

------------------------------------------------------------------------

### Method [`summary()`](https://rdrr.io/r/base/summary.html)

Summary method

#### Usage

    PM_data$summary(...)

#### Arguments

- `...`:

  Arguments passed to
  [summary.PM_data](https://lapkb.github.io/Pmetrics_rust/reference/summary.PM_data.md).

#### Details

See
[summary.PM_data](https://lapkb.github.io/Pmetrics_rust/reference/summary.PM_data.md).

------------------------------------------------------------------------

### Method `addEvent()`

Add events to PM_data object

#### Usage

    PM_data$addEvent(..., dt = NULL, quiet = FALSE, validate = FALSE)

#### Arguments

- `...`:

  Column names and values.

- `dt`:

  Pmetrics will try a variety of date/time formats. If all 16 of them
  fail, use this parameter to specify the correct format as a character
  vector whose first element is date format and second is time. Use the
  following abbreviations:

  - Y = 4 digit year

  - y = 2 digit year

  - m = decimal month (1, 2, ..., 12)

  - d = decimal day (1, 2, ..., 31)

  - H = hours (0-23)

  - M = minutes (0-59) Example: `format = c("myd", "mh")`. Not one of
    the tried combinations! Always check to make sure that dates/times
    were parsed correctly and the relative times in the
    `PM_data$standard_data` field look correct. Other date/time formats
    are possible. See
    [`lubridate::parse_date_time()`](https://lubridate.tidyverse.org/reference/parse_date_time.html)
    for these.

- `quiet`:

  Quietly validate. Default is `FALSE`.

- `validate`:

  Validate the new row or not. Default is `FALSE` as a new row added to
  a blank will result in a one-row data object, which is invalid. Also,
  only one event type (dose or observation) should be added at a time,
  so if the new object contains only doses while building, this would
  cause an error. You should set `validate = TRUE` for the final
  addition.

#### Details

Add lines to a PM_data object by supplying named columns and values.
`ID` is always required. `Time` is handled differently depending on the
sequence of `addEvent` calls (see **Chaining** below).

- It is required for the first call to `addEvent` and should be 0. For
  example: For example:
  `dat <- PM_data$new()$addEvent(id = 1, time = 0, dose = 100, addl = 5, ii = 24)`

- For subsequent calls to `addEvent` with specific times it should be
  included. For example:
  `dat <- PM_data$new()$addEvent(id = 1, time = 0, dose = 100, addl = 5, ii = 24)$addEvent(id = 1, time = 144, out = -1)`
  Here, because `out` wasn't in the original call *and* the next call
  contains a value for `time`, an `out` value of -1 will be added at
  time 144 and `out` will be set to `NA` for all the previous rows.

- In contrast, the behavior is different if you omit `time` when your
  data object already has rows. In this case the arguments in the call
  to `addEvent` (without a value for `time`) will add those arguments as
  columns in the prior data with the specified value or *replace* values
  in those columns if they already exist. Be sure this is what you want.
  For example, building on the prior example:
  `dat$addEvent(id = 1, dur = 0.5)`. Note that we can chain to the
  previously created `dat` object. Here, a duration of 0.5 hours will be
  added to every previous row in `dat` to create the new `dat` object,
  but no new row is added since there is no `time` associated with it.

Adding covariates is supported, but since valid subject records in
Pmetrics with covariates must contain non-missing values at time 0,
covariates should be included with the first call to `$addEvent()`.

As we have seen in the examples above, `ADDL` and `II` are supported.

**Chaining** Multiple `$addEvent()` calls can be chained with
`PM_data$new()` to create a blank data object and then add rows. This
can be particularly useful for creating simulation templates. See the
example.

#### Examples

    \dontrun{
    PM_data$new()$addEvent(id = 1, time = 0, dose = 100, addl = 4, ii = 12,
    out = NA, wt = 75)$addEvent(id = 1, time = 60, out = -1)
    }

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PM_data$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
## ------------------------------------------------
## Method `PM_data$addEvent`
## ------------------------------------------------

if (FALSE) { # \dontrun{
PM_data$new()$addEvent(id = 1, time = 0, dose = 100, addl = 4, ii = 12,
out = NA, wt = 75)$addEvent(id = 1, time = 60, out = -1)
} # }
```
