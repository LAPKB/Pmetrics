# Convert Absolute Dates and Times to Relative Hours

**\[superseded\]**

Convert dates/times to relative times. This function is largely
superseded as it is called automatically when data are initialized as a
[PM_data](https://lapkb.github.io/Pmetrics_rust/reference/PM_data.md)
object. There is rarely a need to call it directly any longer.

## Usage

``` r
PMmatrixRelTime(
  data,
  idCol = "id",
  dateCol = "date",
  timeCol = "time",
  evidCol = "evid",
  format,
  split = F
)
```

## Arguments

- data:

  The name of an R data object.

- idCol:

  A character vector with the name of the id column in `data` or the
  number of the id column, default is “id”

- dateCol:

  A character vector with the name of the date column in `data` or the
  number of the date column, default is “date”

- timeCol:

  A character vector with the name of the time column in `data` or the
  number of the time column, default is “time”

- evidCol:

  A character vector with the name of the event id column in `data` or
  the number of the evid column, default is “evid”

- format:

  Format of the date and time columns; default is m/d/y and h:m:s, as
  specified in the chron::chron function. Note the separators in each
  case (/ for dates and : for times). For dates, *m* is months in digits
  and can be one or two digits; *d* is the day of the month, again as
  one or two digits; *y* is the year in 2 or 4 digits. For times, all
  values can be one or two digits, but time is in 24-hour format, and
  *s* is required to avoid ambiguity.

- split:

  If *true*, `PMmatrixRelTime` will split every `id` into id.block,
  where block is defined by a dose reset, or evid=4, e.g. `id` 1.1, 1.2,
  1.3, 2.1, 3.1, 3.2.

## Value

Returns a dataframe with columns *id, evid, relTime*. If `split`=T all
evid values that were previously 4 will be converted to 1.

## Details

`PMmatrixRelTime` will convert absolute dates and times in a dataset
into relative hours, suitable for Pmetrics analysis. Additionally, the
user has the option to split subjects into pseudosubjects every time a
dose reset (evid=4) is encountered.

## See also

[`PMreadMatrix`](https://lapkb.github.io/Pmetrics_rust/reference/PMreadMatrix.md)

## Author

Michael Neely
