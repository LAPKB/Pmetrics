# Check Pmetrics Inputs for Errors

**\[superseded\]**

This function is largely superseded as it is called automatically when
data are initialized as a
[PM_data](https://lapkb.github.io/Pmetrics_rust/reference/PM_data.md)
object. It can still be called independently of this route and will
check for data errors.

## Usage

``` r
PMcheck(data, fix = FALSE, quiet = FALSE)
```

## Arguments

- data:

  The name of a Pmetrics .csv matrix file in the current working
  directory, the full path to one not in the current working directory,
  or a data.frame containing the output of a previous
  [PMreadMatrix](https://lapkb.github.io/Pmetrics_rust/reference/PMreadMatrix.md)
  command.

- fix:

  Boolean operator; if `TRUE`, Pmetrics will attempt to fix errors in
  the data file. Default is `FALSE`.

- quiet:

  Boolean operator to suppress printed output. Default is false.

## Value

If `fix=TRUE`, then PMcheck returns

- The original data if no errors are found, or

- A PMmatrix data object which has been cleaned of errors as much as
  possible, displaying a report on the console.

If `fix=FALSE`, then PMcheck creates a file in the working directory
called "errors.xlsx". This file can be opened by Microsoft Excel or any
other program that is capable of reading .xlsx files. This file contains
highlighted areas that are erroneous, with clarifying comments. You can
correct the errors in the file and then re-save as a .csv file.

When `fix=FALSE`, the function also returns a list of objects of class
*PMerr*. Each object is itself a list whose first object (`$msg`) is a
character vector with "OK" plus a brief description if there is no
error, or the error. The second object (`$results`) is a vector of the
row numbers that contain that error.

- colorder The first 14 columns must be named id, evid, time, dur, dose,
  addl, ii, input, out, outeq, c0, c1, c2, and c3 in that order.

- maxcharCol All column names should be less than or equal to 11
  characters.

- maxcharID All id values should be less than or equal to 11 characters.

- missEVID Ensure that all rows have an EVID value.

- missTIME Ensure that all rows have a TIME value.

- doseDur Make sure all dose records are complete, i.e. contain a
  duration.

- doseDose Make sure all dose records are complete, i.e. contain a dose.

- doseInput Make sure all dose records are complete, i.e. contain an
  input number.

- obsOut Make sure all observation records are complete, i.e. contain an
  output.

- obsOuteq Make sure all observation records are complete, i.e. contain
  and outeq number.

- T0 Make sure each subject's first time=0.

- covT0 Make sure that there is an non-missing entry for each covariate
  at time=0 for each subject.

- timeOrder Ensure that all times within a subject ID are monotonically
  increasing.

- contigID Ensure that all subject IDs are contiguous.

- nonNum Ensure that all columns except ID are numeric.

- noObs Ensure that all subjects have at least one observation, which
  could be missing, i.e. -99.

- mal_NA Ensure that all NA values are ".", not ". ", " .", "..", or
  other malformations.

## Details

It will check the data for errors which would cause the analysis to
fail. Note that as of Pmetrics Version 2, this function is called
automatically when a new
[PM_data](https://lapkb.github.io/Pmetrics_rust/reference/PM_data.md)
object is created, and users generally no longer need to call the
function directly. In `PM_data$new()`, the data object is first
standardized to contain all required columns, since only "ID", "TIME",
"DOSE" and "OUT" are required at minimum, and then checked with PMcheck.

If calling PMcheck directly, either a filename or a Pmetrics data object
in memory are accepted as `data`. Because there is no standardization
with direct calls, in this case the format of the .csv matrix file is
fairly rigid. It must have the following features. Text is
case-sensitive.

- A header in row 1 with the appropriate version, currently "POPDATA
  DEC_11"

  - Column headers in row 2. These headers are: \#ID, EVID, TIME, DUR,
    DOSE, ADDL, II, INPUT, OUT, CENS, OUTEQ, C0, C1, C2, C3.

- No cell should be empty. It should either contain a value or "." as a
  placeholder.

  - Columns after C3 are interpreted as covariates.

  - All subject records must begin with TIME=0.

- All dose events (EVID=1) must have entries in ID, EVID, TIME, DUR,
  DOSE and INPUT. ADDL and II are optional, but if ADDL is not 0 or
  missing, then II is mandatory.

- All observation events (EVID=0) must have entries in ID, EVID, TIME,
  OUT, OUTEQ. If an observation is missing, use -99; otherwise use a "."
  as a placeholder in cells that are not required (e.g. INPUT for an
  observation event).

- If covariates are present in the data, there must be an entry for
  every covariate at time 0 for each subject.

- All covariates must be numeric.

- All times within a subject ID must be monotonically increasing.

- All subject IDs must be contiguous.

- All rows must have EVID and TIME values.

- All columns must be numeric except ID which may be alpha-numeric.

- All subjects must have at least one observation, which could be
  missing, i.e. -99.

- Cells which are not needed (e.g. dose on an observation event,
  EVID=0), should contain ".".

To use this function, see the example below.

After running PMcheck and looking at the errors in the *errors.xlsx*
file, you can fix the errors manually directly in the *errors.xlsx* file
and resave it as a .csv file. Alternatively, you could then try to fix
the problem(s) with `mdata2 <- PMcheck(mdata,fix=T)`. Note that we are
now returning a PMmatrix data object called mdata2 (hopefully cleaned of
errors) rather than the PMerr object returned when `fix=FALSE`. Pmetrics
handles each of the errors in the following ways.

- If the columns are simply out of order, they will be reordered. If
  some are missing, the fix must be done by the user, i.e. manually.

- All id and covariate values are truncated to 11 characters.

- Missing observations are set to -99 (not ".").

- Incomplete dose records are flagged for the user to fix manually.

- Incomplete observation records are flagged for the user to fix
  manually.

- Subjects without an EVID=1 as first event are flagged for the user to
  fix manually.

- Subjects with TIME != 0 as first event have dummy dose=0 events
  inserted at time 0.

- Subjects with a missing covariate at time 0 are flagged for the user
  to fix manually.

- Non-numeric covariates are converted to numeric (via
  [`factor()`](https://rdrr.io/r/base/factor.html)).

- Non-ordered times are sorted within a subject if there are no EVID=4
  events; otherwise the user must fix manually.

- Non-contiguous subject ID rows are combined and sorted if there are no
  EVID=4 events; otherwise the user must fix manually.

- Rows missing an EVID are assigned a value of 0 if DOSE is missing, 1
  otherwise.

- Rows missing a TIME value are flagged for the user to fix manually.

- Cells with malformed NA values are attempted to be fixed.

- Columns that are non-numeric which must be numeric are flagged for the
  user to fix manually. These are all columns except ID. Covariate
  columns are fixed separately (see above).

- Dose events with censoring will be set to uncensored, with a warning
  to the user.

## See also

[PMwriteMatrix](https://lapkb.github.io/Pmetrics_rust/reference/PMwriteMatrix.md),
[PMreadMatrix](https://lapkb.github.io/Pmetrics_rust/reference/PMreadMatrix.md)

## Author

Michael Neely and Patrick Nolain

## Examples

``` r
if (FALSE) { # \dontrun{
err <- PMcheck(badData)
# look at the errors.xlsx file in the working directory
# try to automatically fix what can be fixed
goodData <- PMcheck(badCSV, fix = T)
PMcheck(goodData)
# you have to fix manually problems which require data entry
} # }
```
