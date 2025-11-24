# Convert NONMEM to Pmetrics Data Files

**\[stable\]**

`NM2PM` will convert NONMEM .csv data files to Pmetrics csv data files.

## Usage

``` r
NM2PM(data, ctl)
```

## Arguments

- data:

  The name and extension of a NONMEM data (e.g. .csv) file in the
  working directory, or the full path to a file.

- ctl:

  The name and extension of a NONMEM control (e.g. .ctl) file in the
  working directory, or the full path to a file.

## Value

A Pmetrics style PMmatrix data.frame.

## Details

The format of NONMEM and Pmetrics data .csv files are similar, but not
quite identical. A major difference is that the order of the columns are
fixed in Pmetrics (not including covariates), while they are
user-determined in NONMEM, and specified in a control (.ctl) file.

A list of other differences follows by data item.

- ID This item is the same in both formats and is required.

- EVID This is the same in both formats but is not required in NONMEM.
  Doses have an EVID of 1 and observations 0. EVID=4 (dose/time reset)
  is the same in Pmetrics and NONMEM. EVID=2 (other event) and EVID=3
  (dose reset) are not directly supported in Pmetrics, but if included
  in a NONMEM file, will be converted into covariate values.
  Specifically the value in the CMT variable will be the covariate value
  for EVID=2, while for EVID=3, the covariate will be 1 at the time of
  the EVID=3 entry and 0 othewise. This allows for handling of these
  events in the Pmetrics model file using conditional statements.

- DATE Pmetrics does not use dates, but will convert all NONMEM dates
  and times into relative times.

- TIME Pmetrics uses relative times (as does NONMEM), but the NONMEM
  pre-processor will convert clock times to relative times, as does
  `NM2PM`.

- RATE NONMEM RATE items are converted by this function to Pmetrics
  DURation values.

- AMT becomes DOSE in Pmetrics

- ADDL is supported in both formats. However, if NONMEM files contain an
  SS flag, it will be incorporated as ADDL=-1 according to Pmetrics
  style.

- II is the same in both formats.

- INPUT in Pmetrics is similar to CMT in NONMEM for doses.

- DV in NONMEM becomes OUT in Pmetrics. Ensure that the units of OUT are
  consistent with the units of DOSE.

- OUTEQ In Pmetrics, this is roughly equivalent to CMT in NONMEM for
  observation events. The lowest CMT value for any observation becomes
  OUTEQ=1; the next lowest becomes OUTEQ=2, etc.

- SS Steady state dosing is incorporated into Pmetrics as ADDL=-1.

- MDV Missing DV in NONMEM become OUT=-99 in Pmetrics.

- Covariates These are copied from NONMEM to Pmetrics. Note that
  Pmetrics does not allow missing covariates at time 0 for each subject.

- DROP Items marked as DROP in the NONMEM control file will not be
  included in the Pmetric data file.

It is strongly suggested to run
[`PMcheck`](https://lapkb.github.io/Pmetrics_rust/reference/PMcheck.md)
on the returned object for final adjusting.

## See also

[`PMcheck`](https://lapkb.github.io/Pmetrics_rust/reference/PMcheck.md),
[`PMwriteMatrix`](https://lapkb.github.io/Pmetrics_rust/reference/PMwriteMatrix.md),
[`PMwrk2csv`](https://lapkb.github.io/Pmetrics_rust/reference/PMwrk2csv.md)

## Author

Michael Neely
