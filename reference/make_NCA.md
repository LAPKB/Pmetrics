# Non-compartmental analysis

Performs a non-compartmental analysis from a
[PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
object using observed concentrations in the raw data file
`PM_result$data$standard_data` or from an individual Bayesian posterior
predicted time-observation profiles `PM_result$post$data` generated
automatically after an NPAG run and loaded with
[PM_load](https://lapkb.github.io/Pmetrics_rust/reference/PM_load.md) .

## Usage

``` r
make_NCA(
  x,
  postPred = F,
  include,
  exclude,
  input = 1,
  icen = "median",
  outeq = 1,
  block = 1,
  start = 0,
  end = Inf,
  first = NA,
  last = NA,
  terminal = 3
)
```

## Arguments

- x:

  PM_data object to analyze.

- postPred:

  Boolean switch to use the posterior predictions rather than the
  observed. concentrations. Default is `FALSE`. Ignored if an IT2B run
  is used to supply the raw data file.

- include:

  A vector of subject IDs to include in the NCA, e.g. `c(1:3,5,15)`

- exclude:

  A vector of subject IDs to exclude in the NCA, e.g. `c(4,6:14,16:20)`.
  When `postPred` is `TRUE`, any subject(s) excluded from the IT2B/NPAG
  run will be excluded as well.

- input:

  The number of the input (e.g. drug) to analyze; default 1.

- icen:

  If `postPred` is `TRUE`, use predictions based on median or mean of
  each subject's Bayesian posterior parameter distribution. Default is
  "median", but could be "mean".

- outeq:

  The number of the output equation to analyze; default 1

- block:

  The number of the observation block within subjects, with each block
  delimited by EVID=4 in the data file; default 1

- start:

  The beginning of the time interval to look for doses and observations,
  e.g. 120. It can be a vector to allow for individual start times per
  subject, e.g. `c(120,120,144,168)`. If the length of `start` is less
  than the number of subjects, the last value will be recycled as
  needed. If the `start` time is not 0 (default), then it is assumed
  that steady state (multiple dose) conditions apply.

- end:

  Analogous to `start`, set this equal to the end of the dosing
  interval. It too can be a vector, with the last value recycled as
  necessary. Default is `Inf`, i.e. all data used.

- first:

  Alternative way to specify time interval for NCA by choosing dose
  number, e.g. 1 or 3. May be a numeric vector, like `start` and `end`,
  e.g. `c(1,1,1,3,1,...)` to allow for individualization by subject. The
  last value will be recycled to ensure length equal to the number of
  subjects. Default is `NA`, which means `start` will be used.

- last:

  The complement to `first`, specifying the last dose to end the time
  interval. If `NA`, which is the default, then the maximum time per
  subject will be the upper bound of the time interval.Like `first`,
  `last` can be a vector, with the last value recycled as necessary. Use
  `NA` in the vector to signify maximum time for that subject.

- terminal:

  Number of observations to use for terminal curve fitting (i.e. to
  estimate *k*). Default is 3.

## Value

A dataframe of class *PMnca* with columns

- id :

  Subject identification

- auc :

  Area under the time-observation curve, using the trapezoidal
  approximation, from time 0 until the second dose, or if only one dose,
  until the last observation

- aumc :

  Area under the first moment curve

- k :

  Slope by least-squares linear regression of the final 3
  log-transformed observations vs. time. If the final 3 concentrations
  are not decreasing such that linear regression results in a positive
  slope, this value and all others that depend on *k* will be
  suppressed.

- auclast :

  Area under the curve from the time of the last observation to
  infinity, calculated as `Final obs/k`. This value will be suppressed
  if `start != 0`.

- aumclast :

  Area under the first moment curve from the time of the last
  observation to infinity. This value will be suppressed if
  `start != 0`.

- aucinf :

  Area under the curve from time 0 to infinity, caluculated as auc +
  auclast

- aumcinf :

  Area under the first moment curve from time 0 to infinity

- mrt :

  Mean residence time, calculated as 1/k

- cmax :

  Maximum predicted concentration after the first dose

- tmax :

  Time to cmax

- cl :

  Clearance, calculated as dose/aucinf

- vdss :

  Volume of distribution at steady state, calculated as cl\*mrt

- thalf :

  Half life of elimination, calculated as ln(2)/k

- dose :

  Dose for each subject

## Details

If concentrations from multiple dose intervals are included in the
`start` to `end` time interval, make_NCA will superpose the
concentrations using the time after dose. An error will be generated if
different doses are within this interval as superposition would no
longer be valid.

A minimum of 5 concentrations must be available to perform NCA for any
given subject. Fewer than this will suppress all results for that
subject.

## Author

Michael Neely
