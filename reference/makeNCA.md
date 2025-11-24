# Non-compartmental analysis

Performs a non-compartmental analysis from observed concentrations in
the raw data file or from an individual Bayesian posterior predicted
time-observation profiles in
[PM_post](https://lapkb.github.io/Pmetrics_rust/reference/PM_post.md)
generated automatically after an NPAG run and loaded with
[PM_load](https://lapkb.github.io/Pmetrics_rust/reference/PM_load.md).

## Usage

``` r
makeNCA(
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

  Data to analyze. This can be specified in a number of ways.

  - It can be the run number, e.g. 3, that has been previously loaded
    with
    [PM_load](https://lapkb.github.io/Pmetrics_rust/reference/PM_load.md).
    Either the mdata file from the run (NPAG or IT2B) can be used
    (default) or the post object can be used (NPAG only) by specifying
    `postPred = T` below. If
    [ggplot2::x](https://ggplot2.tidyverse.org/reference/aes_position.html)
    is a run number that corresponds to both an NPAG and IT2B run which
    have been previously loaded into memory with
    [PM_load](https://lapkb.github.io/Pmetrics_rust/reference/PM_load.md),
    the NPAG run will be used.

  - It can be the run number of a run that has *not* been previously
    loaded with
    [PM_load](https://lapkb.github.io/Pmetrics_rust/reference/PM_load.md).
    In this case, the current working directory should be the Runs
    folder as makeNCA will call
    [PM_load](https://lapkb.github.io/Pmetrics_rust/reference/PM_load.md).

  - It can be the specific name of an mdata.x file already loaded into
    memory with
    [PM_load](https://lapkb.github.io/Pmetrics_rust/reference/PM_load.md),
    e.g. mdata.3. Note that quotation marks are not necessary since
    mdata.3 is an object, not a label/character vector.

  - Finally, it can be the name of a Pmetrics data file in the current
    working directory, which will be loaded with
    [PMreadMatrix](https://lapkb.github.io/Pmetrics_rust/reference/PMreadMatrix.md)
    and analyzed, e.g. "data.csv". In this case, quotation marks are
    reqired, because `x` is now a character vector specifying the
    filename of the file to load.

- postPred:

  Boolean switch to use the posterior predictions rather than the
  observed. concentrations. Default is `FALSE`. Ignored if an IT2B run
  is used to supply the raw data file.

- include:

  A vector of subject IDs to include in the NCA, e.g. c(1:3,5,15)

- exclude:

  A vector of subject IDs to exclude in the NCA, e.g. c(4,6:14,16:20).
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
  subject, e.g. c(120,120,144,168). If the length of `start` is less
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
  e.g. c(1,1,1,3,1,...) to allow for individualization by subject. The
  last value will be recycled to ensure length equal to the number of
  subjects. Default is `NA`, which means `start` will be used.

- last:

  The complement to `first`, specifying the last dose to end the time
  interval. If `NA`, which is the default, then the maximum time per
  subject will be the upper bound of the time interval. Like `first`,
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
  slope, this value and all others that depend on `k` will be
  suppressed.

- auclast :

  Area under the curve from the time of the last observation to
  infinity, calculated as \\Final obs\\/k. This value will be suppressed
  if start != 0.

- aumclast :

  Area under the first moment curve from the time of the last
  observation to infinity. This value will be suppressed if start!=0.

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
`start` to `end` time interval, the function will superpose the
concentrations using the time after dose. An error will be generated if
different doses are within this interval as superposition would no
longer be valid.

A minimum of 5 concentrations must be available to perform NCA for any
given subject. Fewer than this will suppress all results for that
subject.

## Author

Michael Neely

## Examples

``` r
makeNCA(NPex$data)
#>    id      auc      aumc           k   auclast   aumclast    aucinf    aumcinf
#> 1   1 361.8704 4133.5637 0.026673314 471.25753  28968.509  833.1280  33102.072
#> 2   2 121.0778 1286.2205 0.030510262 123.23723   6996.899  244.3150   8283.120
#> 3   3 120.8402 1365.0443 0.035395480 107.92338   5647.867  228.7636   7012.912
#> 4   4  87.1521  866.1199 0.051809452  37.83093   1638.893  124.9830   2505.013
#> 5   5  75.7889  787.1226 0.035508428  62.23874   3246.518  138.0276   4033.640
#> 6   6 240.5456 2757.7293 0.039321570 168.35543   8487.022  408.9010  11244.751
#> 7   7 200.5504 2309.1628 0.037776577 168.62301   8515.703  369.1734  10824.866
#> 8   8 238.9118 2785.1228 0.028829268 271.59898  15933.890  510.5108  18719.013
#> 9   9 148.2174 1676.0129 0.029518224 152.78697   8839.853  301.0044  10515.866
#> 10 10 160.8878 1658.3495 0.045183937  91.84680   4237.054  252.7346   5895.404
#> 11 11 121.3709 1271.6191 0.060039952  43.80417   1786.578  165.1751   3058.197
#> 12 12 207.0078 2408.1546 0.009635465 818.84995 104635.319 1025.8577 107043.473
#> 13 13 235.1283 2862.7292 0.032369319 249.00122  13668.536  484.1295  16531.266
#> 14 14 114.1000 1153.1800 0.046519111  57.61073   2621.089  171.7107   3774.269
#> 15 15 179.8273 1999.4247 0.025417183 225.83148  14316.240  405.6587  16315.664
#> 16 16 185.4531 1945.0044 0.042009360 118.06893   5640.651  303.5221   7585.655
#> 17 17 137.0686 1447.8541 0.042034527  85.40598   4088.381  222.4746   5536.236
#> 18 18 241.9302 2740.7573 0.026364520 318.61001  19721.885  560.5402  22462.642
#> 19 19 333.7126 3911.8963 0.032526897 335.72216  18378.702  669.4348  22290.598
#> 20 20 209.1043 2306.7428 0.036994164 174.35183   8839.863  383.4562  11146.605
#>          mrt  cmax  tmax        cl      vdss    thalf dose
#> 1   39.73228 20.15  9.00 0.7201775  28.61429 25.98654  600
#> 2   33.90344  6.56  9.02 2.4558457  83.26162 22.71849  600
#> 3   30.65572  7.98  6.08 2.6227951  80.40366 19.58293  600
#> 4   20.04282  5.21  2.02 4.8006516  96.21860 13.37878  600
#> 5   29.22342  4.80  2.00 4.3469553 127.03292 19.52064  600
#> 6   27.49993 13.72  6.00 1.4673477  40.35197 17.62766  600
#> 7   29.32190 12.19  6.02 1.6252527  47.65550 18.34860  600
#> 8   36.66722 13.03  5.98 1.1752934  43.09474 24.04318  600
#> 9   34.93593  7.48 12.00 1.9933265  69.63871 23.48201  600
#> 10  23.32647 11.02  6.00 2.3740324  55.37779 15.34057  600
#> 11  18.51488  7.71  5.98 3.6325084  67.25545 11.54477  600
#> 12 104.34534 11.16  6.00 0.5848764  61.02913 71.93708  600
#> 13  34.14637 12.84  9.02 1.2393378  42.31889 21.41371  600
#> 14  21.98039  7.28  6.00 3.4942487  76.80495 14.90027  600
#> 15  40.22017  9.96  6.00 1.1093068  44.61651 27.27081  450
#> 16  24.99210 10.57  6.00 1.9767920  49.40419 16.49983  600
#> 17  24.88480  9.11  6.00 2.6969373  67.11275 16.48995  600
#> 18  40.07320 14.01  9.17 0.8027970  32.17065 26.29091  450
#> 19  33.29764 20.07  5.98 0.8962785  29.84396 21.30997  600
#> 20  29.06879 12.35  5.67 1.5647160  45.48439 18.73666  600
```
