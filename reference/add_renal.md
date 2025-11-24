# Estimate renal function using various equations

**\[stable\]**

Adds columns to
[PM_data](https://lapkb.github.io/Pmetrics_rust/reference/PM_data.md)
object for creatinine clearance or estimated glomerular filtration rate
(eGFR).

Creatinine clearance can be estimated by:

- The Jelliffe equation for paired creatinines

- The Cockcroft-Gault equation using a single creatinine.

eGFR can be estimated by:

- The MDRD equation

- The CKD-EPI equation

- The Schwartz equation for children

## Usage

``` r
add_renal(
  x,
  method,
  id = "id",
  wt = "wt",
  ht = "ht",
  male = "male",
  age = "age",
  black = "black",
  scr = "scr",
  bun = "bun",
  cysC = "cysC",
  SI = F
)
```

## Arguments

- x:

  A
  [PM_data](https://lapkb.github.io/Pmetrics_rust/reference/PM_data.md)
  data object

- method:

  A character vector defining the method to use. Options are

  - "jelliffe" or "jel"

  - "cockcroft-gault" or "cg"

  - "mdrd"

  - "ckd-epi" or "ckd"

  - "schwartz" For the fuller versions, Only the first 4 letters are
    required

- id:

  A character vector with the name of the id column in `x`. The default
  is "id".

- wt:

  A character vector with the name of the weight column in `x`. The
  default is "wt". Only required for "jelliffe" and "cockcroft-gault"
  methods. Values must be in **kilograms**.

- ht:

  A character vector with the name of the height column in `x`. The
  default is "ht". Only required for the "schwartz" method. Values must
  be in **meters**

- male:

  A character vector with the name of the column defining sex in `x`.
  Male should be 1 and female should be 0. The default is "male".

- age:

  A character vector with the name of the age column in `x`. The default
  is "age". Values must be in **years**.

- black:

  A character vector with the name of a column defining black race in
  `x`. Male should be 1 and female should be 0. The default is "male".

- scr:

  A character vector with the name of the serum creatinine column in
  `x`. Default units are **mg/dL**, unless `SI = TRUE` below. The the
  default name is "scr".

- bun:

  A character vector with the name of the blood urea nitrogen column in
  `x`. Default units are **mg/dL**, unless `SI = TRUE` below. Default
  name is "bun". Optional for the Schwartz method.

- cysC:

  A character vector with the name of the cystatin C column in `x`.
  Units are always **mg/L**. Default is "cysC". Optional for the
  Schwartz method.

- SI:

  Boolean value, if true, will expect serum creatinine to be in
  **micromol/L** and BUN reported as **micromol/L** of urea. Default is
  `FALSE`.

## Value

A column added to the `x` data object with the name of the method used
and calcuated values.

## Details

**Note**: In all the equations below, age is in years, weight is in
kilograms, height is in meters, and cystatin C (cysC) is in mg/L. Serum
creatinine (Scr) is in mg/dL and blood urea nitrogen (BUN) is in mg/L.
However, if `SI = TRUE`, then serum creatinine is in micromol/L and BUN
is in micromol/L of urea.

Missing covariate values are interpolated using linear interpolation
within each subject.

- The **Jelliffe** equation depends on age, sex, weight, and serum
  creatinine. It uniquely uses two serum creatinine values separated by
  time to estimate changing renal function.

  - \\ESS = wt \* (29.3 - (0.203 \* age))\\ for males

  - \\ESS = wt \* (25.1 - (0.175 \* age))\\ for females

  - \\scrAve = (Scr1 + Scr2) / 2\\

  - \\ESS_cor = ESS \* (1.035 - (0.0337 \* scrAve))\\

  - \\E = ESS_cor - 4 \* wt \* (Scr2 - Scr1) / (time2 - time1)\\

  - \\CRCL = E / (14.4 \* scrAve)\\ in ml/min/1.73m^2

- The **Cockroft-Gault** equation is a point estimate of creatinine
  clearance. It depends on the same variables as Jelliffe except it does
  not require paired creatinines.

  - \\CRCL = ((140 - age) \* weight \* K)/(72 \* Scr)\\

  - \\K\\ is 1 for males and 0.85 for females

- The **MDRD** equation estimates GFR. It depends on age, sex, black
  race, and serum creatinine.

  - \\eGFR = 175 \* (Scr)^{-1.154} \* (age)^{-0.203}\\

  - Multiply by 1.212 if black.

- The **CKD-EPI** equation estimates GFR. It depends on the same
  variables as MDRD, except `black` is no longer required in the 2021
  version used here.

  - \\eGFR = 141 \* min(Scr/k, 1)^a \* max(Scr/k, 1)^{-1.209} \*
    0.993^{age}\\

  - Multiply by 1.1018 if female.

- The **Schwartz** equation estimates GFR in children. The equation used
  depends on which covariates are included in the data.

  - height and serum creatinine: \\eGFR = 41.3 \* height/scr\\ (updated
    Scwartz equation)

  - height, serum creatinine, and cystatin C: \\eGFR = 41.6 \*
    (height/scr)^{0.599} \* (1.8/cysC)^{0.317}\\ (Equation 1a)

  - height, serum creatinine, and BUN: \\eGFR = 40.7 \*
    (height/scr)^{0.640} \* (30/BUN)^{0.202}\\ (Equation 1b)

  - height, serum creatinine, cystatin C, and BUN: \\eGFR = 41.1 \*
    (height/scr)^{0.510} \* (1.8/cysC)^{0.272} \* (30/BUN)^{0.171}\\
    (Equation II)

  - height, serum creatinine, cystatin C, BUN, and male: \\eGFR = 39.1
    \* (height/scr)^{0.516} \* (1.8/cysC)^{0.294} \* (30/BUN)^{0.169} \*
    1.099^{male} \* (height/1.4)^{1.88}\\ (Equation III)

Discussion of the strengths and weaknesses of these equations are beyond
the scope of this documentation.

## Author

Michael Neely
