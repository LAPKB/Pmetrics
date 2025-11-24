# Pmetrics model object

Example model for an NPAG/IT2B run. There are 4 parameters in the model:
lag time of absorption (Tlag1), rate constant of absorption (Ka), volume
(V) and rate constant of elmination (Ke). There are 5 covariates: weight
in kg (WT), whether from Africa or not (AFRICA), age in years (AGE), 1
for male (GENDER), and height in cm (HEIGHT). There is one output
equation, and the model uses gamma plus an error polynomial derived from
the assay.

## Usage

``` r
modEx
```

## Format

R6
[PM_model](https://lapkb.github.io/Pmetrics_rust/reference/PM_model.md)

## Author

Michael Neely
