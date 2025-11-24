# Example NPAG Output with validation

Example output from an NPAG run with validation.

## Usage

``` r
NPex
```

## Format

R6
[PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)

## Details

This is an R6 Pmetrics
[PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
object created with
[`PM_load()`](https://lapkb.github.io/Pmetrics_rust/reference/PM_load.md)
after an NPAG run. The run consisted of a model with an absorptive
compartment and a central compartment. There were 4 parameters in the
model: lag time of absorption (Tlag1), rate constant of absorption (Ka),
volume (V) with weight as a covariate, and rate constant of elmination
(Ke). There were 20 subjects in the dataset. The run was 100 cycles long
and did not converge. It was then validated with the `$validate` method
for
[PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
objects.

## Author

Michael Neely
