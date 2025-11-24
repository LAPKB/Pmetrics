# Load Pmetrics NPAG or IT2B output

**\[stable\]** Loads all the data from a prior Pmetrics run.

## Usage

``` r
PM_load(run, path = ".", file = "PMout.Rdata")
```

## Arguments

- run:

  The numerical value of the folder in `path` containing run results

- path:

  include Path to the folder containing the raw results of the run.
  Default is the current working directory. .

- file:

  Default is "PMout.Rdata", which is created after a Pmetrics run, but
  it could also be the name of an .Rdata file created by running the
  `$save` method for a
  [PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
  object.

## Value

An R6
[PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md).

## Details

A combination of `run`, `path`, and `file` are used to locate the
results.

- If `run` is provided, it is assumed that the results are in a
  subfolder `/outputs` of the folder named by `run` within the `path`
  folder.

- If `run` is missing, the results are assumed to be in the `path`
  folder.

- The `file` name is the name of the Rdata file containing the results.
  Default is "PMout.Rdata", which is created by Pmetrics after a run.

- If both `run` and `path` are missing, the current working directory is
  used for `path`.

- If both `run` and `file` are missing, the current working directory is
  used for `path` and "PMout.Rdata" is used for `file`.

- If both `path` and `file` are missing, the current working directory
  is used for `path` and `run` is required.

- If all three are missing, the current working directory is used for
  `path` and "PMout.Rdata" is used for `file`.

## See also

[PM_final](https://lapkb.github.io/Pmetrics_rust/reference/PM_final.md),
[PM_cycle](https://lapkb.github.io/Pmetrics_rust/reference/PM_cycle.md),
[PM_op](https://lapkb.github.io/Pmetrics_rust/reference/PM_op.md),
[PM_cov](https://lapkb.github.io/Pmetrics_rust/reference/PM_cov.md),
[PM_pop](https://lapkb.github.io/Pmetrics_rust/reference/PM_pop.md),
[PM_post](https://lapkb.github.io/Pmetrics_rust/reference/PM_post.md)

## Author

Michael Neely and Julian Otalvaro

## Examples

``` r
if (FALSE) { # \dontrun{
run1 <- PM_load(1) 
# loads from ./1/outputs/PMout.Rdata, where "." is the current working directory

run2 <- PM_load(2, path = "Pmetrics/MyRuns") 
# loads from Pmetrics/MyRuns/2/outputs/PMout.Rdata

run3 <- PM_load(path = "Pmetrics/MyRuns/3", file = "MyResults.Rdata") 
# loads from Pmetrics/MyRuns/3/MyResults.Rdata

run4 <- PM_load(file = "Pmetrics/MyRuns/4/outputs/PMout.Rdata") 
# loads from Pmetrics/MyRuns/4/outputs/PMout.Rdata

run5 <- PM_load() 
# loads from ./PMout.Rdata
} # }
```
