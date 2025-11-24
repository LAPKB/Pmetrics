# Summarize Pmetrics Simulation Objects

**\[stable\]**

Summarize simulation

## Usage

``` r
# S3 method for class 'PM_sim'
summary(
  object,
  include,
  exclude,
  field = "obs",
  group = NULL,
  statistics = c("mean", "sd", "median", "min", "max"),
  digits = max(3, getOption("digits") - 3),
  ...
)
```

## Arguments

- object:

  The simulation object to summarize

- include:

  A vector of subject IDs to include in the plot, e.g. c(1:3,5,15) .

- exclude:

  A vector of subject IDs to exclude in the plot, e.g. c(4,6:14,16:20) .

- field:

  Quoted character value, one of

  - **obs** for simulated observations (the default)

  - **amt** for simulated amounts in each compartment

  - **parValues** for simulated parameter values

- group:

  Optional quoted values to group by, e.g. `group = "outeq"` or
  `group = c("id", "outeq")`.

- statistics:

  The summary statistics to report. Default is
  `c("mean", "sd", "median", "min", "max")`, but can be any subset of
  these and can also include specific quantiles, e.g.
  `c(25, "median", 75)`.

- digits:

  Integer, used for number of digits to print.

- ...:

  Not used.

## Value

If `by` is omitted, a data frame with rows for each data element except
ID, and columns labeled according to the selected `statistics`. If `by`
is specified, return will be a list with named elements according to the
selected `statistics`, each containing a data frame with the summaries
for each group in `by`.

## Details

Summarizes simulated observations, compartment amounts or parameter
values. Can be ungrouped (i.e. the entire simulation), average values
grouped by simulated id, outeq, or both, as well as summary statistics
for each individual. Default statistics reported are mean, sd, median,
min, max, but could include individual quantiles. See `statistics`
below.

## See also

[PM_sim](https://lapkb.github.io/Pmetrics_rust/reference/PM_sim.md)

## Author

Michael Neely

## Examples

``` r
if (FALSE) { # \dontrun{
simEx$summary() # preferred
summary(simEx) # alternative
simEx$summary(include = 2, field = "amt", group = "comp") # group amounts by compartment
} # }
```
