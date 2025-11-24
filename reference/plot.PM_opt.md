# Plot Pmetrics Multiple-Model Optimal Sampling Objects

**\[stable\]**

Plots
[PM_opt](https://lapkb.github.io/Pmetrics_rust/reference/PM_opt.md)
objects

## Usage

``` r
# S3 method for class 'PM_opt'
plot(x, line = list(probs = NA), times = T, print = TRUE, ...)
```

## Arguments

- x:

  A [PM_opt](https://lapkb.github.io/Pmetrics_rust/reference/PM_opt.md)
  object

- line:

  Passed to
  [plot.PM_sim](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_sim.md)
  with default as `list(probs = NA)`.

- times:

  Format the vertical lines for optimal times. Default is dashed red
  line. r template("line")\`

- print:

  If `TRUE`, will print the plotly object and return it. If `FALSE`,
  will only return the plotly object.

- ...:

  Other parameters to pass to
  [plot.PM_sim](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_sim.md).

- probs:

  Default is NA. See
  [plot.PM_sim](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_sim.md)
  for details.

## Value

Plots the simulation profiles with MM optimal times indicated as
vertical lines.

## Details

Simulated observations are plotted on the y-axis vs. time on the x.axis.
Optimal sampling times are indicated as vertical lines. Defaults for
optimal sample times are red, dash, width 2. Defaults for the `line`
format are as for
[plot.PM_sim](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_sim.md).

## See also

[plot.PM_sim](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_sim.md)

Other PMplots:
[`plot.PM_cov()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_cov.md),
[`plot.PM_cycle()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_cycle.md),
[`plot.PM_data()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_data.md),
[`plot.PM_final()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_final.md),
[`plot.PM_model()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_model.md),
[`plot.PM_op()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_op.md),
[`plot.PM_pop()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_pop.md),
[`plot.PM_post()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_post.md),
[`plot.PM_pta()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_pta.md),
[`plot.PM_sim()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_sim.md),
[`plot.PM_valid()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_valid.md)

## Author

Michael Neely
