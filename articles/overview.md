# Overview

## 1 Software engines ⚙️

There are three main software engines that Pmetrics controls currently.

- **NPAG** is the Non-parametric Adaptive Grid software. It will create
  a non-parametric population model consisting of discrete support
  points, each with a set of estimates for all parameters in the model
  plus an associated probability (weight) of that set of estimates.
  There can be at most one point for each subject in the study
  population. There is no need for any assumption about the underlying
  distribution of model parameter values.

- **NPOD** is the Non-parametric Optimal Design software. Like NPAG, it
  will create a non-parametric population model consisting of discrete
  support points, each with a set of estimates for all parameters in the
  model plus an associated probability (weight) of that set of
  estimates. While NPAG searches parameter hyperspace randomly, NPOD
  uses likelihood gradients to search systematically. This usually
  results in faster convergence, but currently, NPOD is more likely to
  get stuck in a “local” minimum of the likelihood surface, i.e. it has
  a higher chance of not finding the globally optimal support points.
  NPOD is under active development.

- The **Simulator** is a semi-parametric Monte Carlo simulation software
  program that can use the output of NPAG of NPOD to build randomly
  generated response profiles (e.g. time-concentration curves) for a
  given population model, parameter estimates, and data input.
  Simulation from a non-parametric joint density model, i.e. NPAG
  output, is possible, with each point serving as the mean of a
  multivariate normal distribution, weighted according to the weight of
  the point. The covariance matrix of the entire set of support points
  is divided equally among the points for the purposes of simulation.

## 2 Pmetrics control functions 🎛️

Pmetrics uses
[`PM_data`](https://lapkb.github.io/Pmetrics/reference/PM_data.html) to
create data objects and
[`PM_model`](https://lapkb.github.io/Pmetrics/reference/PM_model.html)
to create model objects. When a model is created, it is compiled and
ready to be combined with the data and parameters within the model’s
`$fit()` method, generating probability distributions for primary model
parameters. `PM_data` and `PM_model` objects and their methods are
extensively documented within R by using the `help(command)` or
`?command` syntax.

The `$fit()` method replaces these Legacy functions: `ITrun`, `ERRrun`,
`NPrun`.

Invoking the simulator in R6 becomes a method attached to `PM_result`
objects or by using `PM_sim$new()` for models, parameter value
probability distributions and template data not derived from a previous
fit, e.g. when lifted from an article.

### 2.1 Fit method 📈

Once a `PM_model` object is compiled, combine it with a data file, and
fit the model to the data using the `$fit()` method.

``` r
run1 <- mod1$fit(data, ...) #... are options listed in ?PM_model for the $fit method
```

For the Simulator, the `$sim` method for a `PM_result` object will
execute the program directly within R and return a `PM_sim` object.

``` r
run1 <- PM_load(1)
sim1 <- run1$sim(data = "new.csv") # uses the model and parameter joint density from run1, but replaces the data with new.csv
```

The first line loads previous results of run 1 into a `PM_result` object
called **run1**. The second line uses the model and prior in run1 with
new data template. `sim1` is a
[`PM_sim`](https://lapkb.github.io/Pmetrics/reference/PM_sim.html)
object.

An equivalent method is to create a new `PM_sim` is directly with
`PM_sim$new()`. It also returns a
[`PM_sim()`](https://lapkb.github.io/Pmetrics_rust/reference/PM_sim.md)
object, i.e. there is no longer any need to run `SIMparse`. Creating
simulations this way allows total freedom with respect to the model, the
prior, and the data template.

``` r
sim1 <- PM_sim$new(poppar = list(...), model = "model.txt", data = "new.csv")
```

See
[simulation](https://lapkb.github.io/Pmetrics_rust/articles/simulation.md)
for details on specifying `poppar` this way.

For the simulator the results are returned automatically to the object
assigned to contain the output of the simulation, e.g. `sim1` below.

``` r
sim1 <- PM_result$sim(...)
```

As mentioned above, there is no longer any need to use the `SIMparse`
Legacy function, because the `SIMrun` and `SIMparse` Legacy functions
are combined within the `$sim()` method for `PM_result` objects or the
`PM_sim$new()` method.

### 2.2 Saving functions 💾

The Pmetrics R6 objects `PM_data`,
[`PM_result()`](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md),
[`PM_sim()`](https://lapkb.github.io/Pmetrics_rust/reference/PM_sim.md),
[`PM_valid()`](https://lapkb.github.io/Pmetrics_rust/reference/PM_valid.md),
and
[`PM_pta()`](https://lapkb.github.io/Pmetrics_rust/reference/PM_pta.md)
all have a `$save()` method. This method saves the object to the hard
drive in the current working directory by default. The format is .rds
which is a binary format used by R to save individual objects. The
purpose of the `$save()` method is to enable retrieval of the object at
a later time.

### 2.3 Loading functions 📂

After a successful NPAG or NPOD run,
[`PM_load()`](https://lapkb.github.io/Pmetrics_rust/reference/PM_load.md)
creates a
[`PM_result()`](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
object rather than loading run results into the current environment and
suffixed with the run number as for legacy mode.

``` r
res1 <- PM_load(1)
res1$op$plot()
```

For
[`PM_sim()`](https://lapkb.github.io/Pmetrics_rust/reference/PM_sim.md),
[`PM_valid()`](https://lapkb.github.io/Pmetrics_rust/reference/PM_valid.md),
and
[`PM_pta()`](https://lapkb.github.io/Pmetrics_rust/reference/PM_pta.md)
use the `$new()` method and provide the full or relative path (if not in
the working directory) and name of an .rds file created by the
corresponding `$save()` method.

``` r
sim1 <- PM_sim$new("sim.rds")
```

### 2.4 Report generation 📄

The
[`PM_report`](https://lapkb.github.io/Pmetrics/reference/PM_report.html)
function is automatically run at the end of a successful run, and it
will generate an HTML page with summaries of the run, as well as the
.Rdata files and other objects. The default browser will be
automatically launched for viewing of the HTML report page.

## 3 Other functions

Within Pmetrics there are also functions to manipulate data .csv files
and process and plot extracted data.

### 3.1 Data manipulation

Comparison between the current and the Legacy methods are shown for
education.

| Function                               | R6                         | Legacy            |
|----------------------------------------|----------------------------|-------------------|
| Read data file                         | PM_data\$new()             | PMreadMatrix()    |
| Check data file                        | Embedded in PM_data\$new() | PMcheck()         |
| Write data file                        | PM_data\$save()            | PMwriteMatrix()   |
| Convert calendar dates and clock times | Embedded in PM_data\$new() | PMmatrixReltime() |
| Convert from old USC\\PACK .wrk format | PMwrk2csv()                | PMwrk2csv()       |
| Convert from NONMEM                    | NM2PM()                    | NM2PM()           |

### 3.2 Model selection and diagnostics

Comparison between the current and the Legacy methods are shown for
education.

| Function                      | R6                                    | Legacy              |
|-------------------------------|---------------------------------------|---------------------|
| Compare models                | PM_compare(PM_result1, PMresult2,…)   | PMcompare(1, 2, …)  |
| Plot residuals                | PM_result\$op\$plot(resid = T,…)      | plot(op, resid = T) |
| Construct VPC, pcVPC, NPDE    | PM_result\$valid() or PM_valid\$new() | makeValid()         |
| Plot VPC, pcVPC, NPDE         | PM_valid\$plot()                      | plot(PMvalid)       |
| Stepwise covariate regression | PM_result\$step()                     | PMstep()            |

### 3.3 Other functions

Comparison between the current and the Legacy methods are shown for
education.

| Function                         | R6                                                       | Legacy    |
|----------------------------------|----------------------------------------------------------|-----------|
| Calculate AUC                    | \$auc() method for PM_result\$op/\$post/\$pop, or PM_sim | makeAUC() |
| Simulate                         | PM_result\$sim() or PM_sim\$new()                        | SIMrun()  |
| Probability of target attainment | PM_sim\$pta() or PM_pta\$new()                           | makePTA() |

Again, all functions have extensive help files and examples which can be
examined in R by using the `help(command)` or `?command` syntax.
