# Defines the PM_model class

**\[stable\]**

PM_model objects contain the variables, covariates, equations and error
models necessary to run a population analysis.

## Details

PM_model objects are one of two fundamental objects in Pmetrics, along
with
[`PM_data()`](https://lapkb.github.io/Pmetrics_rust/reference/PM_data.md)
objects. Defining a PM_model allows for fitting it to the data via the
`$fit()` method to conduct a population analysis, i.e. estimating the
probability distribution of model equation paramter values in the
population. The PM_model object is created using the a model building
app (coming soon), by defining a list directly in R, or by reading a
model text file. See the vignette on models for details.

**Some notes on the example at the end of this help page:**

- It's a complete example of a three compartment model with delayed
  absorption.

- We show the method of defining the model first and embedding the
  `PM_model$new()` within a `dontrun` block to avoid automatic
  compilation.

- Since this model can also be solved analytically with algebra, we
  could have used `eqn = function(){three_comp_bolus}`.

## Author

Michael Neely

## Public fields

- `model_list`:

  A list containing the model components built by translating the
  original arguments into Rust

- `arg_list`:

  A list containing the original arguments passed to the model

- `binary_path`:

  The full path and filename of the compiled model

## Methods

### Public methods

- [`PM_model$new()`](#method-PM_model-new)

- [`PM_model$print()`](#method-PM_model-print)

- [`PM_model$plot()`](#method-PM_model-plot)

- [`PM_model$fit()`](#method-PM_model-fit)

- [`PM_model$map()`](#method-PM_model-map)

- [`PM_model$sim()`](#method-PM_model-sim)

- [`PM_model$compile()`](#method-PM_model-compile)

- [`PM_model$update()`](#method-PM_model-update)

- [`PM_model$clone()`](#method-PM_model-clone)

------------------------------------------------------------------------

### Method `new()`

This is the method to create a new `PM_model` object. If all arguments
are `NULL`, e.g. `mod <- PM_model$new()` the model builder shiny app
will launch by a call to
[`build_model()`](https://lapkb.github.io/Pmetrics_rust/reference/build_model.md),
which will return the model object upon exit.

Otherwise, the first parameter allows creation of a model from a variety
of pre-existing sources, and if used, all the subsequent arguments will
be ignored. If a model is defined on the fly, the arguments form the
building blocks. Blocks are of two types:

- **Vectors** define *primary parameters*, *covariates*, and *error
  models*. These portions of the model have specific and defined creator
  functions and no additional R code is permissible. They take this
  form:

      block_name = c(
        var1 = creator(),
        var2 = creator()
      )

  Note the comma separating the creator functions, "`c(`" to open the
  vector and "`)`" to close the vector. Names are case-insensitive and
  are converted to lowercase for Rust.

- **Functions** define the other parts of the model, including
  *secondary (global) equations*, *model equations* (e.g. ODEs), *lag
  time*, *bioavailability*, *initial conditions*, and *outputs*. These
  parts of the model are defined as R functions without arguments, but
  whose body contains any permissible R code.

      block_name = function() {

       # any valid R code
       # can use primary or secondary parameters and covariates
       # lines are not separated by commas

      }

  Note the absence of arguments between the "`()`", the opening curly
  brace "`{`" to start the function body and the closing curly brace
  "`}`" to end the body. Again, all R code will be converted to
  lowercase prior to translation into Rust.

**Important:** All models must have `pri`, `eqn`, `out`, and `err`
blocks.

#### Usage

    PM_model$new(
      x = NULL,
      pri = NULL,
      cov = NULL,
      sec = NULL,
      eqn = NULL,
      lag = NULL,
      fa = NULL,
      ini = NULL,
      out = NULL,
      err = NULL,
      ...
    )

#### Arguments

- `x`:

  An optional argument, but if specified, all the subsequent arguments
  will be ignored. `x` creates a `PM_model` from existing appropriate
  input, which can be one of the following:

  - Quoted name of a model text file in the working directory which will
    be read and passed to Rust engine.

  - List that defines the model directly in R. This will be in the same
    format as if all the subsequent arguments were used. For example:

        mod_list <- list(
         pri = c(...),
         eqn = function(){...},
         out = function(){...},
         err = c(...)
        )
        mod <- PM_model$new(mod_list)

  - `PM_model` object, which will simply rebuild it, e.g. carrying on
    the prior example: `PM_model$new(mod)`

  See the user manual
  [`PM_manual()`](https://lapkb.github.io/Pmetrics_rust/reference/PM_manual.md)
  for more help on directly defining models in R.

- `pri`:

  The first of the arguments used if `x` is not specified. This is a
  named vector of primary parameters, which are the model parameters
  that are estimated in the population analysis. They are specified by
  one of two creator functions:
  [`ab()`](https://lapkb.github.io/Pmetrics_rust/reference/ab.md) or
  [`msd()`](https://lapkb.github.io/Pmetrics_rust/reference/msd.md). For
  example,

      pri = c(
        Ke = ab(0, 5),
        V = msd(100, 10)
      )

  The [`ab()`](https://lapkb.github.io/Pmetrics_rust/reference/ab.md)
  creator specifies the initial range `[a, b]` of the parameter, while
  the [`msd()`](https://lapkb.github.io/Pmetrics_rust/reference/msd.md)
  creator specifies the initial mean and standard deviation of the
  parameter.

- `cov`:

  A vector whose names are some or all of the covariates in the data
  file. Unlike prior versions of Pmetrics, as of 3.0.0, they do not have
  to be listed in the same order as in the data file, and they do not
  need to be all present. **Only those covariates used in model
  equations need to be declared here.** Values for each element in the
  covariate vector are the
  [`interp()`](https://lapkb.github.io/Pmetrics_rust/reference/interp.md)
  creator function to declare how each covariate is interpolated between
  entries in the data. The default argument for
  [`interp()`](https://lapkb.github.io/Pmetrics_rust/reference/interp.md)
  is "lm" which means that values will be linearly interpolated between
  entries, like the R linear model function
  [`stats::lm()`](https://rdrr.io/r/stats/lm.html). The alternative is
  "none", which holds the covariate value the same as the previous entry
  until it changes, i.e., a carry-forward strategy.

  For example:

      cov = c(
        wt = interp(), # will be linear by default
        visit = interp("none")
      )

  Note that `wt = interp()` is equivalent to `wt = interp("lm")`, since
  "lm" is the default.

- `sec`:

  A function defining the secondary (global) equations in the model.
  Values are not estimated for these equations but they are available to
  every other block in the model. For example:

      sec = function() {
        V = V0 * (wt/70)
      }

  Note that the function must be defined with no arguments between the
  parentheses, and the body **must be in R syntax**. Any number of lines
  and R code, e.g. `if` - `else` statements, etc. are permissible.

- `eqn`:

  A function defining the model equations. The function must have no
  arguments. The body of the function may contain three kinds of
  equations, written in R syntax.

  - **Implicit equations** referenced by calling the name of a Pmetrics
    model library object detailed in
    [`model_lib()`](https://lapkb.github.io/Pmetrics_rust/reference/model_lib.md).
    The Pmetrics model library contains a number of template models
    solved analytically (algebraically) and may include user-defined
    models. For example, to use a two-compartment model with intavenous
    input:

        eqn = function(){
         two_comp_iv
        }

    Required parameters for library models are listed for each model and
    must be included in model blocks exactly as named. For example, in a
    one-compartment model `Ke` is a required parameter. Thus, if `Ke` is
    a function of a covariate called "crcl", here is a code snippet
    illustrating the inclusion of the required parameter.

        mod <- PM_model$new(
          pri = c(
            ke0 = ab(0, 5),
            v = ab(0, 100)
          ),
          cov = c(
            crcl = interp()
          ),
          eqn = function(){
            one_comp
            ke = ke0 * crcl # the required parameter, ke, is defined
          },
          ... # more model blocks, including out, err
        )

  - **Explicit equations** are ordinary differential equations that
    directly define a model. Use the following notation in equations:

    - `dx[i]` for the change in amount with respect to time (i.e.,
      \\dx/dt\\), where `i` is the compartment number,

    - `x[i]` for the compartment amount, where `i` is the compartment
      number.

    - `rateiv[j]` for the infusion rate of input `j`, where `j` is the
      input number in the data corresponding to doses for that input.

    - Bolus doses are indicated by `DUR = 0` for dose events in the
      data. Currently only one bolus input is allowed, which goes into
      compartment 1 and is not modifiable. It does not appear in the
      differential equations.

      For example,

          eqn = function() {
           dx[1] = -ka * x[1]
           dx[2] = rateiv[1] + ka * x[1] - ke * x[2]
          }

  - **Additional equations** in R code can be defined in this block,
    which are similar to the `sec` block, but will only be available
    within the `eqn` block as opposed to global availability when
    defined in `sec`. They can be added to either

- `lag`:

  A function defining the lag time (delayed absorption) for the bolus
  input. The function must have no arguments, and the equations must be
  defined in R syntax The equations must be defined in the form of
  `tlag[i] = par`, where `tlag[i]` is the lag for drug (input) `i` and
  `par` is the lag parameter used in the `pri` block.

  For example, if `antacid` is a covariate in the data file, and `lag1`
  is a primary parameter, this code could be used to model delayed
  absorption if an antacid is present.

      lag = function() {
        tlag[1] = if(antacid == 1) lag1 else 0
      }

  As for `eqn`, additional equations in R code can be defined in this
  block, but will only be available within the `lag` block.

- `fa`:

  A function defining the bioavailability (fraction absorbed) equations,
  similar to `lag`.

  Example:

      fa = function() {
        fa[1] = if(antacid == 1) fa1 else 1
      }

  As for `eqn`, additional equations in R code can be defined in this
  block, but will only be available within the `fa` block.

- `ini`:

  A function defining the initial conditions for a compartment in the
  model. Structure is similar to `lag` and `fa`.

  Example:

      ini = function() {
        x[2] = init2 * V
      }

  This sets the initial amount of drug in compartment 2 to the value of
  a covariate `init2` multiplied by the volume of the compartment, `V`,
  assuming `V` is either a primary parameter or defined in the `sec`
  block.

  As for `eqn`, additional equations in R code can be defined in this
  block, but will only be available within the `ini` block.

- `out`:

  A function defining the output equations, which are the predictions
  from the model. The function must have no arguments, and the equations
  for predictions must be defined in R syntax.

  Use the following notation in equations:

  - `y[i]` for the predicted value, where `i` is the output equation
    number, typically corresponding to an observation with `outeq = i`
    in the data, but not always (see **Note** below).

  - `x[j]` for the compartment amount, where `j` is the compartment
    number.

  As with all function blocks, secondary equations are permitted, but
  will be specific to the `out` block.

  For example,

      out = function() {
        V = V0 * wt # only needed if not included in sec block
        y[1] = x[1]/V
        #Vp and Vm must be defined in pri or sec blocks
        y[2] = x[2]/Vp
        y[3] = x[3]/Vm
      }

  This assumes `V`, `Vp`, and `Vm` are either primary parameters or
  defined in the `sec` block.

  **Note** that as of Pmetrics 3.0.0, you can have more output equations
  than values for `outeq` in the data. This is not possible with prior
  versions of Pmetrics. Outputs without corresponding observations are
  not used in the fitting, but do generate predictions. For example,
  this snippet is part of a model that calculates AUC:

      eqn = function(){
        dx[1] = -ka * x[1]
        dx[2] = rateiv[1] + ka * x[1] - ke * x[2]
        dx[3] = x[2] - x[3]
        dx[4] = x[1] / v
      },
      out = function(){
        y[1] = x[1]/v
        y[2] = x[4]
      },
      err = c(
       proportional(2, c(0.1, 0.15, 0, 0))
      )

  If the data only contain observations for `y[1]`, i.e. the
  concentration of drug in the plasma compartment with `outeq = 1`, the
  model will use that information to optimize the parameter values, but
  will also generate predictions for `y[2]`, which is the AUC of the
  drug in compartment 1, even though there is no `outeq = 2` in the
  data. There is only one `err` equation since there is only one source
  of observations: plasma concentration. AUC (`y[2]`) is not fitted to
  any observations; it is a calculation based on the model state, given
  the optimized parameter values. It's not required, but shown here for
  illustrative purposes.

- `err`:

  An unammed vector of error models for each of the output equations
  with observations, i.e. those that have an `outeq` number associated
  with them in the data. Each error model is defined by the
  [`proportional()`](https://lapkb.github.io/Pmetrics_rust/reference/proportional.md)
  creator or the
  [`additive()`](https://lapkb.github.io/Pmetrics_rust/reference/additive.md)
  creator, relative to the observation error. For example, if there are
  three output equations corresponding to three sources of observations
  in the data, the error models could be defined as:

      err = c(
        proportional(2, c(0.1, 0.15, 0, 0)),
        proportional(3, c(0.05, 0.1, 0, 0)),
        additive(1, c(0.2, 0.25, 0, 0))
      )

  This defines the first two output equations to have proportional error
  with initial values of 2 and 3, respectively, and the third output
  equation to have additive error with initial value of 1. Each output
  is measured by a different assay with different error characteristics.

  If all the output equations have the same error model, you can simply
  use a single error model embedded in
  [`replicate()`](https://rdrr.io/r/base/lapply.html) , e.g., for 3
  outputs with the same error model:

      err = c(
        replicate(3, proportional(2, c(0.1, 0.15, 0, 0)))
      )

- `...`:

  Not currently used.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the model summary.

#### Usage

    PM_model$print(...)

#### Arguments

- `...`:

  Not used.

#### Details

This method prints a summary of the model.

------------------------------------------------------------------------

### Method [`plot()`](https://rdrr.io/r/graphics/plot.default.html)

Plot the model.

#### Usage

    PM_model$plot(...)

#### Arguments

- `...`:

  Additional arguments passed to the plot function.

#### Details

This method plots the model using the
[`plot.PM_model()`](https://lapkb.github.io/Pmetrics_rust/reference/plot.PM_model.md)
function.

------------------------------------------------------------------------

### Method [`fit()`](https://lapkb.github.io/Pmetrics_rust/reference/fit.md)

This is the main method to run a population analysis.

#### Usage

    PM_model$fit(
      data = NULL,
      path = ".",
      run = NULL,
      include = NULL,
      exclude = NULL,
      cycles = 100,
      prior = "sobol",
      points = 100,
      idelta = 0.1,
      tad = 0,
      seed = 23,
      overwrite = FALSE,
      algorithm = "NPAG",
      report = getPMoptions("report_template")
    )

#### Arguments

- `data`:

  Either the name of a
  [PM_data](https://lapkb.github.io/Pmetrics_rust/reference/PM_data.md)
  object in memory or the quoted filename (with or without a path) of a
  Pmetrics data file. If the path is not specified, the file is assumed
  to be in the current working directory, unless the `path` argument
  below is also specified as a global option for the fit. The file will
  be used to create a
  [PM_data](https://lapkb.github.io/Pmetrics_rust/reference/PM_data.md)
  object on the fly. However, if created on the fly, this object will
  not be available to other methods or other instances of `$fit()`.

- `path`:

  Optional full path or relative path from current working directory to
  the folder where `data` and `model` are located if specified as
  filenames without their own paths, and where the output will be saved.
  Default is the current working directory.

- `run`:

  Specify the run number of the output folder. Default if missing is the
  next available number.

- `include`:

  Vector of subject id values in the data file to include in the
  analysis. The default (missing) is all.

- `exclude`:

  A vector of subject IDs to exclude in the analysis, e.g.
  `c(4,6:14,16:20)`

- `cycles`:

  Number of cycles to run. Default is 100.

- `prior`:

  The distribution for the initial support points, which can be one of
  several options.

  - The default is "sobol", which is a semi-random distribution. This is
    the distribution typically used when fitting a new model to the
    data. An example of this is on our
    [website](https://www.lapk.org/images/sobol_3d_plot.html).

  The following all specify non-random, informative prior distributions.
  They are useful for either continuing a previous run which did not
  converge or for fitting a model to new data, whether to simply
  calculate Bayesian posteriors with `cycles = 0` or to revise the model
  to a new covergence with the new data.

  - The name of a suitable
    [PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
    object from a prior run loaded with
    [PM_load](https://lapkb.github.io/Pmetrics_rust/reference/PM_load.md).
    This starts from the non-uniform, informative distribution obtained
    at the end of a prior NPAG run. Example:
    `run1 <- PM_load(1); fit1$run(prior = run1)`.

  - A character string with the filename of a csv file containing a
    prior distribution with format as for 'theta.csv' in the output
    folder of a prior run: column headers are parameter names, and rows
    are the support point values. A final column with probabilities for
    each support point is not necessary, but if present will be ignored,
    as these probabilities are calculated by the engine. Note that the
    parameter names must match the names of the primary variables in the
    model. Example: `fit1$run(prior = "mytheta.csv")`.

  - The number of a previous run with `theta.csv` in the output folder
    which will be read as for the filename option above. Example:
    `fit1$run(prior = 2)`.

  - A data frame obtained from reading an approriate file, such that the
    data frame is in the required format described in the filename
    option above. Example:
    `mytheta <- read_csv("mytheta.csv"); fit1$run(prior = mytheta)`.

- `points`:

  The number of initial support points if one of the semi-random,
  uniform distributions are selected in the `prior` argument above.
  Default is 100. The initial points are spread through the hyperspace
  defined by the random parameter ranges and begin the search for the
  optimal parameter value distribution (support points) in the
  population. If there are fewer than 2 points per unit range for any
  parameter, Pmetrics will suggest the minimum number of points that
  should be tried. The greater the initial number of points, the less
  chance of missing the globally maximally likely parameter value
  distribution, but the slower the run.

- `idelta`:

  How often to generate posterior predictions in units of time. Default
  is 0.1, which means a prediction is generated every 0.1 hours (6
  minutes) if the unit of time is hours. Predictions are made at this
  interval until the time of the last event (dose or observation) or
  until `tad` if that value is greater than the time of the last dose or
  observation in the data.

- `tad`:

  Length of time after the last dose event to add additional predictions
  at frequency `idelta`. Default is 0, which means no additional
  predictions beyond the last dose, assuming the dose is the last event.
  . If the last observation in the data is after `tad`, then a
  prediction will be generated at time = `tad` after the last dose

- `seed`:

  Seed used if `prior = "sobol"`. Ignored otherwise.

- `overwrite`:

  Boolean operator to overwrite existing run result folders. Default is
  `FALSE`.

- `algorithm`:

  The algorithm to use for the run. Default is "NPAG" for the
  **N**on-**P**arametric **A**daptive **G**rid. Alternatives: "NPOD".

- `report`:

  If missing, the default Pmetrics report template as specified in
  [getPMoptions](https://lapkb.github.io/Pmetrics_rust/reference/getPMoptions.md)
  is used. Otherwise can be "plotly", "ggplot", or "none".

- `intern`:

  Run NPAG in the R console without a batch script. Default is TRUE.

#### Details

As of Pmetrics 3.0.0, models contain compiled code to fit the model
equations to the data, optimizing the parameter value probability
distributions in the population to maximize their likelihood, or more
precisely, minimize the objective function, which is -2\*log-likelihood.

The `$fit()` method is the means of running that compiled code to
conduct to fitting procedure. At a minimum, it requires a
[PM_data](https://lapkb.github.io/Pmetrics_rust/reference/PM_data.md)
object, which can be created with `PM_data$new()`. There are a number of
additional arguments to control the fitting procedure, such as the
number of cycles to run, the initial number of support points, and the
algorithm to use, among others.

The `$fit()` method is the descendant of the legacy
[NPrun](https://lapkb.github.io/Pmetrics_rust/reference/NPrun.md)
function, which is maintained as a wrapper to `$fit()` for backwards
compatibility.

#### Returns

A successful run will result in creation of a new folder in the working
directory with the results inside the folder.

------------------------------------------------------------------------

### Method `map()`

Calculate posteriors from a fitted model. \#' @details This method
calculates posteriors from a compiled model. It is not necessary to have
run the model first, but it is necessary to have an informative prior
distribution. This prior will typically be the result of a previous run,
but may also be a file containing support points, with each column named
as a parameter in the model plus a final column for probability. Each
row contains values for the parameters and the associated probability
for those parameter values. The file can be saved as a csv file.

To calculate the posteriors, `map()` calls the
[`fit()`](https://lapkb.github.io/Pmetrics_rust/reference/fit.md) method
with the `cycles` argument set to 0 and the `algorithm` argument set to
"POSTPROB". If `data` are not provided as an argument to `map()`, the
model's `data` field is used instead. If `data` is provided, it must be
a [PM_data](https://lapkb.github.io/Pmetrics_rust/reference/PM_data.md)
object or the pathname of a file which can be loaded as a
[PM_data](https://lapkb.github.io/Pmetrics_rust/reference/PM_data.md)
object.

#### Usage

    PM_model$map(...)

#### Arguments

- `...`:

  Arguments passed to the `fit` method. Note that the `cycles` argument
  is set to 0, and the `algorithm` argument is set to "POSTPROB"
  automatically.

------------------------------------------------------------------------

### Method `sim()`

Simulate data from the model using a set of parameter values.

#### Usage

    PM_model$sim(data, theta)

#### Arguments

- `data`:

  A
  [PM_data](https://lapkb.github.io/Pmetrics_rust/reference/PM_data.md)
  object containing the dosing and observation information.

- `theta`:

  A matrix of parameter values to use for the simulation. The `theta`
  matrix should have the same number of columns as the number of primary
  parameters in the model. Each row of `theta` represents a different
  set of parameter values.

#### Details

This method simulates output from the model using a set of parameter
values provided in the `theta` matrix and the template for
dosing/observations in the `data` object.

------------------------------------------------------------------------

### Method `compile()`

Compile the model to a binary file.

#### Usage

    PM_model$compile()

#### Details

This method write the model to a Rust file in a temporary path, updates
the `binary_path` field for the model, and compiles that file to a
binary file that can be used for fitting or simulation. If the model is
already compiled, the method does nothing.

------------------------------------------------------------------------

### Method [`update()`](https://rdrr.io/r/stats/update.html)

Update the model using recursive lists of changes and recompile the
updated model.

#### Usage

    PM_model$update(...)

#### Arguments

- `...`:

  Named elements corresponding to the blocks in the model, such as
  "pri", "cov", "sec", "eqn", "ini", "lag", "fa", "out", and "err". For
  each block, create a list of changes, which may be additions, edits,
  or deletions. For deletions, set the value to `NULL`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PM_model$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
mod_list <- list(
  pri = list(
    CL = ab(10, 200),
    V0 = ab(0, 100),
    ka = ab(0, 3),
    k23 = ab(0, 5),
    k32 = ab(0, 5),
    lag1 = ab(0, 2)
  ),
  cov = list(
    wt = interp()
  ),
  sec = function() {
    V <- V0 * (wt / 70)
    ke <- CL / V # define here to make eqn simpler
  },
  eqn = function() {
    dx[1] <- -ka * x[1]
    dx[2] <- rateiv[1] + ka * x[1] - (ke + k23) * x[2] + k32 * x[3]
    dx[3] <- k23 * x[2] - k32 * x[3]
    dx[4] <- x[1] / V
  },
  lag = function() {
    tlag[1] <- lag1
  },
  out = function() {
    y[1] <- x[1] / V
    y[2] <- x[4] # AUC, not fitted to any data, not required
  },
  err = list(
    proportional(2, c(0.1, 0.15, 0, 0)) # only applies to y[1]
  )
)

if (FALSE) { # \dontrun{
mod <- PM_model$new(mod_list)
} # }
```
