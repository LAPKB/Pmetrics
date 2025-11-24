# Running model fits

## 1 Fitting data to models

In this section, we suggest a workflow to help you maintain organized
modeling projects.

### 1.1 Setting up a Pmetrics project

Legacy  
R6

When beginning a new modeling project, it is convenient to use the
command
[`PMtree()`](https://lapkb.github.io/Pmetrics_rust/reference/PM_tree.md).
This command will set up a new directory in the current working
directory named whatever you have included as the “project name”.

``` r
PMtree("DrugX")
```

In the above example, a directory called “DrugX” will be created in the
current working directory in R, which you can check with the `getwd`
function. Beneath the new DrugX directory, several subdirectories will
be also created.

- **Rscript** contains a skeleton R script to begin Pmetrics runs in the
  new project.
- **Runs** should contain all files required for a run (described next)
  and it will also contain the resulting numerically ordered run
  directories created after each Pmetrics NPAG or IT2B run.
- **Sim** can contain any files related to simulations
- **src** is a repository for original and manipulated source data files

You are free to edit this directory tree structure as you please, or
make your own entirely.

### 1.2 Getting the required inputs to run Pmetrics

R6

There is a full tutorial encoded inside Pmetrics to teach new users the
basic functionality of the whole package. To start that tutorial in R
type:

``` r
library(Pmetrics)
PM_tutorial()
```

Follow the instructions prompted in the terminal.

To setup a R6 Pmetrics run, you must provide
[`PM_data()`](https://lapkb.github.io/Pmetrics_rust/reference/PM_data.md)
and
[`PM_model()`](https://lapkb.github.io/Pmetrics_rust/reference/PM_model.md)
objects. Once created, it does not matter where the files (if there are
any) are located on your system because Pmetrics will use those objects,
not the files.

### 1.3 Running the model to fit the data

Once the
[`PM_model()`](https://lapkb.github.io/Pmetrics_rust/reference/PM_model.md)
object is created and compiled the `$fit()` method will execute the fit.
Arguments for this method can be found in the help for `$fit` in
[`PM_model`](https://lapkb.github.io/Pmetrics/reference/PM_model.html).

``` r
# default run parameters
mod1$fit(data = dat1)

# change the cycle number from default 100
mod1$fit(data = dat1, cycles = 500)

# change the engine from default NPAG
mod1$fit(engine = "NPOD")
```

## 2 Loading results after a completed run

After the execution is done, you can load the output into R using
[`PM_load()`](https://lapkb.github.io/Pmetrics_rust/reference/PM_load.md).
Note the underscore “\_” to distinguish this function from the Legacy
[`PMload()`](https://lapkb.github.io/Pmetrics_rust/reference/PMload.md).
The argument to the function is the run number which you wish to load,
corresponding to a folder with the same number in your **Runs** folder
(if you made one).

``` r
my_run <- PM_load(1)
```

This creates a
[`PM_result()`](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
object called `my_run`. Detailed information about the different
elements contained in the result object can be accessed via
[`?PM_result`](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
or by typing the result object into the terminal.

After that, that object can be used to access the different elements of
the results, for example:

``` r
exRes <- PM_load(1)
exRes$final$plot() # see ?plot.PM_final
exRes$op$plot(type = "pop") # see ?plot.PM_op,
exRes$data$plot(
  overlay = F,
  line = list(pred = list(exRes$post, color = "green"), join = F),
  marker = list(symbol = "diamond-open", color = "blue"),
  log = T
) # see ?plot.PM_data
```
