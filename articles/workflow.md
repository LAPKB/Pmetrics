# Workflow

## General Workflow

The general Pmetrics workflow is shown in the following diagram.

``` mermaid
flowchart LR

subgraph RSession[" "]
  direction TB
  DATA["PM_data"]
  MODEL["PM_model"]
  RESULT["PM_result"]
end

DISK[("Hard Drive")]

DATA --> MODEL
MODEL -- "$fit()" --> RESULT
RESULT -- "$update()" --> MODEL
DISK -- "PM_load()" --> RESULT
RESULT --> DISK

classDef blue fill:#2f6db6,stroke:#184a8b,color:#fff;
classDef orange fill:#c7662b,stroke:#8a3f18,color:#fff;
classDef disk fill:#d2d3d7,stroke:#7f8084,color:#000;

class DATA,MODEL blue;
class RESULT orange;
class DISK disk;

style RSession fill:#e9f0ff,stroke:#9ab0d6,stroke-width:1px,rx:2,ry:2
```

The user creates the data and model objects at the top. These can come
from the hard drive or from within R (dashed arrows). The model file is
created in R using the `PM_model$new()` function and the data file by
`PM_data$new()`. When combined using the `$fit()` method on the compiled
model, the analysis is executed. At the end of the run, the hard drive
will contain a new numerically named folder, e.g., 1, 2, 3, …, that
contains the files which can be loaded into R subsequently using
`PM_load(x)`, replacing `x` with the folder number.
[`PM_load()`](https://lapkb.github.io/Pmetrics_rust/reference/PM_load.md)
is an alias for `PM_result$new()` because it creates a new
[`PM_result()`](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
object which contains all the results of a run, and has many assciated
methods attached to it for plotting, simulating, etc.

To change model parameters, the model object is updated via
`PM_model$update()`. If continuing a previous run that did not end,
simply use `$fit()` and specify the run number you wish to continue as
the `prior` argument to `$fit()`. These scenarios are illustrated below.

``` r
# Run 1 - ensure the data, model files are in the working directory
data1 <- PM_data$new("data.csv")
mod1 <- PM_model$new("model.txt")
res1 <- mod1$fit(data1) # this is a PM_result object

res1 <- PM_load(1) # if returning to script later, use this to load from hard drive

# Run 2 - update Ke range
mod2 <- mod1$clone # create an independent copy
mod2$update(list(pri = list(Ke = ab(0.5, 3))))
run2 <- mod2$fit(data1)


# Run 3 - continue run 2
run3 <- mod2$fit(data = data1, prior = 2)
```

The great advantage of R6 over Legacy is that in R6, you no longer need
to spend time copying files from prior run folders, modifying them, and
ensuring that they are in the working directory. After the initial
creation of the data and model objects, everything can be done in R from
memory, although results are still saved to hard drive for later
retrieval.
