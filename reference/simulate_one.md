# Simulates the first subject in the data set using the model at the given path.

Simulates the first subject in the data set using the model at the given
path.

## Usage

``` r
simulate_one(data_path, model_path, spp, kind)
```

## Arguments

- data_path:

  Path to the data file.

- model_path:

  Path to the compiled model file.

- spp:

  One support point as a numeric vector with probabiltity.

- kind:

  Kind of model, which can either be "ODE" or "Analytical".

## Value

Simulation results.
