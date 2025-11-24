# Simulates all subjects in the data set using the model at the given path.

Simulates all subjects in the data set using the model at the given
path.

## Usage

``` r
simulate_all(data_path, model_path, theta, kind)
```

## Arguments

- data_path:

  Path to the data file.

- model_path:

  Path to the compiled model file.

- theta:

  Data frame of support points.

- kind:

  Kind of model, which can either be "ODE" or "Analytical".

## Value

Simulation results.
