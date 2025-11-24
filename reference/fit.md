# Fits the model at the given path to the data at the given path using the provided parameters.

Fits the model at the given path to the data at the given path using the
provided parameters.

## Usage

``` r
fit(model_path, data, params, output_path, kind)
```

## Arguments

- model_path:

  Path to the compiled model file.

- data:

  Path to the data file.

- params:

  List of fitting parameters.

- output_path:

  Path to save the fitting results.

- kind:

  Kind of model, which can either be "ODE" or "Analytical".

## Value

Result of the fitting process.
