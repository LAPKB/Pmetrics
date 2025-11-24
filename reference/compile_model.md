# Compiles the text representation of a model into a binary file.

Compiles the text representation of a model into a binary file.

## Usage

``` r
compile_model(model_path, output_path, params, template_path, kind)
```

## Arguments

- model_path:

  Path to the model file.

- output_path:

  Path to save the compiled model.

- params:

  List of model parameters.

- template_path:

  Path to the template directory.

- kind:

  Kind of model, which can either be "ODE" or "Analytical".

## Value

Result of the compilation process.
