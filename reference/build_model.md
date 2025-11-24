# Launch Model Builder app

Open the shiny model builder app.

## Usage

``` r
build_model(..., update = FALSE)
```

## Arguments

- ...:

  Optional
  [PM_data](https://lapkb.github.io/Pmetrics_rust/reference/PM_data.md)
  and/or
  [PM_model](https://lapkb.github.io/Pmetrics_rust/reference/PM_model.md)
  object(s). *PM_data* objects supply covariates. *PM_model* objects
  supply any other defined model element, and covariates only if there
  is no *PM_data* object or it has no covariates. If the *PM_model*
  object contains covariates, they will be superseded by those in the
  *PM_data* object, if supplied.

- update:

  Logical. If `TRUE`, the `Save` button in the app will update the
  model, rather than write the model to a file.

## Value

Launches the shiny app.

## Details

The app will open in a separate window.

## Author

Michael Neely
