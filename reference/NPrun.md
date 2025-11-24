# Run a nonparmetric population model

This function runs a nonparametric population model using the provided
data and model object.

## Usage

``` r
NPrun(data, model, ...)
```

## Arguments

- data:

  A
  [`PM_data()`](https://lapkb.github.io/Pmetrics_rust/reference/PM_data.md)
  object, the character name of a data .csv file, or an appropriate data
  frame that can be coerced into a
  [`PM_data()`](https://lapkb.github.io/Pmetrics_rust/reference/PM_data.md)
  object.

- model:

  A
  [`PM_model()`](https://lapkb.github.io/Pmetrics_rust/reference/PM_model.md)
  object, or a list that can be coerced into one.

- ...:

  Additional arguments passed to the `$fit` method in
  [`PM_model()`](https://lapkb.github.io/Pmetrics_rust/reference/PM_model.md).

## Value

Unlike older versions of Pmetrics, this wrapper function will return a
[`PM_result()`](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
directly.

## Details

This is a wrapper around the current method of fitting models to data in
Pmetrics, available to maintain backwards compatibility with versions of
Pmetrics prior to 3.0.0.

## Author

Michael Neely

## Examples

``` r
if (FALSE) { # \dontrun{
run1 <- NPrun(data = dataEx, model = modEx, cycles = 10)
run1$op$plot()
} # }
```
