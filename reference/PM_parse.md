# Parse Pmetrics output

**\[experimental\]**

A flexible parser for Pmetrics output

## Usage

``` r
PM_parse(path = ".", fit = "fit.rds", write = TRUE)
```

## Arguments

- path:

  The directory containing the output from the Rust-implementation of
  NPAG

- fit:

  The relative path to a "fit.rds" file, which is normal output after a
  fit containing the data and model used.

- write:

  If `TRUE` (default), saves the output as "PMout.Rdata" in the
  specified path.

## Value

The output of `PM_parse` is a list containing the following elements

- **op** Written to the standard of
  [`PM_op()`](https://lapkb.github.io/Pmetrics_rust/reference/PM_op.md)

- **pop** Written to the standard of
  [`PM_pop()`](https://lapkb.github.io/Pmetrics_rust/reference/PM_pop.md)

- **post** Written to the standard of
  [`PM_post()`](https://lapkb.github.io/Pmetrics_rust/reference/PM_post.md)

- **cycles** Written to the standard of
  [`PM_cycle()`](https://lapkb.github.io/Pmetrics_rust/reference/PM_cycle.md)

- **final** Written to the standard of
  [`PM_final()`](https://lapkb.github.io/Pmetrics_rust/reference/PM_final.md)

- **cov** Written to the standard of
  [`PM_cov()`](https://lapkb.github.io/Pmetrics_rust/reference/PM_cov.md)

## Details

Currently written for the Rust implementation of NPAG

## Author

Michael Neely and Markus Hovd
