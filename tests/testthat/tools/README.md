# Test tooling

## regenerate-test-inventory.R

Regenerates a CSV inventory of `test_that()` calls found under `tests/testthat`.

Output file:

- `tests/testthat/test-inventory.csv`

### Usage from shell

Run from the package root:

```sh
Rscript tests/testthat/tools/regenerate-test-inventory.R
```

### Usage from R

```r
source("tests/testthat/tools/regenerate-test-inventory.R")
inventory <- generate_test_inventory()
inventory
```

### Optional arguments

```r
source("tests/testthat/tools/regenerate-test-inventory.R")
inventory <- generate_test_inventory(
  pkg_root = "/path/to/Pmetrics",
  tests_path = file.path("tests", "testthat"),
  output_path = file.path("tests", "testthat", "test-inventory.csv"),
  write_csv = TRUE
)
inventory
```
